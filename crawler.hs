module Main where

import Text.HTML.TagSoup 
import System.Console.GetOpt
import System.Environment
import Network.HTTP
import Network.URI
import Network.Stream
import System.IO
import System.Posix.Signals
import System.Exit
import System.Time
import System.Random
import System.Timeout
import Control.Monad
import Control.Concurrent
import Control.Exception
import Control.Concurrent.Chan
import Control.Concurrent.QSemN
import Data.Maybe
import Data.Set (Set, insert, member, fromList)
import Data.List
import Data.Either

-- splits on every element whose prediction p returns True
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn p x = let (a,b) = break p x in a:(splitOn p $ drop 1 b)


getSuffix :: URI -> Maybe String
getSuffix = maybe mzero (listToMaybe . reverse . drop 1 . splitOn ('.'==)) . listToMaybe . reverse . splitOn ('/'==) . uriPath

showTimeStamp :: ClockTime -> String
showTimeStamp c = let b = toUTCTime c in (show $ ctYear b) ++ "-" ++ (show $ ctMonth b) ++ "-" ++ (show $ ctDay b) ++ "-" ++ (show $ ctHour b) ++ "-" ++ (show $ ctMin b) ++ "-" ++ (show $ ctSec b)


mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ [] = return []
mapM' f (a:as) = (f a) >>= (\y -> y `seq` mapM' f as)

mapM'_ :: Monad m => (a -> m ()) -> [a] -> m ()
mapM'_ _ [] = return ()
mapM'_ f (a:as) = (f a) >>= (\y -> y `seq` mapM'_ f as)



downloader ::Maybe Int -> Maybe Int -> String -> Chan URI -> Chan (URI, Response String) -> Chan String -> IO ()
downloader maxWaitTime timeOut agent urlChan responseChan logChan = do
	urls <- getChanContents urlChan
	clockTime <- getClockTime
	let randomNumbers n = randomRs (0, n) $ mkStdGen $ ctMin $ toUTCTime clockTime
	let waitingTime = maybe (repeat 0) randomNumbers maxWaitTime
	return (zip urls waitingTime) >>= mapM_ (\(u, w) -> do
		when (isJust maxWaitTime) (threadDelay w)
		let tryTimeOut = maybe (liftM Just) timeout timeOut
		result <- try $ tryTimeOut $ simpleHTTP $ insertHeader HdrUserAgent agent $ getRequest $ show u
		case result of
			Left err -> writeChan logChan ("network error\t" ++ (show u) ++ "\t" ++ (show (err :: SomeException)))
			Right Nothing -> writeChan logChan ("timeout\t" ++ (show u)) >> writeChan urlChan u
			Right (Just (Left err)) -> writeChan logChan ("network error\t" ++ (show u) ++ "\t" ++ (show (err :: ConnError)))
			Right (Just (Right resp)) -> writeChan logChan ("download\t" ++ (show u)) >> writeChan responseChan (u, resp))




data ParseResult = HTTPError (Int, Int, Int) String | Redirection URI | InvalidType String | ParsedReferences [(URI, [String])]

parse :: [(URI, Response String)] -> [(URI, ParseResult)]
parse = map $ \(u, r) ->
	let headers = rspHeaders r;
	    contentType = lookupHeader HdrContentType headers;
	    location = lookupHeader HdrLocation headers >>= parseURIReference;
	    body = rspBody r;
	    code = rspCode r;
	    code2text (a,b,c) = show (a * 100 + b * 10 + c);
	    eliminateData s
	    	| null s = []
	    	| "\"data:" `isPrefixOf` s = "\"" ++ (eliminateData $ dropWhile ('\"'/=) $ drop 6 s)
	    	| "\'data:" `isPrefixOf` s = "\'" ++ (eliminateData $ dropWhile ('\''/=) $ drop 6 s)
	    	| otherwise = [head s] ++ (eliminateData $ tail s);
	    fromAnchorTag t = [fromAttrib "hreflang" t, fromAttrib "lang" t, fromAttrib "title" t];
	    fromTextTags = map fromTagText . filter isTagText;
	    fromImgTags = map (fromAttrib "alt") . filter (isTagOpenName "img");
	    fromAnchor ts = let h = head ts; hs = takeWhile (~/= TagClose "a") ts
	                    in (fromAttrib "href" h, fromAnchorTag h ++ fromTextTags hs ++ fromImgTags hs);
	    getAnchors = map fromAnchor . partitions (isTagOpenName "a");
	    getLinks = map (\t -> (fromAttrib "href" t, [fromAttrib "hreflang" t])) . filter (~== "<link rel=alternate>");
	    canonicalizeURI v = URI (uriScheme v) (uriAuthority v) (if null $ uriPath v then "/" else uriPath v) (uriQuery v) "";
	    convertURI s = parseURIReference s >>= return . canonicalizeURI . (\z -> maybe (relativeTo z u) (const z) (uriAuthority z));
	    getReferences ts = mapMaybe (\(v, t) -> maybe Nothing (\z -> Just (z, t)) $ convertURI v) $ getAnchors ts ++ getLinks ts;
	    isCorrectContentType = maybe True (\t -> null t || "text/html" `isPrefixOf` t) contentType
	in if isCorrectContentType
		then case code of 
			(3,_,_) -> (u, maybe (HTTPError code "despite redirection no location given") Redirection location)
			(2,0,0) -> (u, ParsedReferences $ getReferences $ canonicalizeTags $ parseTags $ eliminateData body)
			_ -> (u, HTTPError code $ "code " ++ (code2text code))
		else (u, InvalidType $ maybe "" id contentType)

parser :: Bool -> Chan (URI, Response String) -> Chan String -> Chan String -> Chan Track -> IO ()
parser loopBack respChan outputChan logChan trackChan =
	let code2text (a,b,c) = show $ a * 100 + b * 10 + c;
	    reportPageVisit u = writeChan logChan $ "visit\t" ++ (show u);
	    reportInvalidType u m = writeChan logChan $ "invalid type\t" ++ (show u) ++ "\t" ++ m;
	    markPageVisit u = writeChan outputChan $ "visit\t" ++ (show u);
	    markReferences h = writeList2Chan outputChan . map (\(l, t) -> "reference\t"++(show h) ++"\t" ++ (show l) ++ "\t" ++ (unwords t));
	    markRedirection h l = writeChan outputChan $ "redirection\t" ++ (show h) ++ "\t" ++ (show l);
	    pushi = writeChan trackChan . MarkInvalidType;
	    push = writeChan trackChan . NewSeed;
	    pushs = writeList2Chan trackChan . map (NewSeed . fst);
	    handle (u, HTTPError c reason) = writeChan logChan $ "http error\t" ++ (show u) ++ "\t" ++ (code2text c) ++ "\t" ++ reason;
	    handle (u, Redirection loc) = markRedirection u loc >> markPageVisit u >> reportPageVisit u >> when loopBack (push loc);
	    handle (u, InvalidType mime) = reportInvalidType u mime >> maybe mzero pushi (getSuffix u);
	    handle (u, ParsedReferences refs) = markReferences u refs >> markPageVisit u >> reportPageVisit u >> when loopBack (pushs refs);
	in getChanContents respChan >>= return . parse >>= mapM'_ handle




data Track = NewSeed URI | MarkInvalidType String

tracker :: Set String -> Set URI -> Chan Track -> Chan URI -> IO ()
tracker badSuffixes history trackChan urlChan = 
	let handle :: (Set String, Set URI) -> Track -> IO (Set String, Set URI)
	    handle (b, h) (NewSeed u) = if member u h
			                            	then return (b, h)
	                                	else if maybe False (flip member b) $ getSuffix u
										                     	then return (b, h)
	    				                           	else writeChan urlChan u >> return (b, Data.Set.insert u h)
	    handle (b, h) (MarkInvalidType suffix) = return (Data.Set.insert suffix b, h)
	in getChanContents trackChan >>= foldM_ handle (badSuffixes, history)





data Flag = Help | Looping Bool | UserAgent String | MaxTimeOut Int | MaxWaitingTime Int | Parsers Int | BadSuffixes FilePath |
            Histories FilePath | BadSuffix String | History String | SyncOutput

options :: [OptDescr Flag]
options = [
	Option "" ["help"] (NoArg Help) "shows this text",
	Option "l" ["looping"] (NoArg $ Looping True) "crawl all references which are encountered",
	Option "" ["no-looping"] (NoArg $ Looping False) "crawl only web pages given as seeds",
	Option "u" ["user-agent"] (ReqArg (\s -> UserAgent s) "STRING")
		"STRING is used as user agent identification and should contain the word bot as well as some conctact address for emergency cases",
	Option "t" ["timeout"] (ReqArg (\s -> MaxTimeOut $ read s) "MICROSECONDS")
		"wait at most MICROSECONDS for every download request",
	Option "ws" ["waiting-time", "sleep-time"] (ReqArg (\s -> MaxWaitingTime $ read s) "MICROSECONDS")
		"wait between zero and MICROSECONDS before starting the next download request",
	Option "p" ["parsers"] (ReqArg (\s -> Parsers $ read s) "AMOUNT") "AMOUNT parser threads will be started",
	Option "" ["bad-suffixes"] (ReqArg (\s -> BadSuffixes s) "FILE") "FILE contains list of suffixes which indicate no html resources",
	Option "" ["histories"] (ReqArg (\s -> Histories s) "FILE") "FILE contains urls which were already visited",
	Option "b" ["bad-suffix"] (ReqArg (\s -> BadSuffix s) "SUFFIX") "SUFFIX indicates no html resource",
	Option "h" ["history"] (ReqArg (\s -> History s) "URL") "URL is considered to be already visited",
	Option "" ["sync-output"] (NoArg SyncOutput) "synchronises output and log messages"]


main :: IO ()
main = do
	(originSeeds, doLooping, userAgent, timeOut, sleepTime, parserN, badSuffixes, history, syncOutput) <- getArgs >>= (\args ->
		case getOpt Permute options args of
			(o,n,[]) -> do
				let needHelp = any (\f -> case f of {Help->True;_->False}) o
				when needHelp $ do
					putStr $ usageInfo "usage: crawler [OPTION ...] urls ..." options
					exitSuccess
				let (invalidSeeds, validSeeds) = partitionEithers $ map (\t -> maybe (Left t) Right $ parseAbsoluteURI t) n
				mapM_ (\s -> hPutStrLn stderr $ "invalid absolute uri \"" ++ s ++ "\" will be ignored!") invalidSeeds
				let doLooping = or $ mapMaybe (\f -> case f of {(Looping b)->Just b;_->Nothing}) o
				let userAgent = maybe "webcrawler bot" id $ listToMaybe $ mapMaybe (\f -> case f of {(UserAgent s)->Just s;_->Nothing}) o
				let timeOut = listToMaybe $ mapMaybe (\f->case f of {(MaxTimeOut z)->Just z;_->Nothing}) o
				let sleepTime = listToMaybe $ mapMaybe (\f->case f of {(MaxWaitingTime z)->Just z;_->Nothing}) o
				let parserN = maybe 1 id $ listToMaybe $ mapMaybe (\f->case f of {(Parsers z)->Just z;_->Nothing}) o
				let badSuffixes = mapMaybe (\f->case f of {(BadSuffix s)->Just s;_->Nothing}) o
				let badSuffixFiles = mapMaybe (\f -> case f of {(BadSuffixes s)->Just s; _->Nothing}) o
				badSuffixFilesContents <- mapM readFile badSuffixFiles >>= return . concat . map lines
				let (invalidHistories, validHistories) = partitionEithers $ map (\t -> maybe (Left t) Right $ parseAbsoluteURI t) $
					mapMaybe (\f -> case f of {(History s) -> Just s; _ -> Nothing}) o
				let historyFiles = mapMaybe (\f -> case f of {(Histories s) -> Just s; _ -> Nothing}) o
				historyFilesContentsLines <- mapM readFile historyFiles >>= return . concatMap lines
				let historyFilesContents = map (\t -> maybe (Left t) Right $ parseAbsoluteURI t)  historyFilesContentsLines
				let (invalidHFC, validHFC) = partitionEithers historyFilesContents
				mapM_ (\s -> hPutStrLn stderr $ "invalid absolute uri \"" ++ s ++ "\" in history will be ignored!") $
					invalidHistories ++ invalidHFC
				let syncOutput = any (\f -> case f of {SyncOutput->True;_->False}) o
				return (validSeeds, doLooping, userAgent, timeOut, sleepTime, parserN,
				        fromList $ badSuffixes ++ badSuffixFilesContents,
				        fromList $ validHistories ++ validHFC, syncOutput)
			(_,_,errs) -> ioError (userError (concat errs ++ usageInfo "usage: crawler [OPTION ...] urls..." options)))
	seedChan <- newChan
	logChan <- newChan
	respChan <- newChan
	outputChan <- newChan
	trackChan <- newChan
	finisher <- newQSemN $ 0	
	let completeLogMessage l c = (showTimeStamp c) ++ "\t" ++ l
	logThread <- forkIO (getChanContents logChan >>= mapM_ (\l -> getClockTime >>=
		(if syncOutput then writeChan outputChan else hPutStrLn stderr) . completeLogMessage l) >> signalQSemN finisher 1)
	downloadThread <- forkIO (writeChan logChan "started download thread" >>
		downloader sleepTime timeOut userAgent seedChan respChan logChan >> signalQSemN finisher 1)
--	parserThreads <- replicateM parserN $ forkIO ( writeChan logChan "started parser thread" >>
	parserThreads <- forkIO ( writeChan logChan "started parser thread" >>
		parser doLooping respChan outputChan logChan trackChan >> signalQSemN finisher 1)
	trackerThread <- forkIO ( writeChan logChan "started tracker thread" >>
		tracker badSuffixes history trackChan seedChan >> signalQSemN finisher 1)
	outputThread <- forkIO ( writeChan logChan "started output thread" >>
		getChanContents outputChan >>= mapM_ putStrLn >> signalQSemN finisher 1)
	writeList2Chan seedChan originSeeds
	getContents >>= return . map (\t -> maybe (Left t) Right $ parseAbsoluteURI t) . lines >>= mapM_ (\t ->
		case t of
			Left s -> hPutStrLn stderr $ "invalid absolute uri \"" ++ s ++ "\" will be ignored!"
			Right u -> writeChan seedChan u)
	waitQSemN finisher $ 4 + parserN
