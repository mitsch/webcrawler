module Main where

import Prelude hiding (log, max)
import System.Console.GetOpt
import System.Environment
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Client.TLS
import Network.URI
import Network.Stream()
import System.IO
import System.Posix.Signals()
import System.Exit
import System.Time
import System.Random
import Control.Monad
import Control.Monad.Trans.Class
import Control.Concurrent
import Control.Exception hiding (handle)
import Control.Concurrent.Chan()
import Control.Concurrent.MVar()
import Data.Maybe
import Data.Set (Set, insert, member, fromList)
import Data.List
import Data.Either
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Parser
import Robots
import Ring



-- splits on every element whose prediction p returns True
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn p x = let (a,b) = break p x in a:(splitOn p $ drop 1 b)

-- writes temporary file which is not deleted at the end
writeTempLivingFile :: FilePath -> String -> L.ByteString -> IO String
writeTempLivingFile folder fileNamePattern content = do
	(filePath, handle) <- openTempFile folder fileNamePattern
	L.hPutStr handle content
	hClose handle
	return filePath

isSameDomain :: URI -> URI -> Bool
isSameDomain a b = fromMaybe False $ do
	let getDomainStack = dropWhile null . reverse . splitOn ('.'==)
	aRegName <- uriAuthority a >>= return . getDomainStack . uriRegName
	bRegName <- uriAuthority b >>= return . getDomainStack . uriRegName
	return $ length aRegName == length bRegName && (and $ zipWith (==) aRegName bRegName)


getSuffix :: URI -> Maybe String
getSuffix = maybe Nothing (listToMaybe . reverse . drop 1 . splitOn ('.'==)) . listToMaybe . reverse . splitOn ('/'==) . uriPath


switchMaybe :: a -> Maybe b -> Maybe a
switchMaybe _ (Just _) = Nothing
switchMaybe x Nothing = Just x

mapM'_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM'_ f = g
	where g (x:xs) = f x >>= (\y -> y `seq` (g xs))
	      g [] = return ()

data MyOutput = StdOut String | StdErr String | StdOutFlush

-- puts to standard output
put :: OutputChan MyOutput -> String -> IO ()
put c m = writeOutputChan c $ StdOut m

-- puts list to standard output
putList :: OutputChan MyOutput -> [String] -> IO ()
putList c ms = writeListOutputChan c $ map StdOut ms

-- logs to standard error
log :: OutputChan MyOutput -> String -> IO ()
log c m = do
	time <- getClockTime >>= toCalendarTime
	let ctTZTime = ctTZ time
	let timeT = (show $ ctYear time) ++ "-" ++ (show $ ctMonth time) ++ "-" ++ (show $ ctDay time) ++ "T" ++
	            (show $ ctHour time) ++ ":" ++ (show $ ctMin time) ++ ":" ++ (show $ ctSec time) ++ "+" ++
	            (show $ ctTZTime `div` 3600) ++ ":" ++ (show $ ((ctTZTime `div` 60) `mod` 60))
	writeOutputChan c $ StdErr $ timeT ++ "\t" ++ m

-- logs list to standard error
logList :: OutputChan MyOutput -> [String] -> IO ()
logList c ms = do
	time <- getClockTime >>= toCalendarTime
	let ctTZTime = ctTZ time
	let timeT = (show $ ctYear time) ++ "-" ++ (show $ ctMonth time) ++ "-" ++ (show $ ctDay time) ++ "T" ++
	            (show $ ctHour time) ++ ":" ++ (show $ ctMin time) ++ ":" ++ (show $ ctSec time) ++ "+" ++
							(show $ ctTZTime `div` 3600) ++ ":" ++ (show $ ((ctTZTime `div` 60) `mod` 60))
	writeListOutputChan c $ map (\m -> StdErr $ timeT ++ "\t" ++ m) ms

flushOutput :: OutputChan MyOutput -> IO ()
flushOutput c = writeOutputChan c StdOutFlush

-- download settings for web page download
data DownloadSettings = DownloadSettings {
	downloadMaximumSleepTime :: Maybe Int,
	downloadMaximumTimeOut :: Maybe Int,
	downloadUserAgent :: String}

-- downloader routine
downloader :: DownloadSettings -> RingChan URI -> RingChan (URI, Response L.ByteString) -> OutputChan MyOutput -> IO ()
downloader settings urlChannel responseChannel outputChannel = do
	clockTime <- getClockTime
	setStdGen $ mkStdGen $ ctMin $ toUTCTime clockTime
	manager <- newManager $ tlsManagerSettings {managerResponseTimeout = downloadMaximumTimeOut settings}
	runRing urlChannel responseChannel $ \u -> do
		case downloadMaximumSleepTime settings of
			Nothing -> return ()
			Just t' -> lift $ randomRIO (0, t') >>= threadDelay
		initReq <- lift $ parseUrl $ show u
		let req = initReq { method = methodGet,
		                    requestHeaders = [(hUserAgent, S.pack $ downloadUserAgent settings)],
		                    redirectCount = 0,
		                    checkStatus = \_ _ _ -> Nothing }
		lift $ log outputChannel $ "downloading " ++ (show u)
		result <- lift $ try $ httpLbs req manager
		lift $ result `seq` (log outputChannel $ "downloaded " ++ (show u))
		case result of
			Left err -> lift $ log outputChannel $ "network error on " ++ (show u) ++ " with " ++ (show (err :: SomeException))
			Right resp -> forward (u, resp)
	closeManager manager
	


data ParserSettings = ParserSettings {
	parserLoopRestrictions :: [(URI -> URI -> Bool)],
	parserOutputs :: [ParserOutput],
	parserFileCaching :: Maybe (String, String)}

-- parser routine
parser :: ParserSettings -> RingChan (URI, Response L.ByteString) -> RingChan Track -> OutputChan MyOutput -> IO ()
parser settings responseChannel trackChannel outputChannel = runRing responseChannel trackChannel $ \(u, r) -> do
	let convertURI s = parseURIReference s >>= \l -> return $ if uriIsRelative l then l `relativeTo` u else l
	let headers = responseHeaders r
	let contentType = lookup hContentType headers >>= return . S.unpack
	let location = lookup hLocation headers >>= convertURI . S.unpack
	let body = responseBody r
	let status = responseStatus r
	let code = statusCode status
	let isCorrectContentType = maybe True (\t -> null t || "text/html" `isPrefixOf` t) contentType
	let doLooping l = all (\f -> f u l) $ parserLoopRestrictions settings
	if statusIsRedirection status
	then case location of
	     	Nothing -> lift $ log outputChannel $ "http error on " ++ show u ++ " with code " ++ show code ++
				                                      ": despite redirection no location given";
				Just l -> do
					when (doLooping l) (forward $ NewSeed l)
					lift $ put outputChannel $ intercalate "\t" ["redirection", show u, show l, show code, fromMaybe "" $ getSuffix l]
					lift $ put outputChannel $ intercalate "\t" ["visit", show u, fromMaybe "" $ getSuffix u, fromMaybe "" contentType]
					lift $ log outputChannel $ "parsed redirection " ++ show u
	else if code == 200
	then do
		fileName <- case parserFileCaching settings of
			Nothing -> return Nothing
			Just (d, n) -> do
				(filePath, handle) <- lift $ openTempFile d n
				lift $ L.hPutStr handle body
				lift $ hClose handle
				return (Just filePath)
		if isCorrectContentType
		then do
			let refs = parse (parserOutputs settings) u body
		 	forwardList $ map NewSeed $ filter doLooping $ map fst refs
		 	lift $ putList outputChannel $ map (\(l, t) -> intercalate "\t" $ ["reference", show u, show l] ++ (map L.unpack t)) refs
		 	lift $ put outputChannel $ intercalate "\t" $ ["visit", show u] ++ (maybe [] return fileName) ++
			                                              [fromMaybe "" $ getSuffix u, fromMaybe "" contentType]
			lift $ log outputChannel $ "parsed html document " ++ show u
		else do
			case (getSuffix u) of
		 		Nothing -> return ();
		 		Just s -> forward $ MarkInvalidType s
			lift $ put outputChannel $ intercalate "\t" $ ["invalid type", show u] ++ (maybe [] return fileName) ++
			                                              [fromMaybe "" $ getSuffix u, fromMaybe "" contentType]
			lift $ log outputChannel $ "invalid document on " ++ show u ++ " with type \"" ++ fromMaybe "" contentType ++ "\"" ++
			                           " and suffix \"" ++ fromMaybe "" (getSuffix u) ++ "\""
	else lift $ log outputChannel $ "http error on " ++ show u ++ " with code " ++ show code ++ ":" ++ S.unpack (statusMessage status)
	lift $ flushOutput outputChannel


-- track objects
data Track = NewSeed URI | MarkInvalidType String

-- keeps track of visited web pages, invalid type suffixes and robots texts
tracker :: Set String -> Set String -> Set URI -> String -> RingChan Track -> RingChan URI -> OutputChan MyOutput -> IO ()
tracker goodSuffixes badSuffixes history agent trackChannel urlChannel outputChannel
	= void $ runRingS trackChannel urlChannel (badSuffixes, history, []) $ \ (s, h, r) t -> do
	  case t of
	  	MarkInvalidType o -> do
				if member o goodSuffixes
				then do
					lift $ log outputChannel $ "suffix \"" ++ o ++ "\" indicates invalid type but is part of good suffixes!"
					return (s, h, r)
				else do
		  		lift $ log outputChannel $ "add suffix \"" ++ o ++ "\" to invalid type suffixes"
		  		return (Data.Set.insert o s, h, r)
	  	NewSeed u -> do
	  		let suffix = getSuffix u
	  		if member u h
	  		then return (s, h, r)
	  		else if fromMaybe False $ fmap (flip member s) suffix
	  		     then do
						 	lift $ log outputChannel $ "url \"" ++ show u ++ "\" contains suffix \"" ++ fromMaybe "" suffix ++
							                           "\" indicating invalid type document"
						 	return (s, h, r)
	  		     else do
	  		     	let maybeDomain = uriAuthority u >>= return . uriRegName
	  		     	case maybeDomain of
	  		     		Nothing -> do
	  		     			lift $ log outputChannel $ "url \"" ++ show u ++ "\" is missing a domain specification and will be discarded!"
	  		     			return (s, h, r)
	  		     		Just domain -> do
	  		     			(specRobots, newAllRobots) <- case lookup domain r of
	  		     				Just robots -> return (robots, r)
	  		     				Nothing -> do
	  		     					lift $ log outputChannel $ "miss robots for domain " ++ domain
	  		     					let v = u {uriPath = "/robots.txt", uriQuery = "", uriFragment = ""}
	  		     					lift $ log outputChannel $ "downloading robots from " ++ show v
	  		     					initReq <- lift $ parseUrl $ show v
	  		     					let req = initReq {method = methodGet, redirectCount = 10}
	  		     					manager <- lift $ newManager $ tlsManagerSettings {managerResponseTimeout = Just 10000000}
	  		     					result <- lift $ try $ httpLbs req manager
	  		     					case result of
	  		     						Left err -> do
	  		     							lift $ log outputChannel $ "network error on " ++ show v ++ ":" ++ (show (err :: SomeException))
	  		     							return (makeEmptyRobots, (domain, makeEmptyRobots) : r)
	  		     						Right resp -> do
	  		     							lift $ log outputChannel $ "downloaded robots from " ++ show v
	  		     							let newRobots = makeRobots agent $ L.unpack $ responseBody resp
	  		     							return (newRobots, (domain, newRobots) : r)
	  		     			if isRobotsConform specRobots u
	  		     			then forward u >> return (s, Data.Set.insert u h, newAllRobots)
	  		     			else do
	  		     				lift $ log outputChannel $ "robots discards " ++ (show u)
	  		     				return (s, h, newAllRobots)


data Flag = Help | LoopRestriction (URI -> URI -> Bool) | UserAgent String | MaxTimeOut Int | MaxWaitingTime Int |
            BadSuffixes FilePath | Histories FilePath | BadSuffix String | History String | SyncOutput |
						Pattern ParserOutput | FileCaching String | ReadFromStdInput | GoodSuffixes FilePath | GoodSuffix String

options :: [OptDescr Flag]
options = [
	Option "" ["help"] (NoArg Help) "shows this text",
	Option "" ["no-looping"] (NoArg $ LoopRestriction $ \_ _ -> False) "crawl only web pages given as seeds",
	Option "" ["stay-in-domain"] (NoArg $ LoopRestriction isSameDomain) "put only web pages in loop which where reached from same domain",
	Option "u" ["user-agent"] (ReqArg (\s -> UserAgent s) "STRING")
		"STRING is used as user agent identification and should contain the word bot as well as some conctact address for emergency cases",
	Option "t" ["timeout"] (ReqArg (\s -> MaxTimeOut $ read s) "MICROSECONDS")
		"wait at most MICROSECONDS for every download request",
	Option "ws" ["waiting-time", "sleep-time"] (ReqArg (\s -> MaxWaitingTime $ read s) "MICROSECONDS")
		"wait between zero and MICROSECONDS before starting the next download request",
	Option "" ["bad-suffixes"] (ReqArg (\s -> BadSuffixes s) "FILE") "FILE contains list of suffixes which indicate no html resources",
	Option "" ["good-suffixes"] (ReqArg (\s -> GoodSuffixes s) "FILE") "FILE contains list of suffixes which indicates html resources",
	Option "" ["histories"] (ReqArg (\s -> Histories s) "FILE") "FILE contains urls which were already visited",
	Option "b" ["bad-suffix"] (ReqArg (\s -> BadSuffix s) "SUFFIX") "SUFFIX indicates no html resource",
	Option "g" ["good-suffix"] (ReqArg (\s -> GoodSuffix s) "SUFFIX") "SUFFIX indicates html resource",
	Option "h" ["history"] (ReqArg (\s -> History s) "URL") "URL is considered to be already visited",
	Option "" ["sync-output"] (NoArg SyncOutput) "synchronises output and log messages",
	Option "" ["print-precontext"] (OptArg (\x -> Pattern $ PreContext (fmap read x) (switchMaybe "div" x)) "NUMBER")
		"prints NUMBERs words of text context before the reference; if called without a number it is limited to div tag areas",
	Option "" ["print-postcontext"] (OptArg (\x -> Pattern $ PostContext (fmap read x) (switchMaybe "div" x)) "NUMBER")
		"prints NUMBERs words of text context after the reference; if called without a number it is limited to div tag areas",
	Option "" ["print-text"] (NoArg $ Pattern ReferenceText) "prints text of reference",
	Option "" ["print-attribute"] (ReqArg (Pattern . ReferenceAttribute) "NAME") "prints value of attribute NAME of the reference tag",
	Option "" ["print-embedded-img-attribute"] (ReqArg (Pattern . EmbeddedImageAttribute) "NAME")
		"prints value of attribute NAME of any embedded image tag in the reference area",
	Option "" ["print-name"] (NoArg $ Pattern ReferenceName) "prints name of the reference tag",
	Option "" ["print-ref-suffix"] (NoArg $ Pattern ReferenceSuffix) "prints suffix of reference",
	Option "" ["cache-file"] (ReqArg FileCaching "FILE") "caches crawled resources to some files according to the template FILE",
	Option "" ["read-input"] (NoArg ReadFromStdInput) "reads urls to crawl also from standard input"]


main :: IO ()
main = do
	(originSeeds, loopRestrictions, userAgent, timeOut, sleepTime, badSuffixes, goodSuffixes, history,
		patterns, fileCaching, readStdInput) <- getArgs >>= (\args ->
		case getOpt Permute options args of
			(o,n,[]) -> do
				let needHelp = any (\f -> case f of {Help->True;_->False}) o
				when needHelp $ do
					putStr $ usageInfo "usage: crawler [OPTION ...] urls ..." options
					exitSuccess
				let (invalidSeeds, validSeeds) = partitionEithers $ map (\t -> maybe (Left t) Right $ parseAbsoluteURI t) n
				mapM_ (\s -> hPutStrLn stderr $ "invalid absolute uri \"" ++ s ++ "\" will be ignored!") invalidSeeds
				let loopRestrictions = [\_ _ -> True] ++ mapMaybe (\f -> case f of {(LoopRestriction p)->Just p;_->Nothing}) o
				let userAgent = maybe "webcrawler bot" id $ listToMaybe $ mapMaybe (\f -> case f of {(UserAgent s)->Just s;_->Nothing}) o
				let timeOut = listToMaybe $ mapMaybe (\f->case f of {(MaxTimeOut z)->Just z;_->Nothing}) o
				let sleepTime = listToMaybe $ mapMaybe (\f->case f of {(MaxWaitingTime z)->Just z;_->Nothing}) o
				let badSuffixes = mapMaybe (\f->case f of {(BadSuffix s)->Just s;_->Nothing}) o
				let badSuffixFiles = mapMaybe (\f -> case f of {(BadSuffixes s)->Just s; _->Nothing}) o
				let goodSuffixes = mapMaybe (\f -> case f of {(GoodSuffix s) -> Just s; _ -> Nothing}) o
				let goodSuffixFiles = mapMaybe (\f -> case f of {(GoodSuffixes s) -> Just s; _ -> Nothing}) o
				badSuffixFilesContents <- mapM readFile badSuffixFiles >>= return . concat . map lines
				goodSuffixFilesContents <- mapM readFile goodSuffixFiles >>= return . concat . map lines
				let (invalidHistories, validHistories) = partitionEithers $ map (\t -> maybe (Left t) Right $ parseAbsoluteURI t) $
					mapMaybe (\f -> case f of {(History s) -> Just s; _ -> Nothing}) o
				let historyFiles = mapMaybe (\f -> case f of {(Histories s) -> Just s; _ -> Nothing}) o
				historyFilesContentsLines <- mapM readFile historyFiles >>= return . concatMap lines
				let historyFilesContents = map (\t -> maybe (Left t) Right $ parseAbsoluteURI t)  historyFilesContentsLines
				let (invalidHFC, validHFC) = partitionEithers historyFilesContents
				mapM_ (\s -> hPutStrLn stderr $ "invalid absolute uri \"" ++ s ++ "\" in history will be ignored!") $
					invalidHistories ++ invalidHFC
				let patterns = mapMaybe (\f -> case f of {(Pattern p)->Just p;_->Nothing}) o
				let splitFilePattern = (\(a,b) -> (reverse b, reverse a)) . span ('/'/=) . reverse
				let fileCaching = (listToMaybe $ mapMaybe (\f -> case f of {(FileCaching z)->Just z;_->Nothing}) o) >>=
				                  	(return . splitFilePattern)
				let readFromInput = any (\f -> case f of {ReadFromStdInput->True;_->False}) o
				return (validSeeds, loopRestrictions, userAgent, timeOut, sleepTime,
				        fromList $ badSuffixes ++ badSuffixFilesContents,
								fromList $ goodSuffixes ++ goodSuffixFilesContents,
				        fromList $ validHistories ++ validHFC, patterns, fileCaching, readFromInput)
			(_,_,errs) -> ioError (userError (concat errs ++ usageInfo "usage: crawler [OPTION ...] urls..." options)))
	seedChannel <- newChan
	responseChannel <- newChan
	outputChannel <- newChan
	trackChannel <- newChan
	-- download thread
	void $ forkIO $ do
		log outputChannel "download thread started"
		let downloadSettings = DownloadSettings{
				downloadMaximumSleepTime = sleepTime,
				downloadMaximumTimeOut = timeOut,
				downloadUserAgent = userAgent}
		downloadResult <- try $ downloader downloadSettings seedChannel responseChannel outputChannel
		case downloadResult of
			Left e -> error $ "download thread: exception " ++ show (e :: SomeException)
			Right _ -> log outputChannel "download thread stopped"
		closeOutputChan outputChannel
	-- parser thread
	void $ forkIO $ do
		log outputChannel "parser thread started"
		let parserSettings = ParserSettings {
			parserFileCaching = fileCaching,
			parserOutputs = patterns,
			parserLoopRestrictions = loopRestrictions}
		parserResult <- try $ parser parserSettings responseChannel trackChannel outputChannel
		case parserResult of
			Left e -> error $ "parser thread: exception " ++ show (e :: SomeException)
			Right _ -> log outputChannel "parser thread stopped"
		closeOutputChan outputChannel
	-- tracker thread
	void $ forkIO $ do
		log outputChannel "tracker thread started"
		trackerResult <- try $ tracker goodSuffixes badSuffixes history userAgent trackChannel seedChannel outputChannel
		case trackerResult of
			Left e -> error $ "tracker thread: exception " ++ show (e :: SomeException)
			Right _ -> log outputChannel "tracker thread stopped"
		closeOutputChan outputChannel
	-- stream into track channel
	writeListRingChan trackChannel $ map NewSeed originSeeds
	-- input thread
	when readStdInput $ void $ forkIO $ do
		getContents >>= return . map (\t -> maybe (Left t) Right $ parseAbsoluteURI t) . lines >>= mapM_ (\t -> case t of
			Left s -> log outputChannel $ "invalid absolute uri \"" ++ s ++ "\" will be ignored!"
			Right u -> writeRingChan trackChannel $ NewSeed u)
		closeOutputChan outputChannel
		signalLastRingChan trackChannel
	-- in case we dont read from standard input we have to give here
	-- an exta signal for the situation that no new url is coming in
	-- into the ring pipe from the outside
	when (not readStdInput) (signalLastRingChan trackChannel)
	-- output thread is on main thread since it indirectly synchronises all threads
	runOutput outputChannel (3 + (if readStdInput then 1 else 0)) $ \t -> case t of
		StdOut m -> hPutStrLn stdout m;
		StdErr m -> hPutStrLn stderr m >> hFlush stderr;
		StdOutFlush -> hFlush stdout
