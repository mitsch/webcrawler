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
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Exception hiding (handle)
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent.BoundedChan as B
import Control.Concurrent.MVar()
import Data.Maybe
import Data.Set (Set, insert, member, fromList)
import Data.List
import Data.Either
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser
import Robots
import Ring
import ChanClass


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

data MyOutput = StdOut !T.Text | StdErr !T.Text | StdOutFlush

-- puts to standard output
put :: ChanClass c => c (OutputCarrier MyOutput) -> T.Text -> IO ()
put c m = writeOutputChan c $ StdOut m

-- puts list to standard output
putList :: ChanClass c => c (OutputCarrier MyOutput) -> [T.Text] -> IO ()
putList c ms = writeListOutputChan c $ map StdOut ms

-- logs to standard error
log :: ChanClass c => c (OutputCarrier MyOutput) -> T.Text -> IO ()
log c m = do
	time <- getClockTime >>= toCalendarTime
	let ctTZTime = ctTZ time
	let timeT = (show $ ctYear time) ++ "-" ++ (show $ ctMonth time) ++ "-" ++ (show $ ctDay time) ++ "T" ++
	            (show $ ctHour time) ++ ":" ++ (show $ ctMin time) ++ ":" ++ (show $ ctSec time) ++ "+" ++
	            (show $ ctTZTime `div` 3600) ++ ":" ++ (show $ ((ctTZTime `div` 60) `mod` 60))
	writeOutputChan c $ StdErr $ (T.pack $ timeT ++ "\t") `T.append` m

-- logs list to standard error
logList :: ChanClass c => c (OutputCarrier MyOutput) -> [T.Text] -> IO ()
logList c ms = do
	time <- getClockTime >>= toCalendarTime
	let ctTZTime = ctTZ time
	let timeT = (show $ ctYear time) ++ "-" ++ (show $ ctMonth time) ++ "-" ++ (show $ ctDay time) ++ "T" ++
	            (show $ ctHour time) ++ ":" ++ (show $ ctMin time) ++ ":" ++ (show $ ctSec time) ++ "+" ++
							(show $ ctTZTime `div` 3600) ++ ":" ++ (show $ ((ctTZTime `div` 60) `mod` 60))
	writeListOutputChan c $ map (\m -> StdErr $ (T.pack $ timeT ++ "\t") `T.append` m) ms

-- flushes standard output as soon as this signal has its turn
flushOutput :: ChanClass c => c (OutputCarrier MyOutput) -> IO ()
flushOutput c = writeOutputChan c StdOutFlush

-- logs http error
logHTTPError :: ChanClass c => c (OutputCarrier MyOutput) -> URI -> Int -> T.Text -> IO ()
logHTTPError chan url code message = log chan $ (T.pack $ "http error on " ++ show url ++ " with code " ++ show code ++ ": ") `T.append` message

-- logs network error
logNetworkError :: ChanClass c => c (OutputCarrier MyOutput) -> URI -> T.Text -> IO ()
logNetworkError chan url message = log chan $ (T.pack $ "network error on " ++ show url ++ " with ") `T.append` message

-- puts redirection out
putRedirection :: ChanClass c => c (OutputCarrier MyOutput) -> URI -> URI -> Int -> T.Text -> IO ()
putRedirection chan u l code suffix 
	= put chan $ (T.pack $ intercalate "\t" ["redirection", show u, show l, show code]) `T.append` (T.singleton '\t') `T.append` suffix

-- puts visit out
putVisit :: ChanClass c => c (OutputCarrier MyOutput) -> URI -> T.Text -> T.Text -> IO ()
putVisit chan u fileName suffix = put chan $ T.intercalate (T.singleton '\t') [T.pack "visit", T.pack $ show u, fileName, suffix]

-- download settings for web page download
data DownloadSettings = DownloadSettings {
	downloadMaximumSleepTime :: Maybe Int,
	downloadMaximumTimeOut :: Maybe Int,
	downloadUserAgent :: String,
	downloadBadSuffixes :: [T.Text],
	downloadGoodSuffixes :: [T.Text]}

-- downloader routine
downloader :: (ChanClass c1, ChanClass c2, ChanClass c3, ChanClass c4) => DownloadSettings -> c1 (RingCarrier URI) -> c2 (RingCarrier (URI, Response L.ByteString)) -> c3 (RingCarrier Track) -> c4 (OutputCarrier MyOutput) -> IO ()
downloader settings urlChannel responseChannel trackChannel outputChannel = do
	clockTime <- getClockTime
	setStdGen $ mkStdGen $ ctMin $ toUTCTime clockTime
	manager <- newManager $ tlsManagerSettings {managerResponseTimeout = downloadMaximumTimeOut settings}
	-- handles sleeping
	let sleep = case downloadMaximumSleepTime settings of {Nothing -> return (); Just t' -> lift $ randomRIO (0, t') >>= threadDelay}
	-- the good suffixes should never appear in the bad suffix collection
	let goodSuffixes = downloadGoodSuffixes settings
	-- start the ring
	void $ runRingS urlChannel responseChannel (downloadBadSuffixes settings) $ \ badSuffixes u -> do
		let suffix = getSuffix u >>= return . T.pack
		if fromMaybe True $ fmap (\s -> s `elem` goodSuffixes || s `notElem` badSuffixes) suffix
		then do
			sleep
			initReq <- lift $ parseUrl $ show u
			let req = initReq { method = methodGet,
			                    requestHeaders = [(hUserAgent, S.pack $ downloadUserAgent settings)],
			                    redirectCount = 0,
			                    checkStatus = \_ _ _ -> Nothing }
			lift $ log outputChannel $ T.pack $ "downloading " ++ (show u)
			result <- lift $ try $ runResourceT $ do
				resp <- http req manager
				let headers = responseHeaders resp
				let contentType = lookup hContentType headers >>= return . S.unpack
				if maybe True (\t -> null t || "text/html" `isPrefixOf` t) contentType
				then do
					liftIO $ log outputChannel $ T.pack $ "continue downloading " ++ (show u)
					sinkedResponse <- lbsResponse resp
					liftIO $ log outputChannel $ T.pack $ "downloaded " ++ (show u)
					return (badSuffixes, Just (u, sinkedResponse))
				else do
					liftIO $ put outputChannel $ T.pack $ intercalate "\t" $ ["invalid type", show u, "", fromMaybe "" $ getSuffix u,
						                                                        fromMaybe "" contentType]
					liftIO $ log outputChannel $ T.pack $ "invalid document on " ++ show u ++ " with type \"" ++ fromMaybe "" contentType ++
						                "\"" ++ " and suffix \"" ++ fromMaybe "" (getSuffix u) ++ "\""
					case mfilter (not . flip elem goodSuffixes) suffix of
						Nothing -> return (badSuffixes, Nothing)
						Just s -> do
							liftIO $ writeRingChan trackChannel $ MarkInvalidType s
							return (s : badSuffixes, Nothing)
			case result of
				Left err -> do
					lift $ logNetworkError outputChannel u $ T.pack $ show (err :: SomeException)
					return badSuffixes
				Right (badSuffixes_, maybeForward) -> do
					maybe (return ()) (forward) maybeForward
					return badSuffixes_
		else do
			lift $ log outputChannel $ (T.pack $ "url \"" ++ show u ++ "\" contains suffix \"") `T.append`
			                           (fromMaybe T.empty suffix) `T.append`
			                           (T.pack "\" indicating invalid type document")
			return badSuffixes
	closeManager manager
	


data ParserSettings = ParserSettings {
	parserLoopRestrictions :: [(URI -> URI -> Bool)],
	parserOutputs :: [ParserOutput],
	parserFileCaching :: Maybe (String, String)}

-- parser routine
parser :: (ChanClass c1, ChanClass c2, ChanClass c3) => ParserSettings -> c1 (RingCarrier (URI, Response L.ByteString)) -> c2 (RingCarrier Track) -> c3 (OutputCarrier MyOutput) -> IO ()
parser settings responseChannel trackChannel outputChannel = runRing responseChannel trackChannel $ \(u, r) -> do
	let convertURI s = parseURIReference s >>= \l -> return $ l`relativeTo` u
	let headers = responseHeaders r
	let location = lookup hLocation headers >>= convertURI . S.unpack
	let body = responseBody r
	let status = responseStatus r
	let code = statusCode status
	let doLooping l = all (\f -> f u l) $ parserLoopRestrictions settings
	if statusIsRedirection status
	then case location of
	     	Nothing -> lift $ logHTTPError outputChannel u code $ T.pack "despite redirection no location given";
	     	Just l -> do
	     		when (doLooping l) (forward $ NewSeed l);
	     		lift $ putRedirection outputChannel u l code (maybe T.empty T.pack $ getSuffix l);
	     		lift $ putVisit outputChannel u T.empty (maybe T.empty T.pack $ getSuffix l);
	     		lift $ log outputChannel $ T.pack $ "parsed redirection " ++ show u
	else if code == 200
	then do
		fileName <- case parserFileCaching settings of
			Nothing -> return Nothing
			Just (d, n) -> do
				(filePath, handle) <- lift $ openTempFile d n
				lift $ L.hPutStr handle body
				lift $ hClose handle
				return (Just filePath)
		let refs = parse (parserOutputs settings) u body
		forwardList $ map NewSeed $ filter doLooping $ map fst refs
		lift $ putList outputChannel $ map (\(l, t) -> T.pack $ intercalate "\t" $ ["reference", show u, show l] ++ (map L.unpack t)) refs
		lift $ putVisit outputChannel u (maybe T.empty T.pack fileName) (maybe T.empty  T.pack $ getSuffix u)
		lift $ log outputChannel $ T.pack $ "parsed html document " ++ show u
	else lift $ logHTTPError outputChannel u code $ T.pack $ S.unpack $ statusMessage status
	lift $ flushOutput outputChannel


-- track objects
data Track = NewSeed !URI | MarkInvalidType !T.Text

-- keeps track of visited web pages, invalid type suffixes and robots texts
tracker :: (ChanClass c1, ChanClass c2, ChanClass c3) => [T.Text] -> [T.Text] -> [T.Text] -> String -> c1 (RingCarrier Track) -> c2 (RingCarrier URI) -> c3 (OutputCarrier MyOutput) -> IO ()
tracker goodSuffixes badSuffixes history agent trackChannel urlChannel outputChannel
	= void $ runRingS trackChannel urlChannel (badSuffixes, history, []) $ \ (badSuffixes_, history_, robots_) t -> do
	  	case t of
	  		MarkInvalidType suffix -> do
	  			if suffix `elem` goodSuffixes
	  			then do
	  				lift $ log outputChannel $ (T.pack "suffix \"") `T.append` suffix `T.append` (T.pack "\" is a good suffix!")
	  				return (badSuffixes_, history_, robots_)
	  			else do
	  				lift $ log outputChannel $ (T.pack "add suffix \"") `T.append` suffix `T.append` (T.pack "\" to invalid type suffixes")
	  				return (suffix : badSuffixes_, history_, robots_)
	  		NewSeed u -> do
	  			let suffix = getSuffix u >>= return . T.pack
	  			let uText = T.pack $ show u
	  			if uText `elem` history_ then return (badSuffixes_, history_, robots_)
	  			else if fromMaybe False $ fmap (flip elem badSuffixes_) suffix
	  			     then do
	  			     	lift $ log outputChannel $ (T.pack "url \"") `T.append` (T.pack "\" contains suffix \"") `T.append`
	  			     	                           fromMaybe T.empty suffix `T.append` (T.pack "\" indicating invalid type document")
	  			     	return (badSuffixes_, history_, robots_)
	  			     else case uriAuthority u >>= return . uriRegName of
	  			          	Nothing -> do
	  			          		lift $ log outputChannel $ (T.pack "url \"") `T.append` uText `T.append`
	  			          		                           (T.pack "\" is missing a domain specification and will be discarded!")
	  			          		return (badSuffixes_, history_, robots_)
	  			          	Just domain -> do
	  			          		(specRobots, newAllRobots) <- case lookup domain robots_ of
	  			          			Just robots -> return (robots, robots_)
	  			          			Nothing -> do
	  			          				lift $ log outputChannel $ T.pack $ "miss robots for domain " ++ domain
	  			          				let v = u {uriPath = "/robots.txt", uriQuery = "", uriFragment = ""}
	  			          				lift $ log outputChannel $ T.pack $ "downloading robots from " ++ show v
	  			          				initReq <- lift $ parseUrl $ show v
	  			          				let req = initReq {method = methodGet, redirectCount = 10}
	  			          				manager <- lift $ newManager $ tlsManagerSettings {managerResponseTimeout = Just 10000000}
	  			          				result <- lift $ try $ httpLbs req manager
	  			          				case result of
	  			          					Left err -> do
	  			          						lift $ log outputChannel $ T.pack $ "network error on " ++ show v ++ ":" ++ (show (err :: SomeException))
	  			          						return (makeEmptyRobots, (domain, makeEmptyRobots) : robots_)
	  			          					Right resp -> do
	  			          						lift $ log outputChannel $ T.pack $ "downloaded robots from " ++ show v
	  			          						let newRobots = makeRobots agent $ L.unpack $ responseBody resp
	  			          						return (newRobots, (domain, newRobots) : robots_)
	  			          		if isRobotsConform specRobots u
	  			          		then forward u >> return (badSuffixes_, uText : history_, newAllRobots)
	  			          		else do
	  			          			lift $ log outputChannel $ T.pack $ "robots discards " ++ (show u)
	  			          			return (badSuffixes_, history_, newAllRobots)


data Flag = Help | LoopRestriction (URI -> URI -> Bool) | UserAgent String | MaxTimeOut Int | MaxWaitingTime Int |
            BadSuffixes FilePath | Histories FilePath | BadSuffix String | History String | 
						Pattern ParserOutput | FileCaching String | ReadFromStdInput | GoodSuffixes FilePath | GoodSuffix String |
						ResponseChannelBound Int

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
	Option "" ["read-input"] (NoArg ReadFromStdInput) "reads urls to crawl also from standard input",
	Option "" ["response-channel-bound"] (ReqArg (ResponseChannelBound . read) "MAX") "limit response channel to MAX; default is 5"]


main :: IO ()
main = do
	(originSeeds, loopRestrictions, userAgent, timeOut, sleepTime, badSuffixes, goodSuffixes, history,
		patterns, fileCaching, readStdInput, responseChannelBound) <- getArgs >>= (\args ->
		case getOpt Permute options args of
			(o,n,[]) -> do
				-- help flag anywhere ?
				when (any (\f -> case f of {Help -> True; _ -> False}) o) $ do
					putStr $ usageInfo "usage: crawler [OPTION ...] urls ..." options
					exitSuccess
				-- from the non options consider them as urls to gonna be crawled
				let (invalidSeeds, validSeeds) = partitionEithers $ map (\t -> maybe (Left t) Right $ parseAbsoluteURI t) n
				mapM_ (\s -> hPutStrLn stderr $ "invalid absolute uri \"" ++ s ++ "\" will be ignored!") invalidSeeds
				-- gets restrictions on looping references to the beginning
				-- the first lambda makes looping available if no restriction is given
				let loopRestrictions = [\_ _ -> True] ++ mapMaybe (\f -> case f of {(LoopRestriction p)->Just p;_->Nothing}) o
				-- what is the user agent
				let userAgent = fromMaybe "webcrawler bot" $ listToMaybe $ mapMaybe (\f -> case f of {(UserAgent s) -> Just s; _ -> Nothing}) o
				-- timeout for download request
				let timeOut = listToMaybe $ mapMaybe (\f->case f of {(MaxTimeOut z)->Just z;_->Nothing}) o
				-- maximum time to wait between two download requests
				let sleepTime = listToMaybe $ mapMaybe (\f->case f of {(MaxWaitingTime z)->Just z;_->Nothing}) o
				-- bad suffixes collected from the command line directly as well from any designated file
				let badSuffixesCmdLine = mapMaybe (\f->case f of {(BadSuffix s)->Just s;_->Nothing}) o
				let badSuffixFiles = mapMaybe (\f -> case f of {(BadSuffixes s)->Just s; _->Nothing}) o
				badSuffixFilesContents <- mapM readFile badSuffixFiles >>= return . concat . map lines
				let badSuffixes = map T.pack $ badSuffixesCmdLine ++ badSuffixFilesContents
				-- good suffixes collected from the command line directly as well from any designated file
				let goodSuffixesCmdLine = mapMaybe (\f -> case f of {(GoodSuffix s) -> Just s; _ -> Nothing}) o
				let goodSuffixFiles = mapMaybe (\f -> case f of {(GoodSuffixes s) -> Just s; _ -> Nothing}) o
				goodSuffixFilesContents <- mapM readFile goodSuffixFiles >>= return . concat . map lines
				let goodSuffixes = map T.pack $ goodSuffixesCmdLine ++ goodSuffixFilesContents
				-- build up history from the command line as well from designated files
				let (invalidHistories, validHistories) = partitionEithers $ map (\t -> if isAbsoluteURI t then Right t else Left t) $
					mapMaybe (\f -> case f of {(History s) -> Just s; _ -> Nothing}) o
				let historyFiles = mapMaybe (\f -> case f of {(Histories s) -> Just s; _ -> Nothing}) o
				historyFilesContentsLines <- mapM readFile historyFiles >>= return . concatMap lines
				let historyFilesContents = map (\t -> if isAbsoluteURI t then Right t else Left t)  historyFilesContentsLines
				let (invalidHFC, validHFC) = partitionEithers historyFilesContents
				mapM_ (\s -> hPutStrLn stderr $ "invalid absolute uri \"" ++ s ++ "\" in history will be ignored!") $
					invalidHistories ++ invalidHFC
				let history = map T.pack $ validHistories ++ validHFC
				-- gets output patterns
				let patterns = mapMaybe (\f -> case f of {(Pattern p)->Just p;_->Nothing}) o				
				-- in case file caching is enabled
				let splitFilePattern = (\(a,b) -> (reverse b, reverse a)) . span ('/'/=) . reverse
				let fileCaching = (listToMaybe $ mapMaybe (\f -> case f of {(FileCaching z)->Just z;_->Nothing}) o) >>=
				                  	(return . splitFilePattern)
				-- take also references from the standard input
				let readFromInput = any (\f -> case f of {ReadFromStdInput->True;_->False}) o
				-- takes bound for response channel; default is 5
				let responseChannelBound = fromMaybe 5 $ listToMaybe $
					mapMaybe (\f -> case f of {(ResponseChannelBound v) -> Just v; _ -> Nothing}) o
				return (validSeeds, loopRestrictions, userAgent, timeOut, sleepTime, badSuffixes, goodSuffixes,
				    	history, patterns, fileCaching, readFromInput, responseChannelBound)
			(_,_,errs) -> ioError (userError (concat errs ++ usageInfo "usage: crawler [OPTION ...] urls..." options)))
	seedChannel <- C.newChan
	responseChannel <- B.newBoundedChan responseChannelBound
	outputChannel <- C.newChan
	trackChannel <- C.newChan
	-- download thread
	void $ forkIO $ do
		log outputChannel $ T.pack "download thread started"
		let downloadSettings = DownloadSettings{
				downloadMaximumSleepTime = sleepTime,
				downloadMaximumTimeOut = timeOut,
				downloadUserAgent = userAgent,
				downloadBadSuffixes = badSuffixes,
				downloadGoodSuffixes = goodSuffixes}
		downloadResult <- try $ downloader downloadSettings seedChannel responseChannel trackChannel outputChannel
		case downloadResult of
			Left e -> error $ "download thread: exception " ++ show (e :: SomeException)
			Right _ -> log outputChannel $ T.pack "download thread stopped"
		closeOutputChan outputChannel
	-- parser thread
	void $ forkIO $ do
		log outputChannel $ T.pack "parser thread started"
		let parserSettings = ParserSettings {
			parserFileCaching = fileCaching,
			parserOutputs = patterns,
			parserLoopRestrictions = loopRestrictions}
		parserResult <- try $ parser parserSettings responseChannel trackChannel outputChannel
		case parserResult of
			Left e -> error $ "parser thread: exception " ++ show (e :: SomeException)
			Right _ -> log outputChannel $ T.pack "parser thread stopped"
		closeOutputChan outputChannel
	-- tracker thread
	void $ forkIO $ do
		log outputChannel $ T.pack "tracker thread started"
		trackerResult <- try $ tracker goodSuffixes badSuffixes history userAgent trackChannel seedChannel outputChannel
		case trackerResult of
			Left e -> error $ "tracker thread: exception " ++ show (e :: SomeException)
			Right _ -> log outputChannel $ T.pack "tracker thread stopped"
		closeOutputChan outputChannel
	-- stream into track channel
	writeListRingChan trackChannel $ map NewSeed originSeeds
	-- input thread
	when readStdInput $ void $ forkIO $ do
		getContents >>= return . map (\t -> maybe (Left t) Right $ parseAbsoluteURI t) . lines >>= mapM_ (\t -> case t of
			Left s -> log outputChannel $ T.pack $ "invalid absolute uri \"" ++ s ++ "\" will be ignored!"
			Right u -> writeRingChan trackChannel $ NewSeed u)
		closeOutputChan outputChannel
		signalLastRingChan trackChannel
	-- in case we dont read from standard input we have to give here
	-- an exta signal for the situation that no new url is coming in
	-- into the ring pipe from the outside
	when (not readStdInput) (signalLastRingChan trackChannel)
	-- output thread is on main thread since it indirectly synchronises all threads
	runOutput outputChannel (3 + (if readStdInput then 1 else 0)) $ \t -> case t of
		StdOut m -> TIO.hPutStrLn stdout m;
		StdErr m -> TIO.hPutStrLn stderr m >> hFlush stderr;
		StdOutFlush -> hFlush stdout
