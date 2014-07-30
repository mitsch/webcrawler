module Tracker (
--	TrackState,
--	TrackStateT,
--	memoriseInvalidTypeSuffix,
--	memoriseHistory,
--	memoriseRobots
--	track
) where

import Data.Set hiding (foldl, map)
import Data.List
import Data.Functor
import Network.URI
import Robots
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Data.Maybe
import Robots


-- merges dictionaries a and b with routine f to handle conflicts
unionDictionaries :: Eq a => (b -> b -> b) -> [(a, b)] -> [(a, b)] -> [(a, b)]
unionDictionaries f a b = foldl (g []) a b
	where g pre [] entry = entry : pre
	      g pre (post:posts) entry  = if fst post == fst entry then pre ++ [(fst entry, f (snd post) (snd entry))] ++ posts
	                                                           else g (post:pre) posts entry

-- splits on every element whose prediction p returns True
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn p x = let (a,b) = break p x in a:(splitOn p $ drop 1 b)

-- gets suffix from url
getSuffix :: URI -> Maybe String
getSuffix u = (listToMaybe $ reverse $ splitOn ('/'==) $ uriPath u) >>= (listToMaybe . reverse . drop 1 . splitOn ('.'==))

-- state of tracker
data TrackState a = TrackState {
	badSuffixes :: Set String ,
	history :: Set URI,
	robots :: [(String, Robots)],
	value :: a }

-- implementation of monad class
instance Monad TrackState where
	return x = TrackState { badSuffixes = Data.Set.empty, history = Data.Set.empty, robots = [], value = x }
	x >>= f = let y = f $ value x in TrackState {
		badSuffixes = Data.Set.union (badSuffixes x) (badSuffixes y),
		history = Data.Set.union (history x) (history y),
		robots = unionDictionaries unionRobots (robots x) (robots y),
		value = value y }

--implementation of functor class
instance Functor TrackState where
	fmap f x = x { value = f $ value x}

-- implementation of applicative class
instance Applicative TrackState where
	pure = return
	(<*>) = ap


-- transformer for tracker state
newtype TrackStateT m a = TrackStateT { runTrackStateT :: m (TrackState a) }

-- implementation of functor class for TrackStateT
instance (Functor m) => Functor (TrackStateT m) where
	fmap f = TrackStateT . fmap (fmap f) . runTrackStateT

-- implementation of MonadTrans class for TrackStateT
instance MonadTrans TrackStateT where
	lift = TrackStateT . liftM (\x -> TrackState { badSuffixes = Data.Set.empty, history = Data.Set.empty, robots = [], value = x})

-- implementation of Monad class for TrackStateT
instance (Monad m) => Monad (TrackStateT m) where
	return = lift . return
	x >>= f = TrackStateT $ (runTrackStateT x >>= runTrackStateT . f . value)

-- memorise invalid type suffix
memoriseInvalidTypeSuffix :: String -> TrackState ()
memoriseInvalidTypeSuffix s = TrackState { badSuffixes = Data.Set.singleton s, history = Data.Set.empty, robots = [], value = ()}

-- memorise visited web page
memoriseHistory :: URI -> TrackState ()
memoriseHistory u = TrackState { badSuffixes = Data.Set.empty, history = Data.Set.singleton u, robots = [], value = ()}

-- memorise robots
memoriseRobots :: String -> Robots -> TrackState ()
memoriseRobots h r = TrackState { badSuffixes = Data.Set.empty, history = Data.Set.empty, robots = [(h, r)], value = ()}

getTrackedBadSuffixes :: Monad m => TrackStateT m a -> TrackStateT m (Set String)
getTrackedBadSuffixes t = TrackStateT (runTrackStateT t >>= return . return . badSuffixes)

getTrackedHistory :: Monad m => TrackStateT m a -> TrackStateT m (Set URI)
getTrackedHistory t = TrackStateT (runTrackStateT t >>= return . return . history)

getTrackedRobots :: Monad m => TrackStateT m a -> TrackStateT m [(String, Robots)]
getTrackedRobots t = TrackStateT (runTrackStateT t >>= return . return . robots)

-- tracks web page
-- track :: String -> (URI -> IO String) -> TrackStateT IO URI -> TrackStateT IO (Maybe URI)
--track a g t = do
--	u <- t
--	s <- getTrackedBadSuffixes t
--	h <- getTrackedHistory t
--	r <- getTrackedRobots t
--	if member u h || (fromMaybe False $ fmap (flip member s) $ getSuffix u)
--	then return Nothing
--	else do
--		r' <- case lookup (uriRegName $ fromJust $ uriAuthority u) r of
--		      	Just r' -> return r'
--		      	Nothing -> do
--		        	r' <- lift $ fmap (makeRobots a) $ g u {uriPath = "/robots.txt", uriQuery = "", uriFragment = ""}
--		        	TrackStateT $ return $ memoriseRobots a r'
--		        	return r'
--		if isRobotsConform r' u
--		then do
--			TrackStateT $ return $ memoriseHistory u
--			liftM Just $ return u
--		else return Nothing

