module Robots (Robots, makeRobots, makeEmptyRobots, isRobotsConform, unionRobots) where

import Network.URI
import Prelude
import Data.Maybe
import Data.List hiding (group)

-- splits on every element whose prediction p returns True
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn p x = let (a,b) = break p x in a:(splitOn p $ drop 1 b)

data Robots = Robots [String]

makeEmptyRobots :: Robots
makeEmptyRobots = Robots []

makeRobots :: String -> String -> Robots
makeRobots userAgent = Robots . compress . trimR . concat . filterAgents . map trans . group . lines
	where group :: [String] -> [[String]]
	      group = splitOn null
	      getUserAgents :: [String] -> [String]
	      getUserAgents = map (dropWhile (' '==) . drop 11) . filter (isPrefixOf "User-agent:")
	      getDisallows :: [String] -> [String]
	      getDisallows = map (dropWhile (' '==) . drop 9) . filter (isPrefixOf "Disallow:")
	      trans :: [String] -> ([String], [String])
	      trans s = (getUserAgents s, getDisallows s)
	      matchesAgent :: [String] -> Bool
	      matchesAgent = any (\a -> a == "*" || a == userAgent)
	      filterAgents :: [([String], [String])] -> [[String]]
	      filterAgents = mapMaybe (\(as, ds) -> if matchesAgent as then Just ds else Nothing)
	      trimR :: [String] -> [String]
	      trimR = map (takeWhile (' '/=))
	      compress :: [String] -> [String]
	      compress ds = if any null ds then [] else ds

isRobotsConform :: Robots -> URI -> Bool
isRobotsConform (Robots ds) u = (any null ds) || (not $ any (flip isPrefixOf (uriPath u)) ds)

unionRobots :: Robots -> Robots -> Robots
unionRobots (Robots a) (Robots b) = Robots $ foldl (\x y -> if y `elem` x then x else y:x) a b
