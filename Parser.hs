module Parser (
	ParserOutput(PreContext, PostContext, ReferenceText, ReferenceAttribute, EmbeddedImageAttribute, ReferenceName, ReferenceSuffix),
	parse
) where

import Text.HTML.TagSoup
import Text.StringLike
import Data.Either()
import Data.String()
import Data.Maybe
import Data.List
import Network.URI
import Text.StringLike()
import Control.Monad
import Control.DeepSeq

-- splits on every element whose prediction p returns True
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn p x = let (a,b) = break p x in a:(splitOn p $ drop 1 b)

-- apply function f to x only if y is Just, otherwise return x
maybeApply :: (a -> b -> a) -> Maybe b -> a -> a
maybeApply f x y = fromMaybe y (x >>= return . f y)

-- test whether a is prefix of b
isPrefixOfSL :: StringLike str => str -> str -> Bool
isPrefixOfSL a b = and $ zipWith (==) (unfoldr uncons a) (unfoldr uncons b)

-- drops certain amount of elements from stringlike object
dropSL :: StringLike str => Int -> str -> str
dropSL n s = fromMaybe empty $ foldM (\a b -> fmap snd $ uncons $ const a b) s [1..n]

-- drops elements as long as prediction p returns True
dropWhileSL :: StringLike str => (Char -> Bool) -> str -> str
dropWhileSL p = fromMaybe empty . g
	where g s = uncons s >>= \(x, xs) -> if p x then g xs else return s

-- takes elements as long as prediction p returns True
-- takeWhileSL :: StringLike str => (Char -> Bool) -> str -> str
-- takeWhileSL p = fromMaybe empty . g
--	where g s = uncons s >>= \(x, xs) -> if p x then g xs else return s

-- spans elements with prediction p
spanSL :: StringLike str => (Char -> Bool) -> str -> (str, str)
spanSL p s = fromMaybe (s, empty) $ g empty s
	where g pre post = uncons post >>= \(x, xs) -> if p x then g (append pre $ fromChar x) xs else return (pre, post)

-- splits into words
wordsSL :: StringLike str => str -> [str]
wordsSL s = let (pre, post) = spanSL (not . flip elem "\r\n\t ") $ dropWhileSL (flip elem " \r\n\t") s in pre : (wordsSL post)

-- joins together by putting a whitespace in between every element
unwordsSL :: StringLike str => [str] -> str
unwordsSL = strConcat . intersperse (fromChar ' ')

-- removes all data section in the html code
removeData :: StringLike str => str -> str
removeData s
	| strNull s = empty
	| isPrefixOfSL (fromString "\"data:") s = removeData $ dropSL 1 $ dropWhileSL ('\"'==) $ dropSL 6 s
	| isPrefixOfSL (fromString "\'data:") s = removeData $ dropSL 1 $ dropWhileSL ('\''==) $ dropSL 6  $ dropSL 6 s
	| otherwise = maybe empty (\(x, xs) -> x `cons` removeData xs) $ uncons s

-- canonicalise tag
canonicaliseURI :: URI -> URI
canonicaliseURI u = u {uriPath = if null $ uriPath u then "/" else uriPath u, uriFragment = ""}

forceURIStrictness :: URI -> URI
forceURIStrictness u = (uriScheme u) `deepseq` (g $ uriAuthority u) `deepseq` (uriPath u)
                       `deepseq` (uriFragment u) `deepseq` (uriQuery u) `deepseq` u
	where g Nothing = ()
	      g (Just a) = (uriUserInfo a) `deepseq` (uriRegName a) `deepseq` (uriPort a) `deepseq` ()

-- makes uri absolute if not so; in case it is relative it will be related to h
makeAbsoluteURI :: URI -> String -> Maybe URI
--makeAbsoluteURI h = liftM (\u -> if Network.URI.uriIsRelative u then u `relativeTo` h else u) . parseURIReference
makeAbsoluteURI h = liftM (\u -> if uriScheme u /= "" then u else  forceURIStrictness (u `relativeTo` h)) . parseURIReference

-- get text of tags
getInnerText :: StringLike str => [Tag str] -> str
getInnerText = strConcat . intersperse (fromChar ' ') . mapMaybe maybeTagText


data ParserOutput = PreContext (Maybe Int) (Maybe String) |
                    PostContext (Maybe Int) (Maybe String) |
										ReferenceText |
										ReferenceAttribute String  |
										EmbeddedImageAttribute String |
										ReferenceName |
										ReferenceSuffix

-- prints output according to the patterns
printOutput :: (Show str, Eq str, StringLike str) => [ParserOutput] -> [Tag str] -> Tag str -> [Tag str] -> [Tag str] -> [str]
printOutput ps pre mainTag mainArea post = map g ps
	where g (PreContext wordLim tagLim) = maybeApply cutPreWordLim wordLim $ getInnerText $ maybeApply cutPreTagLim tagLim pre
	      g (PostContext wordLim tagLim) = maybeApply cutPostWordLim wordLim $ getInnerText $ maybeApply cutPostTagLim tagLim post
	      g ReferenceText = getInnerText mainArea
	      g (ReferenceAttribute s) = fromAttrib (fromString s) mainTag
	      g (EmbeddedImageAttribute s) = strConcat $ intersperse (fromChar ' ') $ map (fromAttrib (fromString s)) $
	      	filter (isTagOpenName (fromString "img")) mainArea
	      g ReferenceName = case mainTag of {(TagOpen n _) -> n; _ -> empty}
	      g ReferenceSuffix = fromString $ fromMaybe "" $
	                                         (parseURIReference $ toString $ fromAttrib (fromString "href") mainTag) >>=
	                                         (listToMaybe . reverse . splitOn ('/'==) . uriPath) >>=
	                                         (listToMaybe . reverse . drop 1 . splitOn ('.'==))
	      cutPreTagLim ts n = let n' = fromString n
	                          in reverse $ takeWhile (\t -> (not $ isTagOpenName n' t) || (not $ isTagCloseName n' t)) $ reverse ts
	      cutPreWordLim s n = unwordsSL $reverse $ take n $ reverse $ wordsSL s
	      cutPostTagLim ts n = let n' = fromString n
	                           in takeWhile (\t -> (not $ isTagOpenName n' t) || (not $ isTagCloseName n' t)) ts
	      cutPostWordLim s n = unwordsSL $ take n $ wordsSL s

parse :: (Show str, Eq str, StringLike str) => [ParserOutput] -> URI -> str -> [(URI, [str])]
parse outputs host document = let
	splitTags _ [] = [];
	splitTags reversedPreTags (x:xs) = (reverse reversedPreTags, x, xs) : (splitTags (x : reversedPreTags) xs);
	f (preTags, mainTag, postTags)
		| (fromChar 'a') `isTagOpenName` mainTag = let (mainTags, postMainTags) = span (isTagCloseName (fromChar 'a')) postTags
		                                           in Just (preTags, mainTag, mainTags, postMainTags)
		| (fromString "link") `isTagOpenName` mainTag = Just (preTags, mainTag, [], postTags)
		| otherwise = Nothing;
	g (preTags, mainTag, mainTags, postTags) = do
		u <- makeAbsoluteURI host $ toString $ fromAttrib (fromString "href") mainTag
		u `seq` (return (canonicaliseURI u, printOutput outputs preTags mainTag mainTags postTags))
	in mapMaybe g $ mapMaybe f $ splitTags [] $ canonicalizeTags $ parseTags $ removeData document
