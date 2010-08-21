-- extractByTag.hs
--
-- Extract any text that has been markup up with a given tag.

import Text.HTML.TagSoup
import System.IO
import System.Environment (getArgs)
import Data.List.HT (breakAfter)

checkArgs :: [String] -> Bool
checkArgs [] = True
checkArgs x = any (elem '-') x

helpstring = "Bad Arguments!\n"

main :: IO ()
main = do 
	args <- getArgs
	if (checkArgs args) 
		then hPutStr stderr helpstring
		else interact (unlines . extractByTag args)

extractByTag :: [String] -> String -> [String]
extractByTag tags s = map renderTags (filterTags tags (parseTags s))

filterTags :: [String] -> [Tag String] -> [[Tag String]]
filterTags _ [] = []
filterTags tags (t:ts) 
	| isStartTag tags t = 
		let (good, rest) = breakAfter (TagClose (tagStr t)) (t:ts)
		in good:(filterTags tags rest)
	| otherwise = filterTags tags ts

isStartTag :: [String] -> Tag String -> Bool
isStartTag tags (TagOpen str _) = elem str tags
isStartTag _ _ = False

-- isStopTag :: String -> Tag String -> Bool
-- isStopTag tag (TagClose str) = (tag == str)
-- isStopTag _ _ = False

