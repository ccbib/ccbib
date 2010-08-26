-- extractByTag.hs
--
-- Extract any text that has been markup up with a given tag.

import Text.HTML.TagSoup
import System.IO
import System.Environment (getArgs)
import Data.List.HT (breakAfter)
import Data.List (foldl')

import Data.Map (Map)
import qualified Data.Map as Map


data Entry = Entry Int (Tag String)
	deriving (Show, Eq)

instance Num Entry where
	(+) (Entry i a) (Entry j _) = Entry (i+j) a

instance Ord Entry where
        (>=) (Entry i1 a1) (Entry i2 a2)
                | i1 == i2  = (>=) a1 a2
                | otherwise = (>=) i1 i2
        (<=) (Entry i1 a1) (Entry i2 a2)
                | i1 == i2  = (<=) a1 a2
                | otherwise = (<=) i1 i2

data State = State Int (Map Entry Int)
	deriving (Show)

stateGetMap :: State -> (Map Entry Int)
stateGetMap (State lev m) = m

checkArgs :: [String] -> Bool
checkArgs x = any (elem '-') x

helpstring = "Bad Arguments!\n"

main :: IO ()
main = do 
	args <- getArgs
	if (checkArgs args) 
		then hPutStr stderr helpstring
		else interact (unlines . report)

report :: String -> [String]
report = reportLines . scan . canonicalizeTags . parseTags

reportLines :: (Map Entry Int) -> [String]
reportLines m = map fmt (Map.toAscList m)
        where fmt ((Entry l t), n) = 
                show l ++ "\t" ++ show n ++ "\t" ++ renderTags [t]

scan :: [Tag String] -> (Map Entry Int)
scan = stateGetMap . foldl' update (State 0 Map.empty)
	where 
	update (State lev m) t
		| isStartTag t = State (lev+1) (insert t lev m) 
		| isStopTag t  = State (lev-1) (insert t (lev-1) m)
		| isHtmlTag t = State lev (insert t lev m)
		| otherwise    = State lev m
	insert t l = Map.insertWith (+) (Entry l t) 1
-- update :: Tag String -> State -> State


isStartTag :: Tag String -> Bool
isStartTag (TagOpen str _) = elem str leveltags
isStartTag _ = False

isStopTag :: Tag String -> Bool
isStopTag (TagClose str) = elem str leveltags
isStopTag _ = False

isHtmlTag :: Tag String -> Bool
isHtmlTag (TagOpen _ _) = True
isHtmlTag (TagClose _)  = True
isHtmlTag _             = False

leveltags = ["html", "head", "title", "body", 
	"h1", "h2", "h3", "h4", "h5", "h6", "a", 
	"b", "strong", "i", "tt", "em", "font", "u", "small", "strike",
	"sub", "sup", "big", "span",
	"code", "pre", "cite", "blockquote", "center", "div", 
	"table", "ol", "ul"]
-- Not sure about, "p", ...
