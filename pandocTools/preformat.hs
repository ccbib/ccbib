-- preformat.hs
--
-- inspects HTML to find special formatting
-- cleanup HTML to make it more pandoc friendly

import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Char
import System.IO
import System.Environment (getArgs)

opendoublequote = '\x8220'
closingdoublequote = '\x8221'

data Setup = Setup {
	input :: Maybe String,
	output :: Maybe String,
	report1 :: Maybe String,
	report2 :: Maybe String,
	rules :: [TagTree String -> [TagTree String]]
} 

defaultSetup = Setup Nothing Nothing Nothing Nothing [] 

readSetup :: [String] -> Setup -> Setup
readSetup []     = id
readSetup (x:xs) = (readSetup xs) . (parseSetup (words x))

parseSetup :: [String] -> Setup -> Setup
parseSetup (('#':_):_) x = x
parseSetup [] x = x
parseSetup ("output":file:_) s = s {output = Just file}
parseSetup ("input":file:_) s  = s {input = Just file}
parseSetup ("report1":file:_) s = s {report1 = Just file}
parseSetup ("report2":file:_) s = s {report2 = Just file }
parseSetup ("fixquotes":_) s = s {rules = (textfilter fixquotes : rules s)}
parseSetup ("ignore":tag) s = s {rules = (ignore tag : rules s)}
parseSetup ("remove":tag) s = s {rules = (remove tag : rules s)}
parseSetup ("replace":sel:templ:_) s = s {rules = (replace sel templ : rules s)}
parseSetup ("killattr":sel:attr) s = s {rules = (killattr sel attr : rules s)}
parseSetup line _ = error ("Can't parse: " ++ (unwords line))

textfilter :: (String -> String) -> TagTree String -> [TagTree String]
textfilter f (TagLeaf (TagText s)) = [TagLeaf (TagText (f s))]
textfilter _ t = [t]

fixquotes :: String -> String
fixquotes ('"' : c : s) 
	| or [isSpace c, elem c ").,;"] = closingdoublequote : c : fixquotes s
	| isAlpha c 			= opendoublequote : c : fixquotes s
	| otherwise			= '"' : c : fixquotes s
fixquotes (c : '"' : s) 
	| or [isSpace c, c == '('] 	= c : opendoublequote : fixquotes s
	| and [isAlpha c, (not . isAlpha . head) s] =
			 c : closingdoublequote : fixquotes s
	| otherwise = c : '"' : fixquotes s
fisquotes (c : s) = c : fixquotes s
--fixquotes ""      = ""

ignore :: [String] -> TagTree String -> [TagTree String]
ignore s t = if match t s
	then tagContent t
	else [t]

remove :: [String] -> TagTree String -> [TagTree String]
remove s t = if match t s
	then []
	else [t]

killattr :: String -> [String] -> TagTree String -> [TagTree String]
killattr selector as t = if selects (splitOn ";" selector) t
	then [filterAttrs t]
	else [t]
	where 
		filterAttrs (TagBranch name attrs c) = 
			(TagBranch name (filter pred attrs) c)
		filterAttrs (TagLeaf (TagOpen name attrs)) =
			(TagLeaf (TagOpen name (filter pred attrs)))
		filterAttrs t = t
		pred x = not (elem (fst x) as)

replace :: String -> String -> TagTree String -> [TagTree String]
replace selector template t = if selects (splitOn ";" selector) t
	then [write (map attrPair (splitOn ";" template)) t]
	else [t]
	where 
		write (("tag", n):xs) (TagLeaf (TagOpen _ as)) = 
			(TagLeaf (TagOpen n (h as xs)))
		write xs (TagLeaf (TagOpen n as)) = 
			(TagLeaf (TagOpen n (h as xs)))
		write (("tag", n):xs) (TagBranch _ as c) = 
			(TagBranch n (h as xs) c)
		write xs (TagBranch n as c)          = (TagBranch n (h as xs) c)
		h as = mapMaybe (h2 as)
		h2 as (i,[]) = liftM (\s -> (i,s)) $ lookup i as
		h2 _  p      = Just p
			

selects :: [String] -> TagTree String -> Bool
selects xs t = all (matches t) (map attrPair xs)
	where 
		matches (TagBranch name as _) p@(e1, e2) 
			| e1 == "tag" = e2 == name
			| e2 == []    = elem e1 $ map fst as
			| otherwise   = elem p as
		matches (TagLeaf (TagOpen name as)) p@(e1, e2)
                        | e1 == "tag" = e2 == name
                        | e2 == []    = elem e1 $ map fst as
                        | otherwise   = elem p as
		matches _ _ = False

attrPair :: String -> (String, String)
attrPair = h . (break (== '='))
	where
		h (e1,('=':e2)) = (e1, e2)
		h p = p


tagContent :: TagTree String -> [TagTree String]
tagContent (TagBranch _ _ ts) = ts
tagContent (TagLeaf _)        = []

match :: TagTree String -> [String] -> Bool
match t s = alike t ((head . canonicalizeTags . parseTags . unwords) s)

alike :: TagTree String -> Tag String -> Bool
alike (TagBranch s1 a1 _) (TagOpen s2 a2) = s1 == s2 && a1 == a2
alike (TagLeaf t1) t2 = t1 == t2
alike _ _ = False

unlevel :: [TagTree String] -> ([Tag String], [TagTree String])
unlevel []     = ([], [])
unlevel (x:xs) = let (as, bs) = unlevel xs
                     (t, ls) = strip x
                 in ((t:as), ls++bs)

strip :: TagTree String -> (Tag String, [TagTree String])
strip (TagLeaf x)         = (x, [])
strip (TagBranch n as ts) = (TagOpen n as, ts)

sortByLevel :: [TagTree String] -> [[Tag String]]
sortByLevel [] = [[]]
sortByLevel ts = let (cs, ns) = unlevel ts
                 in ((filter isTagOpen cs) : sortByLevel ns)

histogram :: [Tag String] -> [(Tag String, Int)]
histogram xs = let ts = group $ sort xs
               in zip (map head ts) (map length ts)

displayStats :: [[(Tag String, Int)]] -> [String]
displayStats xs = let headers = map ("\nLevel " ++) (map show [1..])
               in concat $ map (uncurry (:)) (zip headers (map (map format) xs))
               where format (t, n) = show n ++ "\t" ++ renderTags [t]

helptext = "Commandlinearguments are supposed to be control files and\nshould not start with dashes.\n"

parseArgs :: [String] -> Setup -> IO Setup
parseArgs [] s          = return s
parseArgs (('-':_):_) _ = error helptext
--parseArgs (a:as) = ((readFile a) >>=) ((parseArgs as) . readSetup . lines)
parseArgs (a:as) s = do
	c <- readFile a
	parseArgs as (readSetup (lines c) s)

report :: [TagTree String] -> String
report = unlines . displayStats . (map histogram) . sortByLevel

main :: IO ()
main = do 
	args <- getArgs
	setup <- parseArgs args defaultSetup
	raw <- maybe getContents readFile (input setup)
	let origTree = (tagTree . canonicalizeTags . parseTags) raw
	if null args
		then hPutStr stdout (report origTree)
		else return ()
	maybe (return ()) (flip writeFile (report origTree)) (report1 setup)
	let newTree = foldr transformTree origTree (rules setup)
	maybe (return ()) (\n -> (writeFile n) . renderTags . flattenTree $ newTree) (output setup)
	case (report2 setup) of
		Just n -> writeFile n (report newTree)
		Nothing -> return ()
