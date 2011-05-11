-- discardText.hs
import Text.Pandoc
import Text.HTML.TagSoup
import System.IO
import System.Environment (getArgs)
import Data.List.Utils

readDoc :: String -> Pandoc
readDoc = readHtml defaultParserState{
                        stateParseRaw=True
                   }

writeDoc :: Pandoc -> String
writeDoc = writeLaTeX defaultWriterOptions


convertAll :: Pandoc -> Pandoc
convertAll = processWith convertInline

--convertInline :: [Inline] -> [Inline]
--convertInline (HtmlInline s : xs) = tag2latex $ parseTags s : convertInline xs
--convertInline (x : xs)     = x : convertInline xs
--convertInline []           = []

convertInline :: Inline -> Inline
convertInline (HtmlInline s) = tag2latex $ parseTags s
convertInline x = x

tag2latex :: [Tag String] -> Inline
tag2latex [TagOpen str attr] 
	| str == "latex" = TeX (attr2latex attr)
	| startswith "env" str = TeX 
		("\\begin{" ++ (drop 3 str) ++ "}" ++ (attr2latex attr))
	| otherwise = TeX ""
tag2latex [TagClose str] 
	| str == "latex" = TeX "}"
	| startswith "env" str = TeX ("\\end{" ++ (drop 3 str) ++ "}")
	| otherwise = TeX ""
tag2latex (t:ts) = TeX (strTex (tag2latex [t]) ++ strTex (tag2latex ts))
	where strTex (TeX s) = s
tag2latex _ = TeX ""

attr2latex :: [(String, String)] -> String
attr2latex (("command", c) : as) = '\\' : (c ++ (attr2latex as) ++ "{")
attr2latex (("optarg", a) : as) = '[' : (a ++ "]")
attr2latex ((_, a) : as) = '{' : (a ++ "}")
attr2latex [] 	= []

convert :: String -> String
convert = writeDoc . convertAll . readDoc

processFile :: String -> IO ()
processFile fn = catch (readFile fn >>= ((writeFile $ newName fn) . convert))
			(\_ -> hPutStr stderr ("Error processing: " ++ fn))
	where newName n = (fst . break (== '.')) n ++ ".tex"

run :: [String] -> IO ()
run (x:xs) = do
	processFile x
	run xs
run []	= return ()

main :: IO ()
main = do
	args <- getArgs
	if null args
		then interact convert
		else run args
