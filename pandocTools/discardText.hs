-- discardText.hs
import Text.Pandoc

discardText :: [Inline] -> [Inline]
discardText (Space : xs)  = discardText xs
discardText ((Str _) : xs) = discardText xs
discardText (x : xs)     = x : discardText xs
discardText []           = []

readDoc = readHtml defaultParserState{
                        stateParseRaw=True
                   }

writeDoc = writeNative defaultWriterOptions

main = interact (writeDoc . processWith discardText . readDoc)

