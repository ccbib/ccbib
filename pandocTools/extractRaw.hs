-- extractRaw.hs
import Text.Pandoc

class RawContainer a where
  extractRaw :: a -> [String]

instance RawContainer Inline where
  extractRaw (HtmlInline s) = [s]
  extractRaw _ = []

instance RawContainer Block where
  extractRaw (RawHtml s) = [s]
  extractRaw _ = []

readDoc = readHtml defaultParserState{
                        stateParseRaw=True
                   }

main = interact (unlines . queryWith extractRaw . readDoc)

