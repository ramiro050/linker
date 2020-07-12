module OrgParser
  ( section,
    orgLink,
    OrgLink (..),
    orgLinkToString ) where

import Text.Parsec
import Text.Parsec.String

data OrgLink = Link String | LinkDesc String String
  deriving (Show, Eq)

-- |Turn 'OrgLink' into a hyperlink that can be used in an org-mode file
orgLinkToString :: OrgLink -> String
orgLinkToString (Link l) = "[[" ++ l ++ "]]"
orgLinkToString (LinkDesc l d) = "[[" ++ l ++ "][" ++ d ++ "]]"

-- |Parsers hyperlinks used in org-mode files:
-- [[link]] or [[link][description]]
orgLink :: Parser OrgLink
orgLink = brackets $ (try linkDesc) <|> link
  where
    link = Link <$> brackets text
    linkDesc = LinkDesc <$> (brackets text) <*> (brackets text)

section :: Parser a -> Parser a
section s = char '*' >> spaces *> s <* spaces

nextSection :: Parser String
nextSection = manyTill anyChar $ try (char '*')

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

text :: Parser String
text = manyTill anyChar $ lookAhead (char ']')

{-
testString :: String
testString = "hello\nworld\n* abcd   \n\nnice stuff"

run :: Parser (String, String)
run = (,) <$> nextSection <*> many anyChar
-}
