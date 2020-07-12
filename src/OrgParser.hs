module OrgParser
  ( section,
    orgLink,
    OrgLink (..),
    orgLinkToString ) where

import Text.Parsec
import Text.Parsec.String

data OrgLink = Link String String
  deriving (Show, Eq)

-- |Parsers the link part of an org-mode link, returning the link itself.
orgLinkArg :: Parser String
orgLinkArg = many $ (notFollowedBy (char ']')) >> anyChar

-- |Parser the description part of an org-mode link, returning the description.
--
-- __Note:__ Org-mode allows square brackets in the description. The description
-- is only considered finished if it is followed by @]]@.
orgDescArg :: Parser String
orgDescArg = many $ (notFollowedBy (string "]]")) >> anyChar

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

-- |Turn 'OrgLink' into a hyperlink that can be used in an org-mode file
orgLinkToString :: OrgLink -> String
orgLinkToString (Link l "") = "[[" ++ l ++ "]]"
orgLinkToString (Link l d) = "[[" ++ l ++ "][" ++ d ++ "]]"

-- |Parses hyperlinks used in org-mode files:
--
-- - @[[link]]@ or @[[link][description]]@
orgLink :: Parser OrgLink
orgLink = brackets $ Link <$> link <*> desc
  where
    link = brackets orgLinkArg
    desc = brackets orgDescArg <|> string ""

section :: Parser a -> Parser a
section s = char '*' >> spaces *> s <* spaces

nextSection :: Parser String
nextSection = manyTill anyChar $ try (char '*')

{-
testString :: String
testString = "hello\nworld\n* abcd   \n\nnice stuff"

run :: Parser (String, String)
run = (,) <$> nextSection <*> many anyChar
-}
