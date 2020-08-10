module OrgParser
  ( section,
    orgLink,
    OrgLink (..),
    orgLinkToString ) where

import Text.Parsec
import Text.Parsec.String

type Description = String
type Link = String

data OrgLink = OrgLink Link Description
  deriving (Show, Eq)

type Header = String
data OrgFile = OrgFile Header [OrgSection]

type Body = String
type Title = String
data OrgSection = OrgSection Title Body
  deriving Show


orgSection :: Parser OrgSection
orgSection = OrgSection <$> title <*> body
  where
    title = string "* " *> manyTill anyChar ((newline >> return ()) <|> eof)
    body = many $ (notFollowedBy (string "\n* ")) >> anyChar


-- |Parsers the link part of an org-mode link, returning the link itself.
orgLinkArg :: Parser Link
orgLinkArg = many $ (notFollowedBy (char ']')) >> anyChar


-- |Parser the description part of an org-mode link, returning the description.
-- |
-- |__Note:__ Org-mode allows square brackets in the description. The description
-- |is only considered finished if it is followed by @]]@.
orgDescArg :: Parser Description
orgDescArg = many $ (notFollowedBy (string "]]")) >> anyChar


brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')


-- |Turn 'OrgLink' into a hyperlink that can be used in an org-mode file
orgLinkToString :: OrgLink -> String
orgLinkToString (OrgLink l "") = "[[" ++ l ++ "]]"
orgLinkToString (OrgLink l d) = "[[" ++ l ++ "][" ++ d ++ "]]"


-- |Parses hyperlinks used in org-mode files:
-- |
-- |- @[[link]]@ or @[[link][description]]@
orgLink :: Parser OrgLink
orgLink = brackets $ OrgLink <$> link <*> desc
  where
    link = brackets orgLinkArg
    desc = brackets orgDescArg <|> string ""


nextSection :: Parser String
nextSection = manyTill anyChar $ try (char '*')

{-
testString :: String
testString = "hello\nworld\n* abcd   \n\nnice stuff"

run :: Parser (String, String)
run = (,) <$> nextSection <*> many anyChar
-}
