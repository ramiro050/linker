module OrgParser
  ( orgLink,
    OrgLink (..),
    orgLinkToString,
    orgSection,
    OrgSection (..),
    orgSectionToString ) where

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
  deriving (Show, Eq)


-- |Parses an org file, retuning an @OrgFile@ structure.
orgFile :: Parser OrgFile
orgFile = OrgFile <$> header <*> many orgSection
  where
    header = many $ notFollowedBy (many1 newline >> string "* ") >> anyChar


-- |Parses a whole org section, returning an @OrgSection@.
orgSection :: Parser OrgSection
orgSection = do
  string "* "
  title <- many $ notFollowedBy newline >> anyChar
  skipMany newline
  body <- many $ notFollowedBy (many1 newline >> string "* ") >> anyChar
  return $ OrgSection title body


orgSectionToString :: OrgSection -> String
orgSectionToString (OrgSection t b) = "* " ++ t ++ "\n" ++ b


-- |Parses the link part of an org-mode link, returning the link itself.
orgLinkArg :: Parser Link
orgLinkArg = many $ (notFollowedBy (char ']')) >> anyChar


-- |Parser the description part of an org-mode link, returning the description.
-- |
-- |__Note:__ Org-mode allows square brackets in the description. The description
-- |is only considered finished if it is followed by @]]@.
orgDescArg :: Parser Description
orgDescArg = many $ (notFollowedBy (string "]]")) >> anyChar


-- |Turn 'OrgLink' into a hyperlink that can be used in an org-mode file
orgLinkToString :: OrgLink -> String
orgLinkToString (OrgLink l "") = "[[" ++ l ++ "]]"
orgLinkToString (OrgLink l d) = "[[" ++ l ++ "][" ++ d ++ "]]"


-- |Parses hyperlinks used in org-mode files:
-- |
-- |- @[[link]]@ or @[[link][description]]@
orgLink :: Parser OrgLink
orgLink = brackets $ OrgLink <$> bLink <*> bDesc
  where
    brackets = between (char '[') (char ']')
    bLink = brackets orgLinkArg
    bDesc = brackets orgDescArg <|> string ""


-- |Extracts org links from a paragraph of text.
orgLinks :: Parser [OrgLink]
orgLinks = sepEndBy orgLink notLink
  where
    notLink = many $ (notFollowedBy (string "[[")) >> anyChar
