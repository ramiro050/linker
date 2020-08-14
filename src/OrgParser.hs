module OrgParser
  (
    orgSection,
    OrgSection (..),
    orgSectionToString ) where

import Text.Parsec
import Text.Parsec.String

class Org ob where
  orgRead :: String -> Either ParseError ob
  orgShow :: ob -> String

type Description = String
type Link = String

type Header = String
data OrgFile = OrgFile Header [OrgSection]

type Body = String
type Title = String
data OrgSection = OrgSection Title Body
  deriving (Show, Eq)

--------------------------------------------------------------------------------

data OrgFile2 = OrgFile2 [OrgObject]

instance Monoid OrgFile2 where
  -- mempty :: a
  mempty = OrgFile2 []

instance Semigroup OrgFile2 where
  -- (<>) :: a -> a -> a
  (OrgFile2 xs) <> (OrgFile2 ys) = OrgFile2 (xs ++ ys)

--------------------------------------------------------------------------------

data OrgObject = OrgTitle OrgInline
                | OrgPara [OrgInline]
                | OrgList [OrgInline]

--------------------------------------------------------------------------------

data OrgInline = OrgStr String
               | OrgLink Link Description
  deriving Show

instance Org OrgInline where
  -- orgRead :: String -> Either ParseError ob
  orgRead = parse orgInline ""

  -- orgShow :: ob -> String
  orgShow (OrgStr s) = s

--------------------------------------------------------------------------------

  -- |Turn 'OrgLink' into a hyperlink that can be used in an org-mode file
  orgShow (OrgLink l "") = "[[" ++ l ++ "]]"
  orgShow (OrgLink l d) = "[[" ++ l ++ "][" ++ d ++ "]]"

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

--------------------------------------------------------------------------------

-- |Parses the link part of an org-mode link, returning the link itself.
orgLinkArg :: Parser Link
orgLinkArg = many $ (notFollowedBy (char ']')) >> anyChar


-- |Parser the description part of an org-mode link, returning the description.
-- |
-- |__Note:__ Org-mode allows square brackets in the description. The description
-- |is only considered finished if it is followed by @]]@.
orgDescArg :: Parser Description
orgDescArg = many $ (notFollowedBy (string "]]")) >> anyChar


-- |Parses hyperlinks used in org-mode files:
-- |
-- |- @[[link]]@ or @[[link][description]]@
orgLink :: Parser OrgInline
orgLink = brackets $ OrgLink <$> bLink <*> bDesc
  where
    brackets = between (char '[') (char ']')
    bLink = brackets orgLinkArg
    bDesc = brackets orgDescArg <|> string ""


orgStr :: Parser OrgInline
orgStr = OrgStr <$> (many notLinkChar)
  where
    notLinkChar = notFollowedBy orgLink >> anyChar


orgInline :: Parser OrgInline
orgInline = try orgLink <|> orgStr

--------------------------------------------------------------------------------

-- |Extracts org links from a paragraph of text.
orgLinks :: Parser [OrgInline]
orgLinks = sepEndBy orgLink orgStr


orgTitle :: Parser OrgObject
orgTitle = OrgTitle <$> (string "* " >> orgInline)


--orgPara :: Parser OrgObject
--orgPara = OrgPara <$> para
--  where para = many $ notFollowedBy orgTitle >> orgInline
