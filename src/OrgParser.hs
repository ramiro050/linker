module OrgParser
  ( OrgObject (..),
    OrgInline (..),
    Org (..),
    orgLink) where

import Text.Parsec
import Text.Parsec.String
import Data.List (intercalate)

class Org ob where
  orgRead :: String -> Either ParseError ob
  orgShow :: ob -> String

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
  deriving Show

instance Org OrgObject where
  -- orgRead :: String -> Either ParseError ob
  orgRead = parse orgObject ""

  -- orgShow :: ob -> String
  orgShow (OrgTitle t) = "* " ++ orgShow t
  orgShow (OrgPara inls) = intercalate " " (orgShow <$> inls)
  orgShow (OrgList inls) = "- " ++ (intercalate "\n- " (orgShow <$> inls))

--------------------------------------------------------------------------------

type Description = String
type Link = String

data OrgInline = OrgStr String
               | OrgLink Link Description
  deriving (Show, Eq)

instance Org OrgInline where
  -- orgRead :: String -> Either ParseError ob
  orgRead = parse orgInline ""

  -- orgShow :: ob -> String
  orgShow (OrgStr s) = s

  orgShow (OrgLink l "") = "[[" ++ l ++ "]]"
  orgShow (OrgLink l d) = "[[" ++ l ++ "][" ++ d ++ "]]"

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
  where notLinkChar = notFollowedBy orgLink >> anyChar


orgInline :: Parser OrgInline
orgInline = try orgLink <|> orgStr

--------------------------------------------------------------------------------

-- |Extracts org links from a paragraph of text.
orgLinks :: Parser [OrgInline]
orgLinks = sepEndBy orgLink orgStr


orgTitle :: Parser OrgObject
orgTitle = OrgTitle <$> (string "* " >> orgInline)


orgPara :: Parser OrgObject
orgPara = OrgPara <$> para
  where para = many $ notFollowedBy orgTitle >> orgInline


orgList :: Parser OrgObject
orgList = OrgList <$> obs
  where obs = many $ string "- " >> orgInline


orgObject :: Parser OrgObject
orgObject = orgTitle <|> orgList <|> orgPara
