module OrgParserSpec where

import OrgParser
import Data.List
import Text.Parsec
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck


-- |Generator for the link part of an org-mode link.
linkGen :: Gen String
linkGen = resize 50 $ listOf (frequency [(15, letter), (10, num), (5, slash), (1, space)])
  where
    letter = elements $ ['a'..'z'] ++ ['A'..'Z']
    num = elements ['0'..'9']
    slash = elements "/"
    space = elements " "


-- |Generator for the description in an org-mode link.
-- |
-- |__Note:__ This generator generates empty descriptions half of the time
-- |in order to evenly generate links with and without descriptions.
descGen :: Gen String
descGen = oneof [descString `suchThat` validRightBrackets, pure ""]
  where
    descString = resize 30 $ listOf1 (elements "abcdABCD1234 []")
    validRightBrackets s = not $ ("]]" `isInfixOf` s || "]" `isSuffixOf` s)

newtype OrgLink = OL {getOrgLink :: OrgInline}
  deriving Show

instance Arbitrary OrgLink where
  -- arbitrary :: Gen a
  arbitrary = OL <$> (OrgLink <$> linkGen <*> descGen)


titleGen :: Gen String
titleGen = resize 30 $ listOf (elements "abcdABCD1234 ")


bodyGen :: Gen String
bodyGen = body `suchThat` validBody
  where
    body = resize 200 $ listOf (elements "abcABC123!* \n")
    validBody s = not $ ("\n" `isPrefixOf` s ||
                         "\n* " `isInfixOf` s ||
                         "\n" `isSuffixOf` s)


instance Arbitrary OrgSection where
  -- arbitrary :: Gen a
  arbitrary = OrgSection <$> titleGen <*> bodyGen


prop_parseLinks :: OrgLink -> Property
prop_parseLinks l =
  case parse orgLink "" (orgShow (getOrgLink l)) of
    Left error -> property False
    Right l' -> (getOrgLink l) === l'


spec :: Spec
spec = do
  prop "orgLink parser is the left inverse of orgLinkToString" prop_parseLinks
