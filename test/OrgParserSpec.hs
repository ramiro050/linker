module OrgParserSpec where

import OrgParser
import Text.Parsec
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck

linkGen :: Gen String
linkGen = resize 50 $ listOf (frequency [(15, letter), (10, num), (5, slash), (1, space)])
  where
    letter = elements $ ['a'..'z'] ++ ['A'..'Z']
    num = elements ['0'..'9']
    slash = elements "/"
    space = elements " "

descGen :: Gen String
descGen = resize 30 $ listOf (elements "abcdABCD1234 ")

instance Arbitrary OrgLink where
  -- arbitrary :: Gen a
  arbitrary = oneof [Link <$> linkGen, LinkDesc <$> linkGen <*> descGen]

{-
prop_brackets :: OrgLink -> Bool
prop_brackets (Link l) =
  case parse (brackets (string s)) "" bs of
    Left _ -> False
    Right s' -> s' == s
  where bs = "[" ++ s ++ "]"

spec :: Spec
spec = prop "Bracket parser is the inverse of wrapping a string in brackets" prop_brackets
-}
