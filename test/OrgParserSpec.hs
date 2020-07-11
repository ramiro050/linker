module OrgParserSpec
  ( spec
  ) where

import OrgParser
import Text.Parsec
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck

newtype Link = Link String
  deriving Show

instance Arbitrary Link where
  -- arbitrary :: Gen a
  arbitrary = Link <$> (listOf (elements "abcdefg/"))

prop_brackets :: Link -> Bool
prop_brackets (Link s) =
  case parse (brackets (string s)) "" bs of
    Left _ -> False
    Right s' -> s' == s
  where bs = "[" ++ s ++ "]"

spec :: Spec
spec = prop "Bracket parser is the inverse of wrapping a string in brackets" prop_brackets
