module OrgParser
  ( section,
    brackets,
    link ) where

import Text.Parsec
import Text.Parsec.String

section :: Parser a -> Parser a
section s = char '*' >> spaces *> s <* spaces

nextSection :: Parser String
nextSection = manyTill anyChar $ try (char '*')

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

text :: Parser String
text = manyTill anyChar $ try (char ']')

link :: Parser (String, String)
link = brackets ((,) <$> (brackets text) <*> (brackets text))

testString :: String
testString = "hello\nworld\n* abcd   \n\nnice stuff"

run :: Parser (String, String)
run = (,) <$> nextSection <*> many anyChar

