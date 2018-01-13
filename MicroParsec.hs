{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- |
-- Module      : MicroParsec
-- Description : A small parser combinator example
--
-- Inspired by:
-- <http://dev.stephendiehl.com/fun/002_parsers.html>
--
module MicroParsec where

import           Control.Applicative
import           Control.Monad
import           Data.Char


-- * Parser

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, _)]    -> error "Parser did not consume entire stream."
    _           -> error "Parser error."

instance Functor Parser where
  fmap f (Parser cs) = Parser (\ s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure                          = return
  (Parser cs1) <*> (Parser cs2) = Parser (\ s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return a        = Parser (\ s -> [(a, s)])
  Parser af >>= k = Parser (\ s -> concatMap (\ (a, s') -> parse (k a) s') (af s))

failure :: Parser a
failure = Parser (const [])

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\ s -> parse p s ++ parse q s)

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \ s ->
  case parse p s of
    []  -> parse q s
    res -> res

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = failure
  (<|>) = option

-- * Combinators

item :: Parser Char
item = Parser $ \ s ->
  case s of
    []       -> []
    (c : cs) -> [(c, cs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \ c ->
  if p c
  then return c
  else Parser (const [])

oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

char :: Char -> Parser Char
char c = satisfy (c ==)

digit :: Parser Char
digit = satisfy isDigit

alpha :: Parser Char
alpha = satisfy isAlpha

natural :: Parser Integer
natural = read <$> some digit

string :: String -> Parser String
string []     = return []
string (c:cs) = char c >> string cs >> return (c:cs)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  return a

reserved :: String -> Parser String
reserved s = token (string s)

word :: Parser String
word = some alpha

number :: Parser Int
number = do
  s  <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n

-- * Untyped Lambda Calculus example

data Lam
  = Var String
  | Abs String Lam
  | App Lam Lam
  deriving Show

absP :: Parser Lam -> Parser Lam
absP bodyP = do
  reserved "\\"
  p <- token word
  reserved "."
  x <- bodyP
  return (Abs p x)

varP :: Parser Lam
varP = Var <$> token word

nonAppP :: Parser Lam
nonAppP = parens lamP <|> absP lamP <|> varP

lamP :: Parser Lam
lamP = nonAppP `chainl1` (spaces >> return App)

main :: IO ()
main = forever $ do
  putStr "> "
  a <- getLine
  print $ runParser lamP a
