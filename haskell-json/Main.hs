module Main where

import Data.Char
import Control.Applicative

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

--Note: no proper error reporting
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
                          (input', x) <- p input
                          Just (input', f x)


instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
                                    (input', f) <- p1 input
                                    (input'', a) <- p2 input'
                                    Just(input'', f a)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
                                     p1 input <|> p2 input
-- *Main> runParser (charP 'n') "nice"
-- Just ("ice",'n')
-- *Main> runParser (charP 'n') "Hello"
-- Nothing

charP :: Char -> Parser Char
charP x = Parser f
    where
      f (y:ys)
          | y == x    = Just (ys, x)
          | otherwise = Nothing
      f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
    where f "true"  = JsonBool True
          f "false" = JsonBool False
          f _       = undefined

jsonValue :: Parser JsonValue
jsonValue = undefined

main :: IO ()
main = undefined
