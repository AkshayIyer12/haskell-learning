module Main where

import Data.Char

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

-- stringP :: String -> Parser String
-- stringP = sequenceA . map charP

jsonNull :: Parser JsonValue
jsonNull = undefined

jsonValue :: Parser JsonValue
jsonValue = undefined

main :: IO ()
main = undefined
