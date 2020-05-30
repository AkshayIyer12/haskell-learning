module Main where

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
   

jsonValue :: Parser JsonValue
jsonValue = undefined

main :: IO ()
main = undefined




