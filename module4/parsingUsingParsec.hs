-- | 

module Main where

data PersonRecord = MkPersonRecord {
    name :: String,
    address :: Address,
    id :: Integer,
    labels :: [Label]
} deriving (Show)

data Address = MkAddress {
    line1 :: String,
    number :: Integer,
    street :: String,
    town :: String,
    postcode :: String
} deriving (Show)

data Label = Green | Red | Blue | Yellow deriving (Show)

rec1 = MkPersonRecord
    "Akshay Iyer"
    (MkAddress "Geekskool" 2698 "19th Main Kodihalli" "Bangalore" "560008")
    1000645
    [Green, Red]

rec2 = MkPersonRecord
    "Sumit Kumar"
    (MkAddress "Geekskool" 2698 "19th Main Kodihalli" "Bangalore" "560008")
    1006645
    [Blue, Yellow]

main = putStrLn $ show [rec1, rec2]

