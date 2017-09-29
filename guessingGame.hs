import System.Random

check :: String -> String -> Char -> (Bool, String)
check word display c
  = (c `elem` word, [if x==c then c else y | (x,y) <- zip word display])

turn :: String -> String -> Int -> IO ()
turn word display n 
  | n==0          = putStrLn "You lose"
  | word==display = putStrLn "You win!"
  | otherwise     = mkguess word display n

mkguess :: String -> String -> Int -> IO ()
mkguess word display n =
  do putStrLn (display ++ "  " ++ take n (repeat '*'))
     putStr "  Enter your guess: "
     q <- getLine
     let (correct, display') = check word display (q!!0)
     let n' = if correct then n else n-1
     turn word display' n'

starman :: Int -> IO ()
starman n = do
  text <- readFile "./words.txt"
  let words = lines text
  do num <- randomIO ::IO Int
     let i = mod num (length words)
     let word = words !! i
     print words
     turn word ['-' | x <- word] n
