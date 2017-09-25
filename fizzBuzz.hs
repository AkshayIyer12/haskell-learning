main = print [ fizzbuzz x | x <- [1..100 ] ]
         where fizzbuzz x
                | x `multipleOf` [3, 5] = "FizzBuzz"
                | x `multipleOf` [3] = "Fizz"
                | x `multipleOf` [5] = "Buzz"
                | otherwise = show x
                 where m `multipleOf` ns =
                         all (\n -> m `mod` n == 0) ns
