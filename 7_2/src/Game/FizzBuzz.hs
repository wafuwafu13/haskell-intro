module Game.FizzBuzz where

fizzbuzz :: Int -> String
fizzbuzz n = case n `gcd` 15 of
               15 -> "FizzBuzz"
               5  -> "Buzz"
               3  -> "Fizz"
               _  -> show n
