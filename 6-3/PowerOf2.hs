module PowerOf2 
  ( PowerOf2
  , makePowerOf2
  , exponentPowerOf2
  ) where

newtype PowerOf2 = PowerOf2 Integer deriving (Eq, Show)

isPowerOf2 :: Integer -> Bool
isPowerOf2 n
  | n == 1 = True
  | n < 1 || odd n = False
  | otherwise = isPowerOf2 (n `div` 2)

-- | 整数値から2の冪乗型への変換
--
-- >>> makePowerOf2 1
-- Just (PowerOf2 1)
-- >>> makePowerOf2 2
-- Just (PowerOf2 2)
-- >>> makePowerOf2 3
-- Nothing
-- >>> makePowerOf2 1024
-- Just (PowerOf2 1024)

makePowerOf2 :: Integer -> Maybe PowerOf2
makePowerOf2 n
  | isPowerOf2 n = Just (PowerOf2 n)
  | otherwise    = Nothing

-- | 2の冪乗に対し、それが2の何乗されたものかを得る
--
-- >>> exponentPowerOf2 (PowerOf2 1)
-- 0
-- >>> exponentPowerOf2 (PowerOf2 2)
-- 1
-- >>> exponentPowerOf2 (PowerOf2 4)
-- 2
-- >>> exponentPowerOf2 (PowerOf2 1024)
-- 10

exponentPowerOf2 :: PowerOf2 -> Integer
exponentPowerOf2 (PowerOf2 n) = exponentPowerOf2' 0 n where
  exponentPowerOf2' :: Integer -> Integer -> Integer
  exponentPowerOf2' r n
    | n == 1         = r
    | otherwise      = exponentPowerOf2' (r + 1) (n `div` 2)
