-- https://hackage.haskell.org/package/doctest
-- https://stackoverflow.com/questions/53402263/could-not-find-module-test-quickcheck-on-windows

module DoctestSample where

-- $setup
-- >>> import QuickCheck

-- | 文字列中のスペースの個数
--
-- >>> countSpace ""
-- 0
-- >>> countSpace "foobarbaz"
-- 0
-- >>> countSpace "hello, world!"
-- 1
-- >>> countSpace "    "
-- 4
--
-- prop> countSpace s == sum [ 1 | c <- s, c == ' ' ]
--
countSpace :: String -> Int
countSpace = length . filter (' ' ==)
