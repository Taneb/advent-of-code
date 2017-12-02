module Main where

import Data.Char

captcha1 :: [Int] -> Int
captcha1 [] = 0
captcha1 [x] = x
captcha1 xs@(x:_:_) = go xs 0
  where
    go [a] n | a == x = n + x
             | otherwise = n
    go (a:r@(b:_)) n | a == b = go r (a + n)
                     | otherwise = go r n

captcha2 :: [Int] -> Int
captcha2 = (*2) . sum . map fst . filter (uncurry (==)) . uncurry zip . splitHalf
  where
    splitHalf :: [a] -> ([a], [a])
    splitHalf xs = splitAt (length xs `div` 2) xs

main :: IO ()
main = do
  l <- getLine
  let ns = map digitToInt l
  print $ captcha1 ns
  print $ captcha2 ns
