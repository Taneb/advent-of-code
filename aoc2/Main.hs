module Main where

type Row = [Int]
type Spreadsheet = [Row]

checksumRow :: Row -> Int
checksumRow xs = maximum xs - minimum xs

checksum :: Spreadsheet -> Int
checksum = sum . map checksumRow

divisorsumRow :: Row -> Int
divisorsumRow xs = product [q | a <- xs, b <- xs, let (q, r) = a `divMod` b, r == 0]

divisorsum :: Spreadsheet -> Int
divisorsum = sum . map divisorsumRow

main :: IO ()
main = do
  raw <- getContents
  let spreadsheet = map (map read . words) . lines $ raw
  print $ checksum spreadsheet
  print $ divisorsum spreadsheet
