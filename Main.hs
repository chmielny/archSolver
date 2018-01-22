module Main where

import System.IO
import Board

main :: IO ()
main =  do  putStrLn "Hello in the puzzle solver"
            putStrLn "Type path to your data file"
            -- path <- getLine
            content <- readLines "wej1.txt"
            let axisX =  convertToInt.head $ content
            let axisY =  convertToInt.head.tail $ content
            let coordinates =  convertToTuple.head.tail.tail $ content
            let size = length axisX
            let parsed = parseCoordinates size coordinates
            let board = createBoard size parsed
            putStrLn "Loaded data:"
            print coordinates
            print axisX
            print axisY
            putStrLn board
            print "End"



readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

convertToInt :: String -> [Int]
convertToInt input = read input

convertToTuple :: String -> [(Int, Int)]
convertToTuple input = read input

