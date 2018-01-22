module Main where

import System.IO
import Board
import Solver

main :: IO ()
main =  do  putStrLn "Hello in the puzzle solver"
            putStrLn "Type path to your data file"
            path <- getLine
            content <- readLines path
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
            putStrLn "Solving..."
            let solution = solve axisX axisY coordinates
            putStrLn "Solution"
            print solution
            let parsedSolution = parseResult size coordinates solution
            let solutionBoard = createBoard size parsedSolution
            putStrLn solutionBoard
            print "End"



readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

convertToInt :: String -> [Int]
convertToInt input = read input

convertToTuple :: String -> [(Int, Int)]
convertToTuple input = read input


test1 = do
    let v = [" *    ","      ","      ","  * * ","*   * ","  *  *"]
    let size = length v
    let a = createBoard size v
    putStrLn a

test2 = do
    let v = [(0, 1), (3, 2), (3, 4), (4, 0), (4, 4), (5, 2), (5, 5)]
    let size = 6
    let a = parseCoordinates size v
    print a

test3 = do
    content <- readLines "wej1.txt"
    let axisX =  convertToInt.head $ content
    let axisY =  convertToInt.head.tail $ content
    let coordinates =  convertToTuple.head.tail.tail $ content
    let size = length axisX
    let parsed = parseCoordinates size coordinates
    let board = createBoard size parsed
    print coordinates
    print axisX
    print axisY
    putStrLn board
    print "End"

test4 = do
    let c = [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)];
    let d = [(4,5,D),(5,1,P),(4,3,P),(3,0,D),(2,4,D),(2,2,D),(0,2,L)]
    let size = 6
    let r = parseResult size c d
    print r


test5 = do
    let c = [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)];
    let d = [(4,5,D),(5,1,P),(4,3,P),(3,0,D),(2,4,D),(2,2,D),(0,2,L)]
    let size = 6
    let r = parseResult size c d
    let board = createBoard size r
    putStrLn board