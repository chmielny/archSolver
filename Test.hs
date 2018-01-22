module Test where

import Main

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