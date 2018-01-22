module Test where
import Board
import Solver

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
    putStrLn r