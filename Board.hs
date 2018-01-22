module Board where
import Solver
createBoard :: Int -> [[Char]] -> String
createBoard size values = do
    row <- [1..size]
    x <- [1..3]
    createCells row x
        where createCells row x =
                case (row, x) of (_, 1) -> printBorderRow ++ "+\n"
                                 (r, 2) -> printValueRow (values!!(r-1)) ++ "|\n"
                                 (r, 3) -> if r == size then printBorderRow ++ "+\n" else ""

              printBorderRow = do
                node <- [1..size]
                link <- [1..6]
                create node link
                    where create node link =
                            case (node, link) of (_, 1) -> "+"
                                                 (_, _) -> "-"

              printValueRow v = do
                node <- v
                link <- [1..6]
                create node link
                    where create node link =
                            case (node, link) of (_, 1) -> "|"
                                                 (n, 4) -> case n of 'D' -> "v"
                                                                     'P' -> ">"
                                                                     'L' -> "<"
                                                                     'G' -> "^"
                                                                     _ -> [n]
                                                 (_, _) -> " "



parseCoordinates :: Int ->[(Int, Int)] -> [String]
parseCoordinates size coordinates = do
     row     <-  [0..size-1]
     [do index <- [0..size-1]
         if (elem ((row, index)) coordinates) then "*" else " "]


parseResult :: Int -> [(Int, Int)] -> [(Int, Int, GasDir)] -> [String]
parseResult size coordinates result = do
     let mappedResult = map (\(a,b,c)->(a,b, show c)) result
     row  <-  [0..size-1]
     [do index <- [0..size-1]
         if (elem (row, index) coordinates)then "*"
         else tryResult (row, index) mappedResult]


tryResult :: (Int, Int)-> [(Int, Int, String)] -> String
tryResult _ [] = " "
tryResult (row, index) ((r,i,c):xs)  | row == r && index == i = c
                                     | otherwise = tryResult (row, index) xs
