module Board where

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
                                                 (n, 4) -> [n]
                                                 (_, _) -> " "



parseCoordinates :: Int ->[(Int, Int)] -> [String]
parseCoordinates size coordinates = do
     row     <-  [0..size-1]
     let tuples  =  filter (\(a,b)->a == row) coordinates
     [do index <- [0..size-1]
         if (elem ((row, index)) tuples) then "*" else " "]

