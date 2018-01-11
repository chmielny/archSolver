-- typ danych okreslajacy kierunek przylacza zbiornika
-- z gazem
-- lewo, prawo, gora, dol
data GasDir = L | P | G | D deriving Show

-- typ danych opisujacy drzewo rozwiazan
data SolutionTree = Empty| Solution [(Int, Int, GasDir)] | Node SolutionTree SolutionTree SolutionTree SolutionTree deriving Show


-- funkcja rozwiazujaca zagadke logiczna architekta
-- opis funkcji:
-- ilosc zbior. w wierszach -> ilosc zbior. w kolumnach ->
-- pozycje domkow --> pozycje zbior. z kier. przylacza
solve :: [Int] -> [Int] -> [(Int, Int)] -> [(Int, Int, GasDir)]
solve [] _ _ = []
solve _ [] _ = []
solve _ _ [] = []
solve rowVals colVals housePos = findSolutionInTree (makeTree rowVals colVals housePos) 

--zamiana drzewa rozwiazan na rozwiazanie
findSolutionInTree :: SolutionTree -> [(Int, Int, GasDir)]
findSolutionInTree Empty = []
findSolutionInTree (Solution list) = list
findSolutionInTree (Node a b c d) = (findSolutionInTree a) ++ (findSolutionInTree b) ++ (findSolutionInTree c) ++ (findSolutionInTree d)



-- funkcja tworzaca drzewo rozwiazan
makeTree :: [Int] -> [Int] -> [(Int, Int)] -> SolutionTree
makeTree [] _ _ = Empty
makeTree _ [] _ = Empty
makeTree _ _ [] = Empty
makeTree rowVals colVals ((houseY, houseX):houseRest) = Node 
  (makeTree' (houseY - 1, houseX, D) houseRest rowVals colVals ((houseY, houseX):houseRest) [])
  (makeTree' (houseY, houseX + 1, L) houseRest rowVals colVals ((houseY, houseX):houseRest) [])
  (makeTree' (houseY + 1, houseX, G) houseRest rowVals colVals ((houseY, houseX):houseRest) [])
  (makeTree' (houseY, houseX - 1, P) houseRest rowVals colVals ((houseY, houseX):houseRest) [])


--                                   house                         const house list  actual gas tanks
makeTree' :: (Int, Int, GasDir) -> [(Int, Int)] -> [Int] -> [Int] -> [(Int, Int)] -> [(Int, Int, GasDir)] -> SolutionTree
makeTree' (y, x, gasDir) [] rowVals colVals houseList gasTanks 
    |  isLegal (y, x) rowVals colVals houseList gasTanks == True  
       = Solution ((y, x, gasDir):gasTanks) -- rozwiazanie zagadki to ten przypadek
    | otherwise = Empty
makeTree' _ _ [] _ _ _ = Empty
makeTree' _ _ _ [] _ _ = Empty
makeTree' _ _ _ _ [] _ = Empty
makeTree' (y, x, gasDir) ((yh, xh):houseRest) rowVals colVals houseList gasTanks 
    | isLegal (y, x) rowVals colVals houseList gasTanks == True 
--      = Node [(y, x, gasDir)] 
--      = Node gasTanks 
      = Node 
      (makeTree' (yh - 1, xh, D) houseRest (decreaseListElem rowVals (y)) (decreaseListElem colVals (x)) houseList ((y, x, gasDir):gasTanks)) 
      (makeTree' (yh, xh + 1, L) houseRest (decreaseListElem rowVals (y)) (decreaseListElem colVals (x)) houseList ((y, x, gasDir):gasTanks)) 
      (makeTree' (yh + 1, xh, G) houseRest (decreaseListElem rowVals (y)) (decreaseListElem colVals (x)) houseList ((y, x, gasDir):gasTanks)) 
      (makeTree' (yh, xh - 1, P) houseRest (decreaseListElem rowVals (y)) (decreaseListElem colVals (x)) houseList ((y, x, gasDir):gasTanks)) 
    | otherwise = Empty


-- funkcja zmniejszajaca n-ty element listy o 1
decreaseListElem :: [Int] -> Int -> [Int]
decreaseListElem [] _ = []
decreaseListElem list n | n < 0 = []
                        | n > ((length list) -1) = []
                        | otherwise = take n list ++ [(list!!n) - 1] ++ drop (n + 1) list


-- funkcja sprawdzajaca czy pole jest sasiadem
-- domu
-- zakladamy poprawnosc planszy wejsciowej
-- opis funkcji:
-- badane pole -> lista domow -> wynik
isNeighbour :: (Int, Int) -> [(Int, Int)] -> Bool
isNeighbour _ [] = False
isNeighbour (y, x) ((b, a):bas) | y < 0                = False
                                | x < 0                = False
                                | (y, x) == (b - 1, a) = True
                                | (y, x) == (b + 1, a) = True
                                | (y, x) == (b, a - 1) = True
                                | (y, x) == (b, a + 1) = True
                                | otherwise            = isNeighbour (y, x) bas


-- funkcja sprawdzajaca czy pole jest sasiadem
-- zbiornika z gazem
-- zakladamy poprawnosc planszy wejsciowej
-- opis funkcji:
-- badane pole -> lista zbiornikow -> wynik
isGasNeighbour :: (Int, Int) -> [(Int, Int, GasDir)] -> Bool
isGasNeighbour _ [] = False
isGasNeighbour (y, x) ((b, a, _):bas) | y < 0                    = True
                                      | x < 0                    = True
                                      | (y, x) == (b - 1, a)     = True
                                      | (y, x) == (b + 1, a)     = True
                                      | (y, x) == (b, a - 1)     = True
                                      | (y, x) == (b, a + 1)     = True
                                      | (y, x) == (b - 1, a - 1) = True
                                      | (y, x) == (b + 1, a + 1) = True
                                      | (y, x) == (b + 1, a - 1) = True
                                      | (y, x) == (b - 1, a + 1) = True
                                      | otherwise                = isGasNeighbour (y, x) bas


-- funkcja sprawdzajaca czy postawienie zbiornika 
-- na danym polu jest zgodne z regulami gry
-- zakladamy poprawnosc danych wejsciowych
-- opis funkcji:
-- badane pole -> aktualne liczby zbiorników do 
-- postawienia w wierszach -> aktualne liczby
-- zbiornikow do postawienia w kolumnach ->
-- lista domow -> lista postawionych zbiornikow ->  wynik
isLegal :: (Int, Int) -> [Int] -> [Int] -> [(Int, Int)] -> [(Int, Int, GasDir)] -> Bool
isLegal _ [] _ _ _ = False
isLegal _ _ [] _ _ = False
isLegal _ _ _ [] _ = False
isLegal (y, x) rowVals colVals houseList gasList | y < 0 = False
                                                 | x < 0 = False
                                                 | y > ((length rowVals) - 1) = False 
                                                 | x > ((length colVals) - 1) = False 
                                                 | isNeighbour (y, x) houseList == False   = False
                                                 | isGasNeighbour (y, x) gasList == True   = False
                                                 | elem (y,x) houseList == True            = False
                                                 | rowVals!!y < 1  = False
                                                 | colVals!!x < 1  = False
                                                 | otherwise = True
