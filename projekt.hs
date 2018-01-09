-- typ danych okreslajacy kierunek przylacza zbiornika
-- z gazem
-- lewo, prawo, gora, dol
data GasDir = L | P | G | D


-- funkcja rozwiazujaca zagadke logiczna architekta
-- opis funkcji:
-- ilosc zbior. w wierszach -> ilosc zbior. w kolumnach ->
-- pozycje domkow --> pozycje zbior. z kier. przylacza
solve :: [Int] -> [Int] -> [(Int, Int)] -> [(Int, Int, GasDir)]
solve [] _ _ = []
solve _ [] _ = []
solve _ _ [] = []
solve rowVals colVals housePos = [(1,1,L)]


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


-- funkcja sprawdzajaca czy postawienie zbiornika 
-- na danym polu jest zgodne z regulami gry
-- zakladamy poprawnosc danych wejsciowych
-- opis funkcji:
-- badane pole -> aktualne liczby zbiorników do 
-- postawienia w wierszach -> aktualne liczby
-- zbiornikow do postawienia w kolumnach ->
-- lista domow -> wynik
isLegal :: (Int, Int) -> [Int] -> [Int] -> [(Int, Int)] -> Bool
isLegal _ [] _ _ = False
isLegal _ _ [] _ = False
isLegal _ _ _ [] = False
isLegal (y, x) rowVals colVals houseList | y < 0 = False
                                         | x < 0 = False
                                         | y > ((length rowVals) - 1) = False 
                                         | x > ((length colVals) - 1) = False 
                                         | isNeighbour (y, x) houseList == False   = False
                                         | elem (y,x) houseList == True            = False
                                         | rowVals!!y < 1  = False
                                         | colVals!!x < 1  = False
                                         | otherwise = True
