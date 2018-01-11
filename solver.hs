-- typ danych okreslajacy kierunek przylacza zbiornika
-- z gazem, potrzebny na zewnatrz modulu, poniewaz
-- wystepuje w funkcji solve
-- lewo, prawo, gora, dol
data GasDir = L | P | G | D deriving Show

-- typ danych opisujacy drzewo rozwiazan
-- typy wezlow/lisci:
-- Empty - lisc rownowazny brakowi rozwiazania
-- Solution - lisc zawierajacy rozwiazanie
-- w formie listy pozycji zbiornikow
-- z gazem oraz kierunkow ich przylaczy
-- Node - wezel opisujacy czesciowe
-- rozwiazanie, w kolejnych galeziach
-- rozpatrywane sa kolejne pozycje
-- zbiornikow zwiazanych z kolejnym
-- domkiem na planszy
data SolutionTree = Empty| Solution [(Int, Int, GasDir)] | Node SolutionTree SolutionTree SolutionTree SolutionTree deriving Show


-- funkcja rozwiazujaca zagadke logiczna architekta
-- powinna zostac wykorzystana jako jedyna na 
-- zewnatrz modulu
-- opis funkcji:
-- ilosc zbior. w wierszach -> ilosc zbior. w kolumnach ->
-- pozycje domkow --> pozycje zbior. z kier. przylacza
solve :: [Int] -> [Int] -> [(Int, Int)] -> [(Int, Int, GasDir)]
solve [] _ _ = []
solve _ [] _ = []
solve _ _ [] = []
solve rowVals colVals housePos = findSolutionInTree (makeTree rowVals colVals housePos) 

-- funkcja zamieniajaca drzewo rozwiazan na rozwiazanie
-- opis funkcji:
-- drzewo rozwiazan -> lista zbiornikow z gazem z 
-- kierunkami przylaczy
findSolutionInTree :: SolutionTree -> [(Int, Int, GasDir)]
findSolutionInTree Empty = []
findSolutionInTree (Solution list) = list
findSolutionInTree (Node a b c d) = (findSolutionInTree a) ++ (findSolutionInTree b) ++ (findSolutionInTree c) ++ (findSolutionInTree d)


-- funkcja tworzaca drzewo rozwiazan
-- tworzy korzen drzewa z wykorzystaniem pomocniczej
-- funkcji tworzącej drzewo makeTree' wywołanej 
-- dla czterech sasiadujacych pol pierwszego domu 
-- z listy
-- opis funkcji:
-- ilosc zbior. w wierszach -> ilosc zbior. w kolumnach ->
-- pozycje domkow --> drzewo rozwiazan
 
makeTree :: [Int] -> [Int] -> [(Int, Int)] -> SolutionTree
makeTree [] _ _ = Empty
makeTree _ [] _ = Empty
makeTree _ _ [] = Empty
makeTree rowVals colVals ((houseY, houseX):houseRest) = Node 
  (makeTree' (houseY - 1, houseX, D) houseRest rowVals colVals ((houseY, houseX):houseRest) [])
  (makeTree' (houseY, houseX + 1, L) houseRest rowVals colVals ((houseY, houseX):houseRest) [])
  (makeTree' (houseY + 1, houseX, G) houseRest rowVals colVals ((houseY, houseX):houseRest) [])
  (makeTree' (houseY, houseX - 1, P) houseRest rowVals colVals ((houseY, houseX):houseRest) [])


-- pomocnicza funkcja tworzaca drzewo rozwiazan
-- wykorzystuje funkcje sprawdzajaca czy postawienie
-- zbiornika na danym polu jest zgodne z zasadami gry
-- (isLegal), jezeli tak to tworzy wezel oraz probuje
-- utworzyc dalsze cztery zbiorniki na czterech polach
-- sasiadujacych z kolejnym domem, jezeli postawienie
-- zbiornika jest nielegalne to tworzy lisc Empty
-- jezeli udalo sie wyczyscic wszystkie domki z listy
-- to taki lisc uznajemy za rozwiazanie (Solution)
-- opis funkcji
-- badane pole wraz z przewidywanym kierunkiem przylacza ->
-- lista domkow pozostalych do przylaczenia ->
-- ilosc zbior. w wierszach -> ilosc zbior. w kolumnach ->
-- pozycje wszystkich domkow --> aktualne pozycje zbiornikow ->
-- drzewo rozwiazan
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
      = Node 
      (makeTree' (yh - 1, xh, D) houseRest (decreaseListElem rowVals (y)) (decreaseListElem colVals (x)) houseList ((y, x, gasDir):gasTanks)) 
      (makeTree' (yh, xh + 1, L) houseRest (decreaseListElem rowVals (y)) (decreaseListElem colVals (x)) houseList ((y, x, gasDir):gasTanks)) 
      (makeTree' (yh + 1, xh, G) houseRest (decreaseListElem rowVals (y)) (decreaseListElem colVals (x)) houseList ((y, x, gasDir):gasTanks)) 
      (makeTree' (yh, xh - 1, P) houseRest (decreaseListElem rowVals (y)) (decreaseListElem colVals (x)) houseList ((y, x, gasDir):gasTanks)) 
    | otherwise = Empty


-- funkcja zmniejszajaca n-ty element listy o 1
-- opis funkcji:
-- lista -> pozycja elementu ->
-- lista ze zdekrementowanym n-tym elementem
decreaseListElem :: [Int] -> Int -> [Int]
decreaseListElem [] _ = []
decreaseListElem list n | n < 0 = []
                        | n > ((length list) -1) = []
                        | otherwise = take n list ++ [(list!!n) - 1] ++ drop (n + 1) list


-- funkcja sprawdzajaca czy pole jest domem
-- opis funkcji
-- badane pole -> lista domow ->
-- wynik boolowski
isHouse :: (Int, Int) -> [(Int, Int)] -> Bool
isHouse _ [] = False
isHouse (y, x) houses | x < 0 = True
                      | y < 0 = True
                      | otherwise = elem (y, x) houses

-- funkcja sprawdzajaca czy pole jest sasiadem
-- domu
-- jesli pole jest samo w sobie domem to
-- zwraca false
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
-- zbiornika z gazem lub samym zbiornikiem
-- postawionym wczesniej
-- zakladamy poprawnosc planszy wejsciowej
-- opis funkcji:
-- badane pole -> lista zbiornikow -> wynik
isGasNeighbour :: (Int, Int) -> [(Int, Int, GasDir)] -> Bool
isGasNeighbour _ [] = False
isGasNeighbour (y, x) ((b, a, _):bas) | y < 0                    = True
                                      | x < 0                    = True
                                      | (y, x) == (b, a)         = True
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
-- lista domow -> lista postawionych do tej pory zbiornikow 
-- ->  wynik
isLegal :: (Int, Int) -> [Int] -> [Int] -> [(Int, Int)] -> [(Int, Int, GasDir)] -> Bool
isLegal _ [] _ _ _ = False
isLegal _ _ [] _ _ = False
isLegal _ _ _ [] _ = False
isLegal (y, x) rowVals colVals houseList gasList | y < 0 = False
                                                 | x < 0 = False
                                                 | y > ((length rowVals) - 1) = False 
                                                 | x > ((length colVals) - 1) = False 
                                                 | isNeighbour (y, x) houseList == False   = False
                                                 | isHouse (y, x) houseList == True        = False
                                                 | isGasNeighbour (y, x) gasList == True   = False
                                                 | elem (y,x) houseList == True            = False
                                                 | rowVals!!y < 1  = False
                                                 | colVals!!x < 1  = False
                                                 | otherwise = True
