{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Board where

    import System.Random
    import Data.List
    
    -- All the possible cells
    data Cell = Bomb
                | Empty
                | FlagBomb
                | FlagEmpty
                | Neighbours Int
                deriving (Eq, Show)

    -- representation of the board (width, height) bombs currentBoard
    data Board = Board (Int, Int) [Cell] [Int]

    -- intialises board dimensions
    initialiseBoard :: (Int, Int) -> Int -> Board
    initialiseBoard (x, y) numBombs = Board dims cells bombs
                        where 
                            dims = (x, y)
                            cells1 = initBoard (x*y) [] 
                            bombs = initialiseBombs (x*y) numBombs []
                            cells = placeBombs cells1 bombs


    -- initialises all boards to empty to start
    initBoard :: Int -> [Cell] -> [Cell]
    initBoard 0 xs = xs
    initBoard size xs = initBoard (size-1) (xs ++ [Empty])

    -- places all the bombs
    placeBombs :: [Cell] -> [Int] -> [Cell]
    placeBombs cells [x] = 
                    let (head, _:tail) = splitAt x cells in
                            head ++ [Bomb] ++ tail
    placeBombs cells (x:xs) = 
                    let (head, _:tail) = splitAt x cells in
                            placeBombs (head ++ [Bomb] ++ tail) xs

    -- returnsLinear coords of neighbours to given tuple coord
    findNeighbours:: (Int, Int) -> [Int]
    findNeighbours (i, j) = map (\y -> tupleToLinear (10, 10) y) (filter (\x -> isValidCell (10, 10) x) neighbours)
        where
            neighbours = [((i - 1), (j - 1)), ((i - 1), (j)), ((i - 1), (j + 1)),
                            ((i), (j - 1)), ((i), (j + 1)),
                            ((i + 1), (j - 1)), ((i + 1), (j)), ((i + 1), (j + 1))]

    clearNeighbours :: Board -> (Int, Int) -> Board
    clearNeighbours (Board size cells bombs) (x,y)  | findNumBombs (Board size cells bombs) (x,y) == 0 = checkEachNeighbour (Board size cells bombs) (findNeighbours (x,y))
                                                    | otherwise = (Board size cells bombs)

    checkEachNeighbour :: Board -> [Int] -> Board
    checkEachNeighbour (Board size cells bombs) [x] = placeNeighbours (Board size cells bombs) (linearToTuple x (10,10) )
    checkEachNeighbour (Board size cells bombs) (x:xs) = checkEachNeighbour (placeNeighbours (Board size cells bombs) (linearToTuple x (10,10) ) ) xs

    placeNeighbours :: Board -> (Int, Int) -> Board
    placeNeighbours (Board size cells bombs) (x,y) = 
                        let (head, _:tail) = splitAt ((x*10)+y) cells in
                            Board size (head ++ [Neighbours (findNumBombs (Board size cells bombs) (x,y))] ++ tail) bombs
 
    findNumBombs :: Board -> (Int, Int) -> Int
    findNumBombs board pos = findBombs board (findNeighbours pos) 0

    -- convert tuple coords to index in linear space
    tupleToLinear:: (Int, Int) -> (Int, Int) -> Int
    tupleToLinear (width, _) (x, y) = (width * x) + y

    linearToTuple:: Int -> (Int, Int) -> (Int, Int)
    linearToTuple i (width, _) = ((i `div` width) , (i `mod` width))

    
    isValidCell :: (Int, Int) -> (Int, Int) -> Bool
    isValidCell  (row, col) (x, y)    | (x >= 0 && x < row && y >= 0 && y < col) = True
                                        | otherwise = False

    findBombs :: Board -> [Int] -> Int -> Int
    findBombs (Board _ cells _) (x:[]) input | cells !! x == Bomb || cells !! x == FlagBomb = input + 1
                                           | otherwise = input
    findBombs (Board size cells bombs) (x:xs) input | cells !! x == Bomb || cells !! x == FlagBomb = findBombs (Board size cells bombs) xs (input + 1)
                                           | otherwise = findBombs (Board size cells bombs) xs input
    findBombs _ _ _  = 0

    -- initialises the bomb location list
    initialiseBombs :: Int -> Int -> [Int] -> [Int]
    initialiseBombs size n bombsSoFar
                | numBombs >= n    = take n bombsSoFar
                | otherwise        = initialiseBombs size n ( nub (newBombs ++ bombsSoFar) )
                where
                    numBombs = length bombsSoFar
                    g        = mkStdGen numBombs
                    newBombs =  take n (randomRs (0, size-1) g)

    flag :: Board -> (Int, Int) -> Cell -> Board
    flag (Board size cells bombs) (x, y) newcell  = 
                        let (head, _:tail) = splitAt ((x*10)+y) cells in
                            Board size (head ++ [newcell] ++ tail) bombs

    checkWin :: Board -> Bool
    checkWin (Board _ squares _) = (Empty `notElem` squares) && (FlagEmpty `notElem` squares)