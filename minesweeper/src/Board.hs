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

    data Mode = MARKING | OPENING

    -- representation of the board (width, height) bombs currentBoard
    data Board = Board (Int, Int) [Cell] [Int] Bool 

    -- intialises board dimensions
    initialiseBoard :: (Int, Int) -> Int -> Board
    initialiseBoard (x, y) numBombs = Board dims cells bombs False
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

    openMine :: Board -> (Int, Int) -> Board
    openMine (Board size cells bombs status) (x,y) = do
                        if isNeighbour (cells !! ((x*10)+y)) then Board size cells bombs status
                        else do
                            let (head, _:tail) = splitAt ((x*10)+y) cells
                            let currentBoard = Board size (head ++ [Neighbours (findNumBombs (Board size cells bombs status) (x,y))] ++ tail) bombs status
                            if findNumBombs (Board size cells bombs status) (x,y)  == 0 then openNeighbours (Board size cells bombs status) (findNeighbours (x,y))
                            else currentBoard

    isNeighbour ::  Cell -> Bool
    isNeighbour (Neighbours _) = True
    isNeighbour _ = False
            
    openNeighbours :: Board -> [Int]-> Board
    openNeighbours (Board size cells bombs status) [x] = if isNeighbour (cells !! x) then Board size cells bombs status 
                                                  else openMine (Board size cells bombs status) (linearToTuple x size)
    openNeighbours (Board size cells bombs status) (x:xs) = if isNeighbour (cells !! x) then openNeighbours (Board size cells bombs status) xs
                                                    else openNeighbours (openMine (Board size cells bombs status) (linearToTuple x size)) xs


    clearNeighbours :: Board -> (Int, Int) -> Board
    clearNeighbours (Board size cells bombs status) (x,y)  | findNumBombs (Board size cells bombs status) (x,y) == 0 = checkEachNeighbour (Board size cells bombs  status) (findNeighbours (x,y))
                                                    | otherwise = Board size cells bombs status
                                                
    
    checkEachNeighbour :: Board -> [Int] -> Board
    checkEachNeighbour (Board size cells bombs status) [x] = placeNeighbours (Board size cells bombs status) (linearToTuple x (10,10) )
    checkEachNeighbour (Board size cells bombs status) (x:xs) = checkEachNeighbour (placeNeighbours (Board size cells bombs status) (linearToTuple x (10,10) ) ) xs

    placeNeighbours :: Board -> (Int, Int) -> Board
    placeNeighbours (Board size cells bombs status) (x,y) = 
                        let (head, _:tail) = splitAt ((x*10)+y) cells in
                            Board size (head ++ [Neighbours (findNumBombs (Board size cells bombs status) (x,y))] ++ tail) bombs status
 
    findNumBombs :: Board -> (Int, Int) -> Int
    findNumBombs board pos = findBombs board (findNeighbours pos) 0

    -- convert tuple coords to index in linear space
    tupleToLinear:: (Int, Int) -> (Int, Int) -> Int
    tupleToLinear (width, _) (x, y) = (width * x) + y

    linearToTuple:: Int -> (Int, Int) -> (Int, Int)
    linearToTuple i (width, _) = (i `div` width, i `mod` width)

    
    isValidCell :: (Int, Int) -> (Int, Int) -> Bool
    isValidCell  (row, col) (x, y)    | x >= 0 && x < row && y >= 0 && y < col = True
                                        | otherwise = False

    findBombs :: Board -> [Int] -> Int -> Int
    findBombs (Board _ cells _ _) [x] input | cells !! x == Bomb || cells !! x == FlagBomb = input + 1
                                           | otherwise = input
    findBombs (Board size cells bombs status) (x:xs) input | cells !! x == Bomb || cells !! x == FlagBomb = findBombs (Board size cells bombs status) xs (input + 1)
                                           | otherwise = findBombs (Board size cells bombs status) xs input
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
    flag (Board size cells bombs status) (x, y) newcell  = 
                        let (head, _:tail) = splitAt ((x*10)+y) cells in
                            Board size (head ++ [newcell] ++ tail) bombs status

    checkWin :: Board -> Bool
    checkWin (Board _ squares _ _) = (Empty `notElem` squares) && (FlagEmpty `notElem` squares)