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

    -- representation of the board (width, height) cells bombs status 
    data Board = Board (Int, Int) [Cell] [Int] Bool 

    -- intialises board dimensions
        -- input:
        -- x, y dimenesions 
        -- number of bombs
        -- output: 
        -- intialised board
    initialiseBoard :: (Int, Int) -> Int -> Int  ->  Board
    initialiseBoard (x, y) numBombs random = Board dims cells bombs False
                        where 
                            dims = (x, y)
                            cells1 = initBoard (x*y) [] 
                            bombs = initialiseBombs (x*y) numBombs [] random
                            cells = placeBombs cells1 bombs


    -- initialises all boards to empty to start
        -- input:
        -- number of cells to initialise
        -- array of cells
        -- output:
        -- intialised cells
    initBoard :: Int -> [Cell] -> [Cell]
    initBoard 0 xs = xs
    initBoard size xs = initBoard (size-1) (xs ++ [Empty])

    -- places all the bombs
        -- input:
        -- array of cells
        -- bomb locations
        -- output: 
        -- initalised squares
    placeBombs :: [Cell] -> [Int] -> [Cell]
    placeBombs cells [x] = 
                    let (head, _:tail) = splitAt x cells in
                            head ++ [Bomb] ++ tail
    placeBombs cells (x:xs) = 
                    let (head, _:tail) = splitAt x cells in
                            placeBombs (head ++ [Bomb] ++ tail) xs
    placeBombs cells _ = cells


    -- finds valid neighbours to check 
        -- input: 
        -- current board
        -- list of all neighbours
        -- list of current valid neighbours
        -- output:
        -- list all all valid neighbours to check 
    findNeighbs :: Board -> [Int] -> [Int] -> [Int]
    findNeighbs (Board size cells bombs status) [x] y = if isNeighbour (cells !! x) then y
                                                        else y++[x]
    findNeighbs (Board size cells bombs status) (x:xs) y = if isNeighbour (cells !! x) then findNeighbs (Board size cells bombs status) xs y
                                                         else findNeighbs (Board size cells bombs status) xs y++[x]
    findNeighbs _ _ y = y

    -- returnsLinear coords of neighbours to given tuple coord
        -- input:
        -- cell coordinate
        -- output:
        -- list of neighbours
    findNeighbours:: (Int, Int) -> [Int]
    findNeighbours (i, j) = map (\y -> tupleToLinear (10, 10) y) (filter (\x -> isValidCell (10, 10) x) neighbours)
        where
            neighbours = [(i - 1, j - 1), (i - 1, j), (i - 1, j + 1),
                            (i, j - 1), (i, j + 1),
                            (i + 1, j - 1), (i + 1, j), (i + 1, j + 1)]

    -- opens mines + neighbouring mines if there are no bomb neighbours
        -- input: 
        -- current board
        -- cell location
        -- output: 
        -- board after opening positions
    openMine :: Board -> (Int, Int) -> Board
    openMine (Board size cells bombs status) (x,y) = do
                        if isNeighbour (cells !! ((x*10)+y)) || (cells !! ((x*10)+y)) == Bomb then Board size cells bombs status
                        else do
                            let (head, _:tail) = splitAt ((x*10)+y) cells
                            let numBombs = findNumBombs (Board size cells bombs status) (x,y)
                            let currentBoard = Board size (head ++ [Neighbours numBombs] ++ tail) bombs status
                            if numBombs  == 0 then openNeighbours currentBoard  (findNeighbours (x,y))
                            else currentBoard

    -- returns true when the given cell is of type Neighbour
    isNeighbour ::  Cell -> Bool
    isNeighbour (Neighbours _) = True
    isNeighbour _ = False
    
    -- opens the neighbours of a mine using valid neighbour list
        -- input: 
        -- current board
        -- list of valid neighbours to open
        -- output: 
        -- updated board
    openNeighbours :: Board -> [Int] -> Board
    openNeighbours (Board size cells bombs status) [x] = do
                                                        let currentCell = cells !! x
                                                        if isNeighbour currentCell || currentCell == Bomb || currentCell == FlagBomb then Board size cells bombs status 
                                                        else openMine (Board size cells bombs status) (linearToTuple x size)
    openNeighbours (Board size cells bombs status) (x:xs) = do
                                                            let currentCell = cells !! x
                                                            if isNeighbour currentCell || currentCell == Bomb || currentCell == FlagBomb then openNeighbours (Board size cells bombs status) xs
                                                            else openNeighbours (openMine (Board size cells bombs status) (linearToTuple x size)) xs
    openNeighbours (Board size cells bombs status) _ = Board size cells bombs status
 
    -- returns number of bombs around a cell using its neighbours
        -- input:
        -- current board
        -- cell position
        -- output:
        -- number of bombs
    findNumBombs :: Board -> (Int, Int) -> Int
    findNumBombs board pos = findBombs board (findNeighbours pos) 0

    -- convert tuple coords to index in linear space
        -- input: 
        -- width, length
        -- coord 
        -- output:
        -- linear coord
    tupleToLinear:: (Int, Int) -> (Int, Int) -> Int
    tupleToLinear (width, _) (x, y) = (width * x) + y

    -- converts linear coords to a tuple
        -- input:
        -- linear coord
        -- width, length
        -- output:
        -- coord 
    linearToTuple:: Int -> (Int, Int) -> (Int, Int)
    linearToTuple i (width, _) = (i `div` width, i `mod` width)

    -- checks if the position is valid
        -- input: 
        -- number of rows, cols
        -- position to check 
        -- output:
        -- boolean of the validity of the cell
    isValidCell :: (Int, Int) -> (Int, Int) -> Bool
    isValidCell  (row, col) (x, y)    | x >= 0 && x < row && y >= 0 && y < col = True
                                        | otherwise = False

    -- finds the number of bombs around a cell using its list of neighbours
        -- input
        -- current board
        -- list of locations to check
        -- current count
        -- output: 
        -- int number of bombs
    findBombs :: Board -> [Int] -> Int -> Int
    findBombs (Board _ cells _ _) [x] input | cells !! x == Bomb || cells !! x == FlagBomb = input + 1
                                           | otherwise = input
    findBombs (Board size cells bombs status) (x:xs) input | cells !! x == Bomb || cells !! x == FlagBomb = findBombs (Board size cells bombs status) xs (input + 1)
                                           | otherwise = findBombs (Board size cells bombs status) xs input
    findBombs _ _ _  = 0

    -- initialises the bomb location list 
        -- input: 
        -- size of board
        -- number of bombs
        -- list of bomb locations so far
        -- random number
        -- output: 
        -- list of bomb locations
    initialiseBombs :: Int -> Int -> [Int] -> Int -> [Int]
    initialiseBombs size n bombsSoFar random
                | numBombs >= n    = take n bombsSoFar
                | otherwise        = initialiseBombs size n ( nub (newBombs ++ bombsSoFar) ) (random+1)
                where
                    numBombs = length bombsSoFar
                    g        = mkStdGen random 
                    newBombs =  take n (randomRs (0, size-1) g)

    -- flags a position in the board unless it has already been reavealed
        -- input: 
        -- current board
        -- position to flag (col, row)
        -- flag type - either flag empty or flag bomb
        -- output:
        -- board with flagged position
    flag :: Board -> (Int, Int) -> Cell -> Board
    flag (Board size cells bombs status) (x, y) newcell  = 
                        let (head, t:tail) = splitAt ((x*10)+y) cells in
                            if isNeighbour t then Board size cells bombs status
                            else 
                            Board size (head ++ [newcell] ++ tail) bombs status

    -- checks if the user has won - if there are no empty squares or flagged empty squares  
        -- input:
        -- current board
        -- output:
        -- boolean true for win or false for no win
    checkWin :: Board -> Bool
    checkWin (Board _ squares _ _) = (Empty `notElem` squares) && (FlagEmpty `notElem` squares)