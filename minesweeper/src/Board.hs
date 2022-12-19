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
                            cells1 = initBoard ((x*10)+y) [] 
                            bombs = initialiseBombs ((x*10)+y) numBombs []
                            cells = placeBombs cells1 bombs


    -- initialises all boards to empty to start
    initBoard :: Int -> [Cell] -> [Cell]
    initBoard 0 xs = xs
    initBoard size xs = initBoard (size-1) (xs ++ [Empty])

    -- places all the bombs
    placeBombs :: [Cell] -> [Int] -> [Cell]
    placeBombs cells (x:[]) = 
                    let (head, _:tail) = splitAt x cells in
                            head ++ [Bomb] ++ tail
    placeBombs cells (x:xs) = 
                    let (head, _:tail) = splitAt x cells in
                            placeBombs (head ++ [Bomb] ++ tail) xs

    --findNeighbours :: Board -> (Int, Int) -> Int
    --findNeighbours 

    --initialiseBombs :: Int -> Int -> [Int] -> [Int]
    --initialiseBombs _ 0 bombs = bombs
    --initialiseBombs size numBombs bombs = initialiseBombs size numbBombs-1 (initBombs bombs 0)

    -- initialises the bomb location list
    initialiseBombs :: Int -> Int -> [Int] -> [Int]
    initialiseBombs size n bombsSoFar
                | numBombs >= n    = take n bombsSoFar
                | otherwise        = initialiseBombs size n ( nub (newBombs ++ bombsSoFar) )
                where
                    numBombs = length bombsSoFar
                    g        = mkStdGen numBombs
                    newBombs =  take n (randomRs (0, size-1) g)

    --initBombs :: [Int] -> Int -> [Int]
    --initBombs bombs location = bombs ++ location


    flag :: Board -> (Int, Int) -> Cell -> Board
    flag (Board size cells bombs) (x, y) newcell  = 
                        let (head, _:tail) = splitAt ((x*10)+y) cells in
                            Board size (head ++ [newcell] ++ tail) bombs

    checkWin :: Board -> Bool
    checkWin (Board _ squares _) = (Empty `notElem` squares) && (FlagEmpty `notElem` squares)