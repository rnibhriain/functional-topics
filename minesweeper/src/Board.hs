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
    placeBombs cells (x:[]) = 
                    let (head, _:tail) = splitAt x cells in
                            head ++ [Bomb] ++ tail
    placeBombs cells (x:xs) = 
                    let (head, _:tail) = splitAt x cells in
                            placeBombs (head ++ [Bomb] ++ tail) xs

    findNeighbours :: Board -> (Int, Int) -> Board
    findNeighbours (Board size cells bombs) (x, y) = 
                let (head, _:tail) = splitAt ((x*10)+y) cells in
                            Board size (head ++ [Neighbours (findBombs (Board size cells bombs) (x, y))] ++ tail) bombs

    findBombs :: Board -> (Int, Int) -> Int
    findBombs _ _ = 0

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