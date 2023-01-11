module Player where

    import Game
    import Board

    removeDetails :: Board -> Board
    removeDetails (Board size cells mines status) = Board size (exchangeDetails cells 0) mines status

    exchangeDetails :: [Cell] -> Int -> [Cell]
    exchangeDetails cells x = 
                    let (head, t:tail) = splitAt x cells in
                        if (t == Bomb) || t == Empty then
                            head ++ [Unknown] ++ tail
                        else if (t == FlagEmpty) || t == FlagBomb then
                            head ++ [Flag] ++ tail
                        else cells

    cell :: Int -> Int -> [Int] -> [Int] -> [Cell] -> (Int, Int) -> Int
    cell cellpos cellVal unrevealedNeighbours flaggedNeighbours grid dim | length flaggedNeighbours == cellVal && length unrevealedNeighbours > 0 = head unrevealedNeighbours
                                                                | length flaggedNeighbours == cellVal = basic grid dim (cellpos + 1) 
                                                                | (length unrevealedNeighbours + length flaggedNeighbours) == cellVal = head unrevealedNeighbours
                                                                | otherwise = basic grid dim (cellpos + 1) 

    findSurroundingFlagged :: Int -> [Cell] -> (Int, Int) -> [Int]
    findSurroundingFlagged coord grid dim = filter (\n -> (grid !! n  == FlagEmpty || grid !! n  == FlagBomb)) (findNeighbours (linearToTuple coord dim))

    -- a naive solving algorithm, should solve most basic grids i.e. 10x10 grid with 10 mines
    basic :: [Cell] -> (Int, Int) -> Int -> Int
    basic  grid dim i | i == ((length grid) -1 ) = 0
                    | grid !! i == FlagBomb = basic  grid dim (i + 1)
                    | grid !! i == FlagEmpty = basic  grid dim (i + 1)
                    | neighbourNumber (grid !! i) > 0 = cell i (neighbourNumber (grid !! i)) (findNeighbs grid (findNeighbours (linearToTuple i dim)) []) (findSurroundingFlagged i grid dim) grid dim            
                    -- = cell i (digitToInt (grid !! i)) (findUnrevealedNeighbours i grid dim) (findSurroundingFlagged i grid dim) grid dim
                    | otherwise = basic  grid dim (i + 1)  

    neighbourNumber :: Cell -> Int
    neighbourNumber (Neighbours x) = x
    neighbourNumber _ = 0


    makeSafeMove :: Board -> Board
    makeSafeMove (Board size cells mines status) = do 
                                                    if (not( hasNeighbours cells) ) then makeMove (Command 'r' (0, 0)) (Board size cells mines status)
                                                    else do
                                                        let pos = linearToTuple (basic cells (10, 10) 0) (10, 10)
                                                        makeMove (Command 'r' pos) (Board size cells mines status)

    -- checks to see that the board has not been revealed
        -- input:
        -- list of cells
        -- ouput:
        -- true if there are any neighbours
    hasNeighbours :: [Cell] -> Bool
    hasNeighbours [] = False
    hasNeighbours [x] = if (isNeighbour x) then True
                        else False
    hasNeighbours (x:xs) = if (isNeighbour x) then True
                        else hasNeighbours xs

    -- finds a safe position to reveal
        -- input:
        -- list of cells
        -- output:
        -- a position to reveal or a -1, -1 for no safe move found
    findSafeCoords :: [Cell] -> Int -> (Int, Int)
    findSafeCoords [] _ = (-1,-1)
    findSafeCoords [x] count = if x == Neighbours 1 then linearToTuple count (10, 10) 
                                  else findSafeCoords [] (count+1)
    findSafeCoords (x:y:xs) count = if x == Neighbours 1 && y == Neighbours 1 then linearToTuple (count+10) (10, 10) 
                                  else findSafeCoords xs (count+1)
