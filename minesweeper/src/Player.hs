module Player where

    import Game
    import Board

    -- returns board with hidden details
        -- input:
        -- current board
        -- output:
        -- board with hidden details
    removeDetails :: Board -> Board
    removeDetails (Board size cells mines status) = Board size (exchangeDetails cells 0) mines status

    -- changes a cell to secrets
        -- input:
        -- cells
        -- position
        -- output:
        -- hidden grid
    exchangeDetails :: [Cell] -> Int -> [Cell]
    exchangeDetails cells x = 
                    let (head, t:tail) = splitAt x cells in
                        if (t == Bomb) || t == Empty then
                            head ++ [Unknown] ++ tail
                        else if (t == FlagEmpty) || t == FlagBomb then
                            head ++ [Flag] ++ tail
                        else cells

    -- finds the cell to reveal
        -- input:
        -- cell position to check
        -- cell value
        -- unrevealed neighbours
        -- neighbours that are flagged
        -- cells
        -- dimensions of board
        -- output:
        -- position to reveal
    cell :: Int -> Int -> [Int] -> [Int] -> [Cell] -> (Int, Int) -> Int
    cell cellpos cellVal unrevealedNeighbours flaggedNeighbours grid dim | length flaggedNeighbours == cellVal && length unrevealedNeighbours > 0 = head unrevealedNeighbours
                                                                | length flaggedNeighbours == cellVal = basic grid dim (cellpos + 1) 
                                                                | (length unrevealedNeighbours + length flaggedNeighbours) == cellVal = head unrevealedNeighbours
                                                                | otherwise = basic grid dim (cellpos + 1) 

    -- finds positions of flagged cells around the position
        -- input:
        -- current position
        -- cells
        -- dimensions of board
        -- output:
        -- list of positions
    findSurroundingFlagged :: Int -> [Cell] -> (Int, Int) -> [Int]
    findSurroundingFlagged coord grid dim = filter (\n -> (grid !! n  == FlagEmpty || grid !! n  == FlagBomb)) (findNeighbours (linearToTuple coord dim))

    -- a naive solving algorithm, should solve most basic grids i.e. 10x10 grid with 10 mines
        -- input:
        -- cells
        -- dimensions of board
        -- current position
        -- output:
        -- position to reveal
    basic :: [Cell] -> (Int, Int) -> Int -> Int
    basic  grid dim i | i == ((length grid) -1 ) = 0
                    | grid !! i == FlagBomb = basic  grid dim (i + 1)
                    | grid !! i == FlagEmpty = basic  grid dim (i + 1)                  
                    | neighbourNumber (grid !! i) >= 1 = cell i (neighbourNumber (grid !! i)) (findNeighbs grid (findNeighbours (linearToTuple i dim)) []) (findSurroundingFlagged i grid dim) grid dim            
                    | otherwise = basic  grid dim (i + 1)  

    -- Updated function - doesn't work
    --makeSafeMove :: Board -> Board
    --makeSafeMove (Board size cells mines status) = do 
    --                                                let (Board dims grid bombs current) = removeDetails (Board size cells mines status)
    --                                                if (not( hasNeighbours grid) ) then makeMove (Command 'r' (0, 0)) (Board size cells mines status)
    --                                                else do
    --                                                    let posToCheck = findSquareToCheck grid 0
    --                                                    let corner = findCornerPattern grid posToCheck
    --                                                    if (head corner == -1 || corner == []) then do
    --                                                         let square = findOneToOne grid posToCheck
    --                                                         if (square == -1) then makeMove (Command 'r' (0,0)) (Board size cells mines status)
    --                                                         else make makeMove (Command 'r' square) (Board size cells mines status)
    --                                                    else makeMove (Command 'r' head corner) (Board size cells mines status)


    -- finds positions of Neighbours 1 to check for patterns described in report
        -- input:
        -- cells
        -- current index
        -- output:
        -- position of current cell
    findSquareToCheck :: [Cell] -> Int -> Int
    findSquareToCheck [] _ = -1
    findSquareToCheck [x] pos | x == Neighbours 1 = pos
                                               | otherwise = -1
    findSquareToCheck (x:xs) pos | x == Neighbours 1 = pos
                                                 | otherwise = findSquareToCheck xs (pos+1)

    --findCornerPattern :: [Cell] -> Int -> [Int]
    --findCornerPattern cells pos  | (cells !! pos-10) == Neighbours 1 && (cells !! pos-1) == Neighbours 1  && (cells !! pos-11) == Unknown + all other squares equal neighbours = 
    --                                do 
    --                                let list = []
    --                                if 
                                                                              -- List of positions and then get the head 
                                             -- | other corners
                                             -- | special cases - edges
    --                                         | otherwise = [-1]

    -- do two diff ones for above + below + edge cases 
    --findOneToOne :: [Cell] -> Int -> Int
    --findOneToOne cells pos | (cells !! pos+1) == Neighbours 1 && (cells !! pos + 2) == Neighbours 1 = pos-9

    -- gets the number of bomb neighbours of a cell
        -- input:
        -- cell
        -- output
        -- integer number of bomb neighbours
    neighbourNumber :: Cell -> Int
    neighbourNumber (Neighbours x) = x
    neighbourNumber _ = 0


    -- makes a move for the player if there is one available
        -- input:
        -- current board
        -- output:
        -- board after a safe move
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
