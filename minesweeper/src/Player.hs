module Player where

    import Game
    import Board


    makeSafeMove :: Board -> Board
    makeSafeMove (Board size cells mines status) = do 
                                                    if (not( hasNeighbours cells) ) then makeMove (Command 'r' (0, 0)) (Board size cells mines status)
                                                    else do
                                                        let (x, y) = findSafeCoords cells
                                                        if (x == -1) then (Board size cells mines status)
                                                        else (makeMove (Command 'r' (x, y)) (Board size cells mines status))

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
    findSafeCoords :: [Cell] -> (Int, Int)
    findSafeCoords [] = (-1,-1)
    findSafeCoords [x] = (0,0)
    findSafeCoords (x:xs) = findSafeCoords xs
