module Game where

    import Data.List.Split
    import Data.Char
    import Board

    -- either 'f' or 'e' and a position
    data Command = Command Char (Int, Int)

    game :: Board -> IO()
    game board = do
        printBoard board
        putStrLn "\nNext Move"
        if checkWin board
            then
                putStrLn "Win!!\n"
            else do
                putStrLn "Enter your next Move\n"
                command <- getLine
                let move = convertCommand command
                if isGameFinished move board
                    then do
                        endGame board
                        putStrLn "You Lose\n"
                    else game (makeMove move board)

    makeMove :: Command -> Board -> Board
    makeMove (Command 'f' pos) board    | checkForMine board (Command 'f' pos) = flag board pos FlagBomb
                                        | otherwise = flag board pos FlagEmpty
    makeMove (Command 'r' pos) board = do  
                                        let currentBoard = placeNeighbours board pos
                                        clearNeighbours currentBoard pos
                                        --openMine board pos
                                    --let currentBoard = placeNeighbours board pos
                                    --clearNeighbours currentBoard pos
    makeMove _ board = board

    isGameFinished :: Command -> Board -> Bool
    isGameFinished (Command 'r' pos) board = checkForMine board (Command 'r' pos) 
    isGameFinished (Command _ _) _ = False

    convertCommand :: String -> Command
    convertCommand cmd = Command move (x, y)
        where 
            move = head cmd
            pos = drop 2 cmd
            x = digitToInt (pos !! 0)
            y = digitToInt (pos !! 2)

    endGame:: Board -> IO()
    endGame (Board (x, _) grid _) = mapM_ putStrLn (splitEvery x (map printingLostGame grid))

    checkForMine :: Board -> Command -> Bool
    checkForMine (Board _ cells _ ) (Command _ (x, y))  | cells !! ((x*10)+y) == Bomb ||  cells !! ((x*10)+y) == FlagBomb = True
                                            | otherwise = False

    -- prints the board
    printBoard :: Board -> IO()
    printBoard (Board (x, _) grid _) = mapM_ putStrLn (splitEvery x (map printingReplacements grid))

    printingReplacements:: Cell -> Char
    printingReplacements Empty = '-'
    printingReplacements Bomb = '-'
    printingReplacements FlagBomb = 'f'
    printingReplacements FlagEmpty = 'e'
    printingReplacements (Neighbours num) = intToDigit num

    printingLostGame:: Cell -> Char
    printingLostGame Bomb = 'x'
    printingLostGame _ = '-'