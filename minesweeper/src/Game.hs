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
                    then
                        endGame board
                    else putStrLn "Move entered\n"
                    --game (makeMove move board)

    isGameFinished :: Command -> Board -> Bool
    isGameFinished (Command 'e' pos) board = checkForMine board (Command 'e' pos) 
    isGameFinished (Command _ _) _ = False

    convertCommand :: String -> Command
    convertCommand cmd = Command move (x, y)
        where 
            move = head cmd
            pos = drop 2 cmd
            x = digitToInt (pos !! 0)
            y = digitToInt (pos !! 2)

    endGame:: Board -> IO()
    endGame (Board (x, _) grid _) = mapM_ putStrLn (splitEvery x (map printingReplacements grid))

    checkForMine :: Board -> Command -> Bool
    checkForMine (Board _ cells _ ) (Command _ (x, y))  | cells !! ((x*10)+y) == Bomb = True
                                            | otherwise = False

    printBoard :: Board -> IO()
    printBoard (Board (x, _) grid _) = mapM_ putStrLn (splitEvery x (map printingReplacements grid))

    printingReplacements:: Cell -> Char
    printingReplacements Empty = '-'
    printingReplacements Bomb = 'x'
    printingReplacements FlagBomb = 'f'
    printingReplacements (Neighbours 0) = ' '
    printingReplacements (Neighbours num) = chr num

    printingLostGame:: Cell -> Char
    printingLostGame Empty = '-'
    printingLostGame Bomb = 'x'
    printingLostGame FlagBomb = '-'
    printingLostGame (Neighbours num) = '-'