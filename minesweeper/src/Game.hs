module Game where

    import Data.List.Split
    import Board

    game :: Board -> IO()
    game board = do
        prettyPrintBoard board
        putStrLn "Next Move"
        --if True board
          --  then
            --    putStrLn "You Win!!"
            --else do
              --  putStrLn "Next Move"
                --command <- getLine
                --let move = resolveMove (commandToMove command) board
                --if endsGame move board
                --    then
                --        gameOver board
                --    else game (makeMove move board)

    prettyPrintBoard :: Board -> IO()
    prettyPrintBoard (Board (x, _) grid _) = mapM_ putStrLn (splitEvery x (map printingReplacements grid))

    printingReplacements:: Cell -> Char
    printingReplacements Empty = '-'
    printingReplacements Bomb = 'x'