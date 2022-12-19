module Main where

    import Board
    import Game


    main:: IO()
    main = do
        putStrLn "Enter a command:\n[action] [row no.] [col no.]\nwhere actions are:\n'c' for clear\n'f' for flag "
        game (initialiseBoard (10, 10) 10)  -- this is the basic difficulty level in Minesweeper