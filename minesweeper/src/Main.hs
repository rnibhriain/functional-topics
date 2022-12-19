module Main where

    import Board
    import Game


    main:: IO()
    main = do
        putStrLn "Enter a command in the form of:\n[action] / [row no.] / [col no.]\nwhere actions are:\n'r' for reveal\n'f' for flag "
        game (initialiseBoard (10, 10) 10)  -- this is the basic difficulty level in Minesweeper