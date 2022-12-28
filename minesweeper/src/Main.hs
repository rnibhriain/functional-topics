{-# LANGUAGE ScopedTypeVariables #-}


module Main where



    import qualified Graphics.UI.Threepenny as UI
    import Graphics.UI.Threepenny.Core
    import Reactive.Threepenny
    import Board
    import Game
    import           Data.IORef
    import Data.List.Split
    import GHC.Float.RealFracMethods (roundDoubleInt)
    
    main :: IO ()
    main = do
        let board = initialiseBoard (10, 10) 10  -- this is the basic difficulty level in Minesweeper
        startGUI defaultConfig (setup board)


   -- main:: IO()
    --main = do
    --    putStrLn "Enter a command:\n[action] [row no.] [col no.]\nwhere actions are:\n'c' for clear\n'f' for flag "
        
        --game (initialiseBoard (10, 10) 10)  -- this is the basic difficulty level in 
    

    canvasSize = 1000

    siz :: Double
    siz = 10.0

    size :: Int
    size = 10.0

    setup :: Board -> Window -> UI ()
    setup game window = do

        return window # set title "Minesweeper Game"

        canvas <- UI.canvas # set UI.width canvasSize # set UI.height canvasSize # set
            UI.style
            [("border", "solid black 1px"), ("background", "#000")]

        flagmode    <- UI.button #+ [string "Flag"]
        revealmode  <- UI.button #+ [string "Reveal"]
        playmode    <- UI.button #+ [string "Play"]
        clear       <- UI.button #+ [string "Reset"]

        drawBoard game canvas

        getBody window
            #+ [ column [element canvas],
                element flagmode,
                element revealmode,
                element playmode, 
                element clear
                ]

        currentGame     <- liftIO $ newIORef (game)
        currentMode     <- liftIO $ newIORef OPENING
        currentCount    <- liftIO $ newIORef 1

        on UI.click revealmode $ \_ -> do
            liftIO $ writeIORef currentMode OPENING

        on UI.click flagmode $ \_ -> do
            liftIO $ writeIORef currentMode MARKING

       -- on UI.click playmode $ \_ -> do
        --    current <- liftIO $ readIORef currentGame
        --    let latestGame = makeMove current
        --    liftIO $ writeIORef currentGame latestGame
        --    do
        --        drawBoard latestGame canvas

        on UI.click clear $ \_ -> do
            canvas # UI.clearCanvas
            current <- liftIO $ readIORef currentGame
            count <- liftIO $ readIORef currentCount
            let latestGame = initialiseBoard (10, 10) 10
            liftIO $ writeIORef currentGame latestGame
            liftIO $ writeIORef currentCount (count+1)
            do
                drawBoard latestGame canvas

        on UI.mousedown canvas $ \(x, y) -> do
            mode <- liftIO $ readIORef currentMode
            case mode of
                OPENING -> do
                    current     <- liftIO $ readIORef currentGame
                    let move = Command 'e'  ( roundDoubleInt (y `div` (canvasSize `div` size)) , roundDoubleInt (x `div` (canvasSize `div` size)) )
                    let latestGame = makeMove current move 
                    liftIO $ writeIORef currentGame latestGame
                    do
                        drawBoard latestGame canvas
                MARKING -> do
                    current     <- liftIO $ readIORef currentGame
                    let move = Command 'f'  ( roundDoubleInt (y `div` (canvasSize `div` size)) , roundDoubleInt (x `div` (canvasSize `div` size)) )
                    let latestGame = makeMove current move
                    liftIO $ writeIORef currentGame latestGame
                    do
                        drawBoard latestGame canvas

    drawBoard :: Board -> Element -> UI ()
    drawBoard (Board (x, y) grid bombs) canvas = do
        if isGameFinished (Board (x, y) grid bombs) then do
            canvas # set' UI.fillStyle (UI.htmlColor "black")
            canvas # UI.fillRect (200, 275) 400 200
            canvas # set' UI.fillStyle (UI.htmlColor "white")
            canvas # set' UI.textAlign (UI.Center)
            canvas # set' UI.textFont "52px sans-serif"
            canvas # UI.fillText "LOSE" (400, 400)
        else if checkWin (Board (x, y) grid bombs) then do
            canvas # set' UI.fillStyle (UI.htmlColor "black")
            canvas # UI.fillRect (200, 275) 400 200
            canvas # set' UI.fillStyle (UI.htmlColor "white")
            canvas # set' UI.textAlign (UI.Center)
            canvas # set' UI.textFont "52px sans-serif"
            canvas # UI.fillText "WIN" (400, 400)
        else mapM_ drawBoardSquare (splitEvery x (map printingReplacements grid)) canvas
        return () 
    --V.forM mapM_ putStrLn (splitEvery x (map printingReplacements grid))
    --row (\square -> drawBoardSquare (printingReplacements square) canvas)

    drawBoardSquare :: Char -> (Int, Int) -> Element -> UI ()
    drawBoardSquare 'x' (i, j) canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "red")
        canvas # UI.fillRect
            ( fromIntegral ((j * (canvasSize `div` size) + 3))
            , fromIntegral ((i * (canvasSize `div` size) + 3))
            )
            (fromIntegral (canvasSize `div` size - 6))
            (fromIntegral (canvasSize `div` size - 6))
    drawBoardSquare '-' (i, j) canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "gray")
        canvas # UI.fillRect
            ( fromIntegral ((j * (canvasSize `div` size) + 3))
            , fromIntegral ((i * (canvasSize `div` size) + 3))
            )
            (fromIntegral (canvasSize `div` size - 6))
            (fromIntegral (canvasSize `div` size - 6))
    drawBoardSquare 'f' (i, j) canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "blue")
        canvas # UI.fillRect
            ( fromIntegral ((j * (canvasSize `div` size) + 3))
            , fromIntegral ((i * (canvasSize `div` size) + 3))
            )
            (fromIntegral (canvasSize `div` size - 6))
            (fromIntegral (canvasSize `div` size - 6))
    drawBoardSquare 'e' (i, j) canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "green")
        canvas # UI.fillRect
            ( fromIntegral ((j * (canvasSize `div` size) + 3))
            , fromIntegral ((i * (canvasSize `div` size) + 3))
            )
            (fromIntegral (canvasSize `div` size - 6))
            (fromIntegral (canvasSize `div` size - 6))
    drawBoardSquare ch (i, j) canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "white")
        canvas # UI.fillRect
            ( fromIntegral ((j * (canvasSize `div` size) + 3))
            , fromIntegral ((i * (canvasSize `div` size) + 3))
            )
            (fromIntegral (canvasSize `div` size - 6))
            (fromIntegral (canvasSize `div` size - 6))
        canvas # set' UI.textAlign (UI.Center)
        canvas # set' UI.textFont "24px sans-serif"
        canvas # UI.fillText
            (show ch)
            ( fromIntegral
            ((j * (canvasSize `div` size)) + ((canvasSize `div` size) `div` 2))
            , fromIntegral
            ((i * (canvasSize `div` size)) + ((canvasSize `div` size) - 10))
            )
