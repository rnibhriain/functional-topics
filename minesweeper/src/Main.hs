{-# LANGUAGE ScopedTypeVariables #-}

module Main where

    import qualified Graphics.UI.Threepenny as UI
    import Graphics.UI.Threepenny.Core
    import Reactive.Threepenny
    import Board
    import Game
    import Player
    import           Data.IORef
    import Data.List.Split
    import GHC.Float.RealFracMethods (roundDoubleInt)
    import System.Random
    
    main :: IO ()
    main = do
        gen <- newStdGen
        let randomNumbers = randomRs (0, 19) gen :: [Int]
        let board = initialiseBoard (10, 10) 10 7  -- Basic difficulty level
        startGUI defaultConfig (setup board randomNumbers)

    data Mode = MARKING | OPENING

    canvasSize = 1000

    size :: Int
    size = 10

    setup :: Board -> [Int] -> Window -> UI ()
    setup game randNums window = do

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

        currentGame     <- liftIO $ newIORef game
        currentMode     <- liftIO $ newIORef OPENING
        currentCount    <- liftIO $ newIORef 0

        on UI.click revealmode $ \_ -> do
            liftIO $ writeIORef currentMode OPENING

        on UI.click flagmode $ \_ -> do
            liftIO $ writeIORef currentMode MARKING

        on UI.click playmode $ \_ -> do
            current <- liftIO $ readIORef currentGame
            let latestGame = makeSafeMove current
            liftIO $ writeIORef currentGame latestGame
            drawBoard latestGame canvas

        -- restarts the game with the bombs in different positions
        on UI.click clear $ \_ -> do
            canvas # UI.clearCanvas
            current <- liftIO $ readIORef currentGame
            count <- liftIO $ readIORef currentCount
            let ranNum = randNums !! count
            let latestGame = initialiseBoard (10, 10) 10 ranNum
            liftIO $ writeIORef currentGame latestGame  
            liftIO $ writeIORef currentCount (count+1)
            do
                drawBoard latestGame canvas

        -- either reveals or marks a cell
        on UI.mousedown canvas $ \(x, y) -> do
            mode <- liftIO $ readIORef currentMode
            case mode of
                OPENING -> do
                    current     <- liftIO $ readIORef currentGame
                    let move = Command 'r'  (  roundDoubleInt y `div` (canvasSize `div` size) , roundDoubleInt x `div` (canvasSize `div` size) )
                    if isGameFinished move current
                    then do
                        let latestGame = explodedBomb current
                        liftIO $ writeIORef currentGame latestGame
                        do
                            drawBoard latestGame canvas
                    else do
                        let latestGame = makeMove move current
                        liftIO $ writeIORef currentGame latestGame
                        do
                            drawBoard latestGame canvas
                MARKING -> do
                    current     <- liftIO $ readIORef currentGame
                    let move = Command 'f'  (  roundDoubleInt y `div` (canvasSize `div` size) , roundDoubleInt x `div` (canvasSize `div` size) )
                    let latestGame = makeMove move current
                    liftIO $ writeIORef currentGame latestGame
                    do
                        drawBoard latestGame canvas

    -- darws all the cells on the canvas and a win or lose sign if the game is over
    drawBoard :: Board -> Element -> UI ()
    drawBoard (Board (x, y) grid bombs status) canvas = do
        if status then do
            canvas # set' UI.fillStyle (UI.htmlColor "black")
            canvas # UI.fillRect (200, 275) 400 200
            canvas # set' UI.fillStyle (UI.htmlColor "white")
            canvas # set' UI.textAlign UI.Center
            canvas # set' UI.textFont "52px sans-serif"
            canvas # UI.fillText "LOSE" (400, 400)
        else if checkWin (Board (x, y) grid bombs status) then do
            canvas # set' UI.fillStyle (UI.htmlColor "black")
            canvas # UI.fillRect (200, 275) 400 200
            canvas # set' UI.fillStyle (UI.htmlColor "white")
            canvas # set' UI.textAlign UI.Center
            canvas # set' UI.textFont "52px sans-serif"
            canvas # UI.fillText "WIN" (400, 400)
        else drawGrid grid 0 canvas

    -- draws the board when the user has lost
    drawGridEnd :: [Cell] -> Int -> Element -> UI ()
    drawGridEnd [x] num canvas = drawBoardSquare (printingLostGame x) (linearToTuple num (10, 10)) canvas
    drawGridEnd (x:xs) num canvas = do 
                                    drawBoardSquare (printingLostGame x) (linearToTuple num (10, 10)) canvas
                                    drawGrid xs (num+1) canvas

    -- draws the current board
    drawGrid :: [Cell] -> Int -> Element -> UI ()
    drawGrid [x] num canvas = drawBoardSquare (convertCells x) (linearToTuple num (10, 10)) canvas
    drawGrid (x:xs) num canvas = do 
                                    drawBoardSquare (convertCells x) (linearToTuple num (10, 10)) canvas
                                    drawGrid xs (num+1) canvas

    -- draws a single cell depending on its content
    drawBoardSquare :: Char -> (Int, Int) -> Element -> UI ()
    drawBoardSquare 'x' (i, j) canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "red")
        canvas # UI.fillRect
            ( fromIntegral (j * (canvasSize `div` size) + 3)
            , fromIntegral (i * (canvasSize `div` size) + 3)
            )
            (fromIntegral (canvasSize `div` size - 6))
            (fromIntegral (canvasSize `div` size - 6))
    drawBoardSquare '-' (i, j) canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "gray")
        canvas # UI.fillRect
            ( fromIntegral (j * (canvasSize `div` size) + 3)
            , fromIntegral (i * (canvasSize `div` size) + 3)
            )
            (fromIntegral (canvasSize `div` size - 6))
            (fromIntegral (canvasSize `div` size - 6))
    drawBoardSquare 'f' (i, j) canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "blue")
        canvas # UI.fillRect
            ( fromIntegral (j * (canvasSize `div` size) + 3)
            , fromIntegral (i * (canvasSize `div` size) + 3)
            )
            (fromIntegral (canvasSize `div` size - 6))
            (fromIntegral (canvasSize `div` size - 6))
    drawBoardSquare 'e' (i, j) canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "green")
        canvas # UI.fillRect
            ( fromIntegral (j * (canvasSize `div` size) + 3)
            , fromIntegral (i * (canvasSize `div` size) + 3)
            )
            (fromIntegral (canvasSize `div` size - 6))
            (fromIntegral (canvasSize `div` size - 6))
    drawBoardSquare '0' (i, j) canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "white")
        canvas # UI.fillRect
            ( fromIntegral (j * (canvasSize `div` size) + 3)
            , fromIntegral (i * (canvasSize `div` size) + 3)
            )
            (fromIntegral (canvasSize `div` size - 6))
            (fromIntegral (canvasSize `div` size - 6))
    drawBoardSquare ch (i, j) canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "white")
        canvas # UI.fillRect
            ( fromIntegral (j * (canvasSize `div` size) + 3)
            , fromIntegral (i * (canvasSize `div` size) + 3)
            )
            (fromIntegral (canvasSize `div` size - 6))
            (fromIntegral (canvasSize `div` size - 6))
        canvas # set' UI.textAlign UI.Center
        canvas # set' UI.textFont "24px sans-serif"
        canvas # set' UI.fillStyle (UI.htmlColor "black")
        canvas # UI.fillText
            (show ch)
            ( fromIntegral
            ((j * (canvasSize `div` size)) + ((canvasSize `div` size) `div` 2))
            , fromIntegral
            ((i * (canvasSize `div` size)) + ((canvasSize `div` size) - 10))
            )