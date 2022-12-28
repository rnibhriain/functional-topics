{-# LANGUAGE ScopedTypeVariables #-}

module Canvas where

  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core
  import Reactive.Threepenny
  import Board
  import Game

  canvasSize = 1000

  data Modes = Flag | Reveal deriving (Show)

  setup :: Window -> UI ()
  setup window = do
    return window # set title "Clickable canvas"
    let board = game (initialiseBoard (10, 10) 10)
    canvas <-
      UI.canvas
        # set UI.height canvasSize
        # set UI.width canvasSize
        # set UI.style [("border", "solid black 1px"), ("background", "#eee")]

    flagmode <- UI.button #+ [string "Flag"]
    revealmode <- UI.button #+ [string "Reveal"]
    clear <- UI.button #+ [string "Reset"]

    getBody window
      #+ [ column [element canvas],
          element flagmode,
          element revealmode,
          element clear
        ]

    let -- type declarations in let clauses are allowed by the ScopedTypeVariables extension
        efs :: Event (Modes -> Modes)
        efs = const Flag <$ UI.click flagmode

        ehs :: Event (Modes -> Modes)
        ehs = const Reveal <$ UI.click revealmode

        modeEvent :: Event (Modes -> Modes)
        modeEvent = unionWith const efs ehs

    drawingMode <- accumB Flag modeEvent
    mousePos <- stepper (0, 0) $ UI.mousemove canvas
    let bst :: Behavior (Modes, (Int, Int))
        bst = (,) <$> drawingMode <*> mousePos

        eDraw :: Event (Modes, (Int, Int))
        eDraw = bst <@ UI.click canvas

    onEvent eDraw $ \e -> do drawShape e canvas

    on UI.click clear $
      const $
        canvas # UI.clearCanvas

  drawShape :: (Modes, (Int, Int)) -> Element -> Board -> UI ()
  drawShape (Flag, (x, y)) canvas = do

    canvas # set' UI.fillStyle (UI.htmlColor "red")
    canvas # UI.fillRect (x, y) 75 75
  drawShape (Reveal, (x, y)) canvas = do
    canvas # set' UI.fillStyle (UI.htmlColor "pink")
    canvas # UI.fillRect (x, y) 75 75
