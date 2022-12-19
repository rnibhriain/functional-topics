{-# Language ScopedTypeVariables #-}
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Reactive.Threepenny

canvasSize = 400

data Modes = Beginner | Intermediate deriving Show


--main :: IO ()
--main = do
--  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
  return window # set title "Clickable canvas"

  canvas <- UI.canvas
    # set UI.height canvasSize
    # set UI.width canvasSize
    # set UI.style [("border", "solid black 1px"), ("background", "#eee")]

  beginnerMode  <- UI.button #+ [string "Flag Empty"]
  intermediateMode <- UI.button #+ [string "Flag Clear"]
  clear     <- UI.button #+ [string "Reset"]

  getBody window #+
    [column [element canvas]
    , element beginnerMode, element intermediateMode, element expertMode, element clear]


  let -- type declarations in let clauses are allowed by the ScopedTypeVariables extension
      efs :: Event (Modes -> Modes)
      efs = const Beginner <$ UI.click beginnerMode

      ehs :: Event (Modes -> Modes)
      ehs = const Intermediate <$ UI.click intermediateMode

      modeEvent :: Event (Modes -> Modes)
      modeEvent = unionWith const efs ehs

  drawingMode <- accumB Beginner modeEvent
  mousePos <- stepper (0,0) $ UI.mousemove canvas
  let bst :: Behavior (Modes, (Double,Double))
      bst = (,) <$> drawingMode <*> mousePos

      eDraw :: Event (Modes, (Double,Double))
      eDraw = bst <@ UI.click canvas

  onEvent eDraw $ \e -> do drawShape e canvas

  on UI.click clear $ const $
    canvas # UI.clearCanvas

drawShape :: (Modes, (Double,Double)) -> Element -> UI ()
drawShape (Beginner, (x,y)) canvas = do
  canvas # set' UI.fillStyle   (UI.htmlColor "red")
  canvas # UI.fillRect (x,y) 75 75
drawShape (Intermediate, (x,y)) canvas = do
  canvas # set' UI.fillStyle   (UI.htmlColor "pink")
  canvas # UI.fillRect (x,y) 75 75


