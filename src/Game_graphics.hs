module Game_graphics where

import Config
import Types
import Game_logic

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- | starting function play
start_life :: IO ()
start_life = play window backgroundColor numSimStep initWorld worldToPic event_handler simIter

-- | create window
window :: Display
window = InWindow
        "Conway life"
        (wWin, hWin)
        (100, 50)

-- | Build init world from extr file or predownload scene
initWorld :: LifeWorld
initWorld = (LifeWorld fillCellList (Control True 10 10) (Control True 10 10) (Menu False False (500, 500)))

-- | make Picture from LifeWorld for play function
worldToPic :: LifeWorld -> Picture
worldToPic world = Pictures (makeScene world)

-- | construct interface, aliveC - list of cells, ctrlMenu - params for bar buttons
makeScene :: LifeWorld -> [Picture]
makeScene (LifeWorld aliveC _ _ _) = (drawField aliveC)

-- | draw field
drawField :: AliveCells -> [Picture]
drawField cells = drawRows rowNum cells

-- | draw rows, Int - number of rows
drawRows :: Int -> AliveCells -> [Picture]
drawRows 0 _ = []
drawRows n cells = (drawC colNum (rowNum - n) cells) ++ (drawRows (n - 1) cells)

-- | draw cells in row, 
drawC :: Int -> Int -> AliveCells -> [Picture]
drawC n rN cells = map (\i -> drawCell i rN cells) [0..n]
      
-- | Draw a single cell
drawCell :: Int -> Int -> AliveCells -> Picture
drawCell n rN cells = Translate dx dy deadOrAliveCell
    where
      dx = fromIntegral (fieldXoff + (colNum - n) * cellXsize)
      dy = fromIntegral (fieldYoff - rN * cellYsize)
      deadOrAliveCell = if isAlive then aCell else dCell
      isAlive = (rN, colNum - n) `elem` cells

-- | draw alive cell with black color
aCell :: Picture
aCell = Pictures
        [ Color white          (rectangleSolid (fromIntegral cellXsize) (fromIntegral cellYsize))
        , Color (greyN 0.65)   (rectangleWire  (fromIntegral cellXsize) (fromIntegral cellYsize))
        ]

-- | draw dead cell with black color
dCell :: Picture
dCell = Pictures
        [ Color black          (rectangleSolid (fromIntegral cellXsize) (fromIntegral cellYsize))
        , Color (greyN 0.65)   (rectangleWire  (fromIntegral cellXsize) (fromIntegral cellYsize))
        ]

-- | handling events
event_handler :: Event -> LifeWorld -> LifeWorld
event_handler _ w = w

-- | Step world one iteration
simIter :: Float -> LifeWorld -> LifeWorld
simIter _ w@LifeWorld{aliveCells = a} =  w{aliveCells = (lifeStep a)}


















