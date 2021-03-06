module Game_logic where

import Types
import Config
import Data.List
import System.IO.Unsafe
import Prelude
import Control.Exception

-- | creating list of alive cells from file
fillCellList :: AliveCells
fillCellList = unsafePerformIO (fH `catch` handler)

-- | handle exception in reading
handler :: IOException -> IO (AliveCells)
handler _ = return ([(1, 0), (2, 1), (0, 2), (1, 2), (2, 2), (18, 0), (18, 1), (18, 2)])

-- | Read from file.in
fH :: IO(AliveCells)
fH = do
        src <- readFile "gun.in"
        return (operate (charToInt src 0 False))

-- | make AliveCells from list of Int, make (x, y) from first two elems of [Int] and put in list
operate :: [Int] -> AliveCells
operate [] = []
operate (_:[]) = []
operate (x:y:xs) = (x, y):(operate xs)

-- | changing string into list of int
charToInt :: String -> Int -> Bool -> [Int]
charToInt [] s _ = [s]
charToInt (x:xs) s b
                | (x == ' ') && (b == False) = charToInt xs 0 False
                | (x == ' ') =  s:(charToInt xs 0 False)
                | (x == '\n') && (b == False) = charToInt xs 0 False
                | (x == '\n') =  s:(charToInt xs 0 False)
                | otherwise = charToInt xs (s * 10 + ((fromEnum x) - (fromEnum '0'))) True

-- | proc cells for next iteration
lifeStep :: AliveCells -> AliveCells
lifeStep cells = [head g | g <- makeSurround cells, checkSurround g cells]

-- | if elem have 3 border cells - cell is alive, 
checkSurround :: AliveCells -> AliveCells -> Bool
checkSurround [_, _, _] _ = True
checkSurround [x, _] cells = x `elem` cells
checkSurround _ _ = False

-- | make list of cells from AliveCells for next processing
makeSurround :: AliveCells -> [AliveCells]
makeSurround = group . sort . concatMap surrounding

-- | create list of surrounding cells
surrounding :: (Int, Int) -> AliveCells
surrounding (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0), x+dx <= (rowNum - 1), x+dx >= 0, y+dy <= (colNum - 1), y+dy >= 0]

