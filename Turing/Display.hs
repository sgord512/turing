module Turing.Display where

import Prelude hiding ( Either, Left, Right )

import Data.Map ( Map )
import qualified Data.Map as Map

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.IO.Simulate

import Turing.Machine
dimensions :: (Int, Int)
dimensions = (1600, 600)
fDimensions = (\(w, h) -> (fromIntegral w, fromIntegral h)) $ dimensions
topLeftCorner = (200, 200)

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

radii :: (Float, Float)
radii = mapPair (\x -> x / 2.0) (\x -> x / 2.0) fDimensions

nw = mapPair (0 -) id radii
ne = mapPair id id radii
sw = mapPair (0 -) (0 -) radii
se = mapPair id (0 -) radii

unit = 40
fUnit = fromIntegral unit
spacing = 4
fSpacing = fromIntegral spacing
squareSide = unit - spacing
fSquareSide = fromIntegral squareSide

tapeHeight = (fromIntegral $ snd dimensions) / 2 - fUnit

scaleSymbol = scale 0.1 0.1
scaleState = scale 0.1 0.1
stepsPerSecond = 1

displayAction :: Action -> Picture
displayAction Erase = blankSquare
displayAction (Write s) = displayTapeCell s
displayAction Left = color magenta $ Polygon [(0.0, fSquareSide / 2), (fSquareSide, fSquareSide), (fSquareSide, 0.0)]
displayAction Right = color magenta $ Polygon [(0.0, 0.0), (0.0, fSquareSide), (fSquareSide, fSquareSide / 2)]
displayAction Halt = color red $ Pictures $ [Polygon [(0.0, fSpacing), (fSquareSide - fSpacing, fSquareSide), (fSquareSide, fSquareSide - fSpacing), (fSpacing, 0.0)],
                                             Polygon [(0.0, fSquareSide - fSpacing), (fSpacing, fSquareSide), (fSquareSide, fSpacing), (fSquareSide - fSpacing, 0.0)]]

displayTable :: Program -> Picture
displayTable (Program prog) = undefined 
  where states = Map.keys prog
        (Instruction instr) = head $ Map.elems prog
        symbols = Map.keys instr
        numRows = length states


rect :: (Float, Float) -> (Float, Float) -> Picture
rect (x,y) (w,h) = Polygon [(x,y), (x + w, y), (x + w, y + h), (x, y + h)]

tapeSquare :: (Float, Float) -> Picture
tapeSquare (x, y) = color white $ rect (x, y) (fSquareSide, fSquareSide)

blankSquare :: Picture 
blankSquare = tapeSquare (0.0, 0.0)

middleSquareCorner :: (Float, Float)
middleSquareCorner = (0 - fUnit / 2.0, tapeHeight)
        
displayTape (leftTape, curr, rightTape) = Pictures $ displayLeft ++ (displayCurr : displayRight)
  where displaySquare (sym, n) = let cornerTranslation = ((fst middleSquareCorner) + fUnit * n, tapeHeight)
                                 in uncurry translate cornerTranslation $ displayTapeCell sym
        displayCurr = displaySquare (curr, 0)
        displayLeft = map (\(sym, offset) -> displaySquare (sym, negate offset)) $ zip leftTape [1..]
        displayRight = map (\(sym, offset) -> displaySquare (sym, offset)) $ zip rightTape [1..]
        
displayTapeCell :: Symbol -> Picture        
displayTapeCell sym = Pictures [blankSquare, translate fSpacing fSpacing $ color (dark violet) $ scaleSymbol $ text $ disp sym]

visibleTape :: Tape -> Tape
visibleTape (leftTape, curr, rightTape) = (take squaresLeft $ leftTape ++ (repeat B), curr, take squaresRight $ rightTape ++ (repeat B))
  where squaresRight = (((fst dimensions) - (floor $ fst middleSquareCorner)) `div` unit) - 1
        squaresLeft = squaresRight + 2

displaySettings :: Display  
-- displaySettings = FullScreen (1600, 1000)
displaySettings = InWindow "Turing Machine Simulator" dimensions (0,0)

displayState :: State -> Picture
displayState state = Pictures [color aquamarine $ Polygon [bottomLeft, bottomRight, topMiddle], uncurry translate textTranslate $ color rose $ scaleState $ text $ show state]
  where (x, y) = middleSquareCorner
        bottomLeft = (x, y - fUnit - fSpacing)
        bottomRight = (x + fUnit, y - fUnit - fSpacing)
        topMiddle = (0, y - fSpacing)
        textTranslate = (x + (fUnit / 2) - fSpacing, y - fUnit + fSpacing)

displayTM :: TuringMachine -> Picture 
displayTM (TM prog state tape) = Pictures [displayTape $ visibleTape tape, displayState state]

stepTM :: ViewPort -> Float -> TuringMachine -> TuringMachine
stepTM _ _ tm = case step tm of 
  Nothing -> tm
  Just tm' -> tm'
  
stepTMIO :: ViewPort -> Float -> TuringMachine -> IO TuringMachine
stepTMIO _ _ tm = do tm' <- stepIO tm
                     return $ case tm' of
                       Nothing -> tm
                       Just rsltTM -> rsltTM

runSimulation :: TuringMachine -> IO ()
runSimulation tm = simulate displaySettings black stepsPerSecond tm displayTM stepTM

runDebugSimulation :: TuringMachine -> IO ()
runDebugSimulation tm = simulateIO displaySettings black stepsPerSecond tm (return . displayTM) stepTMIO