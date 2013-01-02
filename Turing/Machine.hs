module Turing.Machine where

import Prelude hiding ( Either, Left, Right )

import Data.List ( intersperse )

import Data.Map ( Map )
import qualified Data.Map as Map

makeError str strings = error $ str ++ ": " ++ (concat $ intersperse ", " $ strings)
dispMap :: (Show a, Show b) => Map a b -> String
dispMap m = concat $ intersperse "\n" $ map (\(k,v) -> (show k) ++ ": " ++ (show v)) $ Map.toList m

data TuringMachine = TM Program State Tape
data Program = Program (Map State Instruction)
data Instruction = Instruction (Map Symbol Delta) deriving (Eq, Ord)

instance Show Instruction where
  show (Instruction instr) = dispMap instr
instance Show Program where
  show (Program prog) = dispMap prog

type Delta = (Action, State)
data Action = Erase        
            | Write Symbol
            | Left
            | Right 
            | Halt 
              deriving (Eq, Ord)
                       
instance Show Action where                       
  show Erase = "E"
  show (Write sym) = "W(" ++ (show sym) ++ ")"
  show Left = "L"
  show Right = "R"
  show Halt = "H"
              
data State = State Label | H deriving (Eq, Ord)
type Label = String
instance Show State where
  show (State s) = s
  show H = "H"

type Tape = ([Symbol], Symbol, [Symbol])
data Symbol = Symbol String | B deriving (Eq, Ord)
instance Show Symbol where
  show (Symbol s) = s
  show B = "B"
  
-- | An alternative used to have B show up as something other than an empty string
disp :: Symbol -> String
disp (Symbol s) = s
disp B = "" 

currentDelta :: TuringMachine -> Maybe Delta
currentDelta (TM (Program stateInstructionMap) state (_, curr, _)) = 
  if state == H 
  then Nothing
  else case Map.lookup state stateInstructionMap of
    Nothing -> makeError "No instruction found for state" [show state]
    Just (Instruction instruction) -> case Map.lookup curr instruction of 
      Nothing -> makeError "No instruction found for state/symbol pair" [show state, disp curr]
      delta -> delta
      
applyDelta :: Delta -> TuringMachine -> TuringMachine
applyDelta (action, nextState) tm@(TM prog _ (leftTape, curr, rightTape)) = 
  TM prog nextState $ case action of 
    Erase -> (leftTape, B, rightTape)
    Write s -> (leftTape, s, rightTape)
    Left -> case leftTape of 
      (l:ls) -> (ls, l, curr : rightTape)
      [] -> ([], B, curr : rightTape) -- error "At the left end of tape, can't go left"
    Right -> case rightTape of
      (r:rs) -> (curr : leftTape, r, rs)
      [] -> (curr : leftTape, B, []) -- error "At the right end of tape, can't go right"
    Halt -> (leftTape, curr, rightTape)

initialize :: Program -> State -> [Symbol] -> TuringMachine
initialize prog startState startTape = 
  TM prog startState $ case startTape of 
    [] -> ([], B, [])
    (sym:syms) -> ([], sym, syms)
    
step :: TuringMachine -> Maybe TuringMachine
step tm = do d <- currentDelta tm
             return $ applyDelta d tm

showApplicableRule :: TuringMachine -> Maybe String
showApplicableRule tm@(TM _ state (_, curr, _)) = fmap showRule $ currentDelta tm                                                  
  where showRule (action, nextState) = (show state) ++ ": " ++ (show curr) ++ " -> " ++ (show action) ++ "; " ++ (show nextState)

showCurrentTape :: TuringMachine -> String
showCurrentTape tm@(TM _ _ (leftTape, curr, rightTape)) = 
  concat $ intersperse " " $ map show ((reverse leftTape) ++ (curr : rightTape))

stepIO :: TuringMachine -> IO (Maybe TuringMachine)
stepIO tm@(TM prog@(Program fetch) state (leftTape, curr, rightTape)) = 
  do case showApplicableRule tm of
       Nothing -> return ()
       Just ruleStr -> do putStrLn ruleStr 
                          putStrLn $ showCurrentTape tm
     return $ step tm

