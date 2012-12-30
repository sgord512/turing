module Turing.Machine where

import Prelude hiding ( Either, Left, Right )

data Symbol = Symbol String | B deriving (Eq, Ord)

data Action = Erase        
            | Write Symbol
            | Left
            | Right 
            | Halt 
              deriving Eq
              
data Instruction = Instruction (Symbol -> (Action, State))              

type Label = String

data State = State Label deriving (Eq, Ord)

data Program = Program (State -> Instruction)

type Tape = ([Symbol], Symbol, [Symbol])

data TuringMachine = TM Program State Tape

step :: TuringMachine -> Maybe TuringMachine
step (TM (Program fetch) state (leftTape, curr, rightTape)) = 
  if (action == Halt) 
  then Nothing 
  else Just $ TM (Program fetch) nextState $ case action of 
    Erase -> (leftTape, B, rightTape)
    Write s -> (leftTape, s, rightTape)
    Left -> case leftTape of 
      (l:ls) -> (ls, l, curr : rightTape)
      [] -> error "At the left end of tape, can't go left"
    Right -> case rightTape of
      (r:rs) -> (curr : leftTape, r, rs)
      [] -> error "At the right end of tape, can't go right"
    Halt -> error "Shouldn't step on a halt instruction"
  where (Instruction instr) = fetch state
        (action, nextState) = instr curr