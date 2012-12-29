module Turing where

data Symbol = B | S1

data Action = Erase        
            | Print Symbol
            | Left
            | Right 
            | Halt
              
data Instruction = Instruction (Symbol -> (Action, State))              

type Label = String

data State = State Label

data Program = Program (State -> Instruction)

type Tape = ([Symbol], Symbol, [Symbol])

data TuringMachine = TM Program State Tape Symbol Tape

step :: TuringMachine -> Maybe TuringMachine
step (TM (Program fetch) state (leftTape, curr, rightTape)) = 
  if (action == Halt) 
  then Nothing 
  else Just $ TM (Program fetch) nextState $ case action of 
    Erase -> (leftTape, B, rightTape)
    Print s -> (leftTape, s, rightTape)
    Left -> case leftTape of 
      (l:ls) -> (ls, l, curr : rightTape)
      [] -> error "At the left end of tape, can't go left"
    Right -> case rightTape of
      (r:rs) -> (curr : leftTape, r, rs)
      [] -> error "At the right end of tape, can't go right"
    Halt -> error "Shouldn't step on a halt instruction"
  where (Instruction instr) = fetch state
        (action, nextState) = instr symbol