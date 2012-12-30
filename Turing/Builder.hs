module Turing.Builder where

import Data.Monoid hiding ( Any )

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Set ( Set )
import qualified Data.Set as Set

import Turing.Machine
import Turing.Parser

type AlphabetSet = Set Symbol
type StateSet = Set State

fork :: (a -> b) -> (a -> c) -> a -> (b, c)
fork f g x = (f x, g x)

build :: (Alphabet, [State], [PreInstruction]) -> Program
build (symbols, stateList, preInstrs) = buildProgram (alphabet, states, preInstrs)
  where alphabet = foldr (\sym set -> 
                           if sym `Set.notMember` set
                           then Set.insert sym set
                           else error "multiple occurences of the same symbol in alphabet") Set.empty symbols          
        states = foldr (\state set -> 
                         if state `Set.notMember` set
                         then Set.insert state set
                         else error "multiple occurences of the same state in alphabet") Set.empty stateList

buildProgram :: (AlphabetSet, StateSet, [PreInstruction]) -> Program
buildProgram (alphabet, states, preInstrs) = Program (\state -> case Map.lookup state instructions of
  Nothing -> error "no instructions found for given state"
  Just instruction -> instruction)
  where (specInstrs, wildInstrs) = extractWildcardInstructions preInstrs
        statePreInstrMap = groupSpecInstructions specInstrs $ Map.fromList $ map (\s -> (s, [])) $ Set.elems states
        stateInstrMap = Map.map (buildInstructionsMap (alphabet, states)) statePreInstrMap
        (wildSymInstrs, wildWildInstrs) = extractWildWildInstructions wildInstrs
        wildSymDoMap = buildWilds (alphabet, states) wildSymInstrs
        catchAllInstr = case wildWildInstrs of
          [] -> Nothing 
          (wwInstr:[]) -> Just wwInstr
          _ -> error "multiple catch-all instructions"
        instructions = Map.mapWithKey (\state symDoMap -> let instruction symbol = getFirst . mconcat . map First $ [Map.lookup symbol symDoMap,
                                                                                                                     Map.lookup symbol wildSymDoMap,
                                                                                                                     catchAllInstr]
                                                          in Instruction (\symbol -> case instruction symbol of
                                                                             Just rslt -> rslt
                                                                             Nothing -> error "no instruction found for given state-symbol pair")) stateInstrMap
                                                                           
                                               
buildWilds :: (AlphabetSet, StateSet) -> [WildInstruction] -> SymDoMap                                               
buildWilds (alphabet, states) wis = buildWildSymDoMap wis Map.empty
  where buildWildSymDoMap [] wildSymDoMap = wildSymDoMap
        buildWildSymDoMap ((symbol, act, state):sass) wildSymDoMap = 
          buildWildSymDoMap sass $ if symbol `Set.member` alphabet 
                                   then if state `Set.member` states 
                                        then Map.alter (addWildSymDo (act, state)) symbol wildSymDoMap
                                        else error "next state for wildcard instruction not found in list of states"
                                   else error "symbol for wildcard instruction not found in alphabet" 
        addWildSymDo symDo Nothing = Just symDo
        addWildSymDo symDo (Just _) = error "multiple wildcard rules for given symbol"

extractWildWildInstructions :: [WildcardInstruction] -> ([WildInstruction], [WildWildInstruction])
extractWildWildInstructions instrs = ewwi instrs
  where ewwi [] = ([], [])
        ewwi ((pSymbol, act, state):wis) = 
          let (wis',wwis') = ewwi wis
          in case pSymbol of 
            Any -> (wis', (act, state):wwis')
            Spec symbol -> ((symbol,act,state):wis', wwis')

type WildWildInstruction = (Action, State)
type WildInstruction = (Symbol, Action, State)                              
type WildcardInstruction = (Pre Symbol, Action, State)
type SpecInstruction = (State, Pre Symbol, Action, State)

extractWildcardInstructions :: [PreInstruction] -> ([SpecInstruction], [WildcardInstruction])
extractWildcardInstructions instrs = ewi instrs
  where ewi [] = ([], [])
        ewi ((pState, pSymbol, act, state):pis) = 
          let (pis', wis') = ewi pis
          in case pState of
            Any -> (pis', (pSymbol, act, state):wis')
            Spec state -> ((state, pSymbol, act, state):pis', wis')

type StatePIMap = Map State [(Pre Symbol, Action, State)]
type SymDoMap = Map Symbol (Action, State)

groupSpecInstructions :: [SpecInstruction] -> StatePIMap -> StatePIMap
groupSpecInstructions [] statePIMap = statePIMap
groupSpecInstructions ((state,sym,act,nextState):pis) statePIMap = 
  groupSpecInstructions pis $ Map.alter (addPI (sym,act,nextState)) state statePIMap
  where addPI piSAS Nothing = error "state for instruction not in list of states"
        addPI piSAS (Just ps) = Just (piSAS : ps)
          
buildInstructionsMap :: (AlphabetSet, StateSet) -> [(Pre Symbol, Action, State)] -> SymDoMap
buildInstructionsMap (alphabet, states) symActStates = 
  let (symDoMap, other) = buildSpecInstructionMap symActStates (Map.empty, Nothing)
  in case other of 
    Nothing -> symDoMap 
    Just doActState -> Set.foldr (\symbol sdm -> Map.alter (\val -> if val == Nothing then Just doActState else val) symbol sdm) symDoMap alphabet
  where buildSpecInstructionMap :: [(Pre Symbol, Action, State)] -> (SymDoMap, Maybe (Action, State)) -> (SymDoMap, Maybe (Action, State))
        buildSpecInstructionMap [] sdm = sdm
        buildSpecInstructionMap ((sym,act,state):ss) (symDoMap, other) = 
          buildSpecInstructionMap ss $ case sym of 
            Spec symbol -> if symbol `Set.member` alphabet                               
                           then if state `Set.member` states
                                then (Map.alter (addSymDo (act,state)) symbol symDoMap, other)
                                else error "state following instruction not in list of states"
                           else error "symbol for instruction not in list of symbols"
            Any -> case other of 
              Nothing -> (symDoMap, Just (act, state))
              Just _ -> error "multiple default rules for given state"
        addSymDo symDo Nothing = Just symDo
        addSymDo symDo (Just _) = error "multiple rules for given state-symbol pair"
