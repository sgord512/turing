module Turing.Builder where

import Data.List ( intersperse )

import Data.Monoid hiding ( Any )

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Set ( Set )
import qualified Data.Set as Set

import Turing.Machine
import Turing.Parser

type AlphabetSet = Set Symbol
type StateSet = Set State

type AlphasStatesPreInstrs = (AlphabetSet, StateSet, [PreInstruction])

type StatePIMap = Map State [(Pre Symbol, Delta)]
type SymDeltaMap = Map Symbol Delta
type WildWildInstruction = Delta
type WildInstruction = (Symbol, Delta)
type WildcardInstruction = (Pre Symbol, Delta)
type SpecInstruction = (State, Pre Symbol, Delta)

alterWithKey :: Ord k => (k -> Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alterWithKey f k m = Map.alter (f k) k m

buildAlphabet :: Alphabet -> AlphabetSet
buildAlphabet symbols = foldr (\sym set -> if sym `Set.notMember` set
                                           then Set.insert sym set
                                           else makeError "multiple occurences of the same symbol in alphabet" [show sym]) (Set.singleton B) symbols
                        
buildStates :: States -> StateSet                         
buildStates states = foldr (\state set -> if state `Set.notMember` set
                                          then Set.insert state set
                                          else makeError "multiple occurences of the same state in alphabet" [show state]) (Set.singleton H) states

                        
build :: (Alphabet, [State], [PreInstruction]) -> (AlphabetSet, StateSet, Program)
build (symbols, stateList, preInstrs) = (alphabet, states, buildSimpleProgram (alphabet, states, preInstrs))
  where alphabet = buildAlphabet symbols
        states = buildStates stateList

buildSimpleProgram :: (AlphabetSet, StateSet, [PreInstruction]) -> Program
buildSimpleProgram (alphabet, states, instrs) = Program $ Map.fromList $ map (\state -> (state, buildInstruction state)) $ Set.elems states
  where instrsWithHalt = instrs ++ [(Any, Any, (Halt, H))]
        buildInstruction state = Instruction $ Map.fromList $ map (\symbol -> (symbol, firstMatch state symbol instrsWithHalt)) $ Set.elems alphabet

        
matches :: State -> Symbol -> PreInstruction -> Bool
matches state symbol (Any          , Any           , delta) = True
matches state symbol ((Spec state'), Any           , delta) = state == state'
matches state symbol (Any          , (Spec symbol'), delta) = symbol == symbol'
matches state symbol ((Spec state'), (Spec symbol'), delta) = state == state' && symbol == symbol'

firstMatch :: State -> Symbol -> [PreInstruction] -> Delta
firstMatch state symbol [] = makeError "No matching clauses for state/symbol pair" [show state, show symbol]
firstMatch state symbol (i@(_, _, delta):is) | matches state symbol i = delta
                                               | otherwise = firstMatch state symbol is
  
  
{--
buildProgram :: (AlphabetSet, StateSet, [PreInstruction]) -> Program
buildProgram (alphabet, states, preInstrs) = 
  Program $ Map.mapWithKey (\state symDeltaMap -> Instruction $ buildAssocsFromResolutionChain alphabet [symDeltaMap, wildSymDeltaMap] catchAllInstr) stateInstrMap
  where (specInstrs, wildInstrs) = extractWildcardInstructions preInstrs
        statePreInstrMap = buildStatePreInstrMap
        stateInstrMap = Map.mapWithKey (buildInstructionsMap (alphabet, states)) statePreInstrMap -- Build a map of instructions to Action/State pairs for each state
        (wildSymInstrs, wildWildInstrs) = extractWildWildInstructions wildInstrs -- take all WildcardInstructions and separate out the catchAll instructions from the symbol wildcards
        wildSymDeltaMap = buildWilds (alphabet, states) wildSymInstrs -- build a map from symbols to Action/State pairs for the wildcard rules
        catchAllInstr = case wildWildInstrs of
          [] -> Nothing 
          (wwInstr:[]) -> Just wwInstr
          _ -> error "multiple catch-all instructions"


buildStatePreInstrMap :: AlphasStatesPreInstrs -> StatePIMap
buildStatePreInstrMap (alphabet, states, preInstrs) = 
  groupSpecificInstructions (fst $ extractWildcardInstrs preInstrs) $ Map.fromList $ map (\s -> (s, [])) $ Set.elems states -- Group PreInstructions by state they apply to

-- | Creates a map from Symbol -> Delta by setting the Delta for each Symbol to be the value of the first successful lookup in the list of maps to try 
buildAssocsFromResolutionChain :: AlphabetSet -> [SymDeltaMap] -> Maybe Delta -> SymDeltaMap
buildAssocsFromResolutionChain symbols maps catchAll = foldr updateSymDelta Map.empty $ Set.elems symbols
  where updateSymDelta sym symDeltaMap = let deltas = map First $ (map (Map.lookup sym) maps) ++ [catchAll]
                                             updateDelta curr = case curr of
                                               Nothing -> getFirst . mconcat $ deltas
                                               delt -> delt
                                         in Map.alter updateDelta sym symDeltaMap
                                                                           
                                               
buildWilds :: (AlphabetSet, StateSet) -> [WildInstruction] -> SymDeltaMap                                               
buildWilds (alphabet, states) wis = buildWildSymDeltaMap wis Map.empty
  where buildWildSymDeltaMap [] wildSymDeltaMap = wildSymDeltaMap
        buildWildSymDeltaMap ((symbol, act, state):sass) wildSymDeltaMap = 
          buildWildSymDeltaMap sass $ if symbol `Set.member` alphabet 
                                   then if state `Set.member` states 
                                        then alterWithKey (addWildSymDelta (act, state)) symbol wildSymDeltaMap
                                        else makeError "next state for wildcard instruction not found in list of states" [show state]
                                   else makeError "symbol for wildcard instruction not found in alphabet" [show symbol]
        addWildSymDelta symDo s Nothing = Just symDo
        addWildSymDelta symDo s (Just _) = makeError "multiple wildcard rules for given symbol" [show s]

extractWildWildInstructions :: [WildcardInstruction] -> ([WildInstruction], [WildWildInstruction])
extractWildWildInstructions instrs = ewwi instrs
  where ewwi [] = ([], [])
        ewwi ((pSymbol, act, state):wis) = 
          let (wis',wwis') = ewwi wis
          in case pSymbol of 
            Any -> (wis', (act, state):wwis')
            Spec symbol -> ((symbol,act,state):wis', wwis')


extractWildcardInstructions :: [PreInstruction] -> ([SpecInstruction], [WildcardInstruction])
extractWildcardInstructions instrs = ewi instrs
  where ewi [] = ([], [])
        ewi ((pState, pSymbol, act, state):pis) = 
          let (pis', wis') = ewi pis
          in case pState of
            Any -> (pis', (pSymbol, act, state):wis')
            Spec state -> ((state, pSymbol, act, state):pis', wis')


groupSpecificInstructions :: [SpecInstruction] -> StatePIMap -> StatePIMap
groupSpecificInstructions [] statePIMap = statePIMap
groupSpecificInstructions ((state,sym,act,nextState):pis) statePIMap = 
  groupSpecificInstructions pis $ alterWithKey (addPI (sym,act,nextState)) state statePIMap
  where addPI piSAS s Nothing = makeError "state for instruction not in list of states" [show s]
        addPI piSAS s (Just ps) = Just (piSAS : ps)
          
buildInstructionsMap :: (AlphabetSet, StateSet) -> State -> [(Pre Symbol, Action, State)] -> SymDeltaMap
buildInstructionsMap (alphabet, states) state symActStates = 
  let (symDeltaMap, catchAll) = buildSpecInstructionMap symActStates (Map.empty, Nothing)
  in case catchAll of 
    Nothing -> symDeltaMap 
    Just d -> Set.foldr (\symbol sdm -> Map.alter (\val -> if val == Nothing then Just d else val) symbol sdm) symDeltaMap alphabet
  where buildSpecInstructionMap :: [(Pre Symbol, Action, State)] -> (SymDeltaMap, Maybe Delta) -> (SymDeltaMap, Maybe Delta)
        buildSpecInstructionMap [] sdm = sdm
        buildSpecInstructionMap ((sym,act,nextState):ss) (symDeltaMap, other) = 
          buildSpecInstructionMap ss $ case sym of 
            Spec symbol -> if symbol `Set.member` alphabet                               
                           then if nextState `Set.member` states
                                then (alterWithKey (addDelta (act,nextState)) symbol symDeltaMap, other)
                                else makeError "state following instruction not in list of states" [show nextState]
                           else makeError "symbol for instruction not in list of symbols" [show symbol]
            Any -> case other of 
              Nothing -> (symDeltaMap, Just (act, nextState))
              Just _ -> makeError "multiple default rules for given state" [show state]
        addDelta delta s Nothing = Just delta
        addDelta delta s (Just _) = makeError "multiple rules for given state-symbol pair" [show state, show s]
--}