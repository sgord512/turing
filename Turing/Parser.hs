module Turing.Parser where

import Data.Char ( digitToInt )
import Numeric
import Prelude hiding ( Left, Right )
import qualified Data.Either as Either

import Control.Applicative hiding ( (<|>) )

import Text.Parsec hiding ( State )
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Turing.Machine

type Alphabet = [Symbol]
type States = [State]

type PreInstruction = (Pre State, Pre Symbol, Delta)
type PreTM = (Alphabet, States, [PreInstruction])


data Pre a = Spec a | Any 

instance Eq a => Eq (Pre a) where
  Any      == Any       = True
  (Spec a) == (Spec a') = a == a'
  _        == _         = False
  
instance Ord a => Ord (Pre a) where  
  Any `compare` Any = EQ
  (Spec a) `compare` (Spec a') = a `compare` a'
  (Spec a) `compare` Any = GT
  Any `compare` (Spec a) = LT

instance Show a => Show (Pre a) where
  show Any = "_"
  show (Spec a) = show a

anyP = char '_'

-- | Turing Machine spec parser (files have extension .tm)
tmP :: Parser (Alphabet, States, [PreInstruction])
tmP = (,,)  
      <$> (alphabetP <* newline)
      <*> (statesP <* newline)
      <*> (instrP `sepEndBy1` newline)
      <?> "turing machine specification"

commaSep1 = (`sepBy1` (char ',' *> spaces))

preSymbolP :: Parser (Pre Symbol) 
preSymbolP =   (anyP *> pure Any) 
           <|> (Spec <$> symbolP)
           <?> "symbol or _"

symbolP :: Parser Symbol
symbolP =   (char 'B' *> pure B)            
        <|> (Symbol <$> many1 alphaNum)
        <?> "symbol"

alphabetP :: Parser Alphabet
alphabetP = string "Alphabet:" *> spaces *> commaSep1 symbolP

preStateP :: Parser (Pre State)
preStateP =   (anyP *> pure Any)
          <|> (Spec <$> stateP)
          <?> "state or _"

stateP :: Parser State
stateP =   (char 'H' *> pure H) 
       <|> (State <$> many1 alphaNum)
       <?> "state"           


stateOrHaltP :: Parser State           
stateOrHaltP =   (option H $ (State <$> many1 alphaNum))
             <?> "optional next state (defaults to H)"
               

statesP :: Parser [State]
statesP = string "States:" *> spaces *> commaSep1 stateP

actionP :: Parser Action
actionP =   char 'E' *> pure Erase
        <|> char 'L' *> pure Left
        <|> char 'R' *> pure Right
        <|> char 'H' *> pure Halt
        <|> char 'W' *> (Write <$> between (char '(') (char ')') symbolP)
        <?> "action"

instrP :: Parser (Pre State, Pre Symbol, Delta)
instrP = (,,)
         <$> (preStateP <* (spaces *> char ':'))
         <*> (spaces *> preSymbolP) 
         <*> ((,) 
              <$> (spaces *> string "->" *> spaces *> actionP)
              <*> (spaces *> char ';' *> spaces *> stateOrHaltP))
         <?> "instruction"         
         
-- | Parser for initial tape, relatively straightforward, with some sugar to make life easier
parseTape :: String -> Maybe [Symbol]
parseTape str = case parse tapeP "initial tape option" str of
  Either.Left _ -> Nothing 
  Either.Right tape -> Just tape

tapeP :: Parser [Symbol]
tapeP =   concat 
      <$> (starSymbolOrSymbolP `sepEndBy` ((char ',' <|> space) *> spaces))
      <?> "list of symbols separated by commas or spaces"
          
starSymbolOrSymbolP :: Parser [Symbol]          
starSymbolOrSymbolP =   (try starSymbolP) 
                    <|> ((:[]) <$> symbolP)
                    <?> "symbol or num * symbol to be repeated"
          
starSymbolP :: Parser [Symbol]          
starSymbolP =   replicate 
            <$> int
            <*> (char '*' *> symbolP)
            <?> "num * some symbol to be repeated"
            
int :: Parser Int            
int = foldl (\sum nextDigit -> sum * 10 + nextDigit) 0 . map digitToInt <$> many1 digit <?> "int"