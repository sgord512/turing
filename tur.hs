module Main where

import Options.Applicative
import System.IO
import Text.Parsec.String

import Graphics.Gloss
import Graphics.Gloss

import Turing.Display
import Turing.Parser
import Turing.Builder
import Turing.Machine hiding ( Left, Right )

specFile = argument' str 
           ( metavar "SPEC"  
          <> help "Path to .tm file containing a turing machine program" )                     
                    
initTape = nullOption 
           ( long "tape" 
          <> short 't'
          <> metavar "TAPE"
          <> help "Initial tape given as input to the turing machine"
          <> value [] 
          <> (reader tapeReader) )
           
tapeReader str = case parseTape str of
  Nothing -> Left $ ShowHelpText
  Just tape -> Right tape

main :: IO ()
main = do (specFileName, tape) <- execParser opts
          putStrLn $ "Parsing: " ++ specFileName ++ " ..." 
          tmSpec <- parseSpecFile specFileName
          let (preAlphabet, preStates, preProg) = tmSpec
              (alphabet, states, p@(Program prog)) = build tmSpec
          --putStrLn $ "PreAlphabet: " ++ (show preAlphabet)
          --putStrLn $ "PreStates: " ++ (show preStates)
          --putStrLn $ "PreProg: \n" ++ (show preProg) 
          --putStrLn "Successfully built! Final program is "                                     
          --putStrLn $ dispMap prog
          runDebugSimulation $ initialize p (head preStates) tape
  where opts = info (helper <*> ((,) <$> specFile <*> initTape))
               ( fullDesc
              <> progDesc "Run the turing machine in SPEC on initial tape TAPE"
              <> header "tur - a turing machine simulator visualizer" )
        
parseSpecFile :: String -> IO (Alphabet, States, [PreInstruction])        
parseSpecFile specFile = do parsedSpec <- parseFromFile tmP specFile
                            case parsedSpec of 
                              Left parseError -> error $ show parseError
                              Right tmSpec -> return tmSpec