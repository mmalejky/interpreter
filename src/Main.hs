module Main where

import Prelude
import System.IO ( hPutStrLn, stderr )
import System.Environment ( getArgs )
import Control.Monad      ( when )

import AbsBarraquito ( Program )
import LexBarraquito ( Token )
import ParBarraquito ( pProgram, myLexer )

import TypeChecker
import Interpreter

type Err       = Either String
type ParseFun  = [Token] -> Err Program
type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 0) $ putStrLn s

interpret :: Verbosity -> Program -> IO ()
interpret _ prog = case checkProgram prog of
  Left err -> do hPutStrLn stderr "> Type-checking failed with error: "
                 hPutStrLn stderr err
  Right _  -> evalProgram prog

run :: Verbosity -> ParseFun -> String -> IO ()
run v p s = case p tokens of
  Left err   -> do hPutStrLn stderr "> Parsing failed!"
                   hPutStrLn stderr err
  Right tree -> interpret v tree
  where
    tokens = myLexer s

runFile :: Verbosity -> ParseFun -> FilePath -> IO ()
runFile v p f = putStrV v f >> readFile f >>= run v p

main :: IO ()
main = do
  args <- getArgs
  case args of
    "-v":[] -> getContents >>= run 1 pProgram
    []      -> getContents >>= run 0 pProgram
    "-v":fs -> mapM_ (runFile 1 pProgram) fs
    fs      -> mapM_ (runFile 0 pProgram) fs
