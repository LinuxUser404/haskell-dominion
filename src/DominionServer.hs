{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Dominion Server
-}

import Data.List
import System.IO
import System.Process
import System.Environment
import System.Exit

import DominionTypes

main = do
  myArgs <- getArgs
  let players = myArgs
  let playerProcs = map (\x -> createProcess (proc x []){std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}) players
  player <- head playerProcs
  hPutStrLn (getInput player) "5"
  hFlush (getInput player)
  answer <- hGetContents (getOutput player)
  putStr answer

getInput :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> Handle
getInput (h, _, _, _) = fromMaybe stdin h

getOutput :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> Handle
getOutput (_, h, _, _) = fromMaybe stdout h
