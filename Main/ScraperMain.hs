module Main
( main
)
where

import System.Environment

import CeqRobot.Control

main :: IO ()
main = do
   args <- getArgs

   case args of
       ["-i"] -> run YesInitQueue
       [] -> run NoInitQueue
       _ -> error $ "Invalid command line args: " ++ show args

