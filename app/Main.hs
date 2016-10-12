module Main (main) where

import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure, exitSuccess)

import qualified Dnsnitch


main :: IO ()
main = do
  args <- getArgs

  case length args of
    0 ->
      Dnsnitch.main 53 8080
    2 ->
      let
        dnsPort = read (head args)
        httpPort = read (args !! 1)
      in
        Dnsnitch.main dnsPort httpPort
    _ -> do
      prog <- getProgName
      putStrLn $ prog ++ " <dns port> <http port>"
      exitFailure

  exitSuccess
