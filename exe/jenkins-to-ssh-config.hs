module Main (main) where

import Data.List (intercalate)

import JTSC.Config
import JTSC

main :: IO ()
main = do
  settings <- getSettings
  result <- fetchFromJenkinsAndParse settings
  newConfig <- case result of
                  Left err -> (error . show) err
                  Right entries  -> return (intercalate "\n\n" entries ++ "\n\n")
  case (append settings, sshConfig settings) of
      (False, Just fp) -> writeFile fp newConfig
      (True , Just fp) -> appendFile fp newConfig
      (_    , Nothing) -> putStrLn newConfig
