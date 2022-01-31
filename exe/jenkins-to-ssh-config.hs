module Main (main) where

import JTSC
import JTSC.Config

main :: IO ()
main = do
  settings <- getSettings
  dispatchSettingsToAction settings
