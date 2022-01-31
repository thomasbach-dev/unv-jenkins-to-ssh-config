module Main (main) where

import Data.List (intercalate)

import JTSC
import JTSC.Config

main :: IO ()
main = do
  settings <- getSettings
  dispatchSettingsToAction settings
