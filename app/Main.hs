module Main
    ( main
    ) where

import Prelude

import LitX (litx)
import System.Environment (getArgs)

main :: IO ()
main = litx =<< getArgs
