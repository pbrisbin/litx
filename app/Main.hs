module Main
    ( main
    ) where

import Prelude

import LitX (litx)
import LitX.Options (parseOptions)
import System.Environment (getArgs)

main :: IO ()
main = litx =<< parseOptions =<< getArgs
