module Main
    ( main
    ) where

import qualified Distribution.Simple


main :: IO ()
main = do
    Distribution.Simple.defaultMain
