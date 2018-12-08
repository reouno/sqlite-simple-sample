module Main where

import Lib

main :: IO ()
main =
    testSQLite
    >> testSQLite2
