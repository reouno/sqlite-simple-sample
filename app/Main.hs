module Main where

import Lib

main :: IO ()
main =
    testCreate
    >> testSQLite
    >> testSQLite2
