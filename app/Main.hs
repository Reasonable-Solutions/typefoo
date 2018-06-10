module Main where

import Lib
import Data.Traversable

a :: [String]
a = ["foo", "bar", "baz"]


printa = traverse putStrLn a

main :: IO ()
main = do
    _ <- printa
    return ()
