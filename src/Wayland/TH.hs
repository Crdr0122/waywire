module Wayland.TH where

import Data.Char

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst _ [] = []
mapFirst f (a : as) = f a : as

toCamelL :: String -> String
toCamelL [] = []
toCamelL ('_' : x : xs) = toUpper x : toCamelL xs
toCamelL (x : xs) = x : toCamelL xs

toCamelU :: String -> String
toCamelU = mapFirst toUpper . toCamelL
