{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Wayland where

import Wayland.TH
import Wayland.Types

$(testProtocol)

main :: IO ()
main = do
  putStrLn ""
  print $ unObject display

display :: Object WlDisplay
display = Object (ObjectId 1)
