{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Graphics.Wayland
Description : Haskell implementation of the Wayland wire protocol
Copyright   : (C) Sivert Berg, 2014-2015
License     : GPL3
Maintainer  : code@trev.is
Stability   : Experimental

Main module that pulls in all other required modules.
-}
module Wayland where

import Text.XML
import Text.XML.Cursor
import Wayland.Protocol
import Wayland.Protocol.Parser

main :: IO ()
main = do
  doc <- Text.XML.readFile def "files/xdg-shell.xml"

  let cursor = fromDocument doc

  -- print $
  --   cursor
  --     $/ element "description"
  --     >=> attribute "summary"
  --
  -- print $ parseProtocol cursor
  case parseProtocol cursor of
    Left err -> print err
    Right protocol -> print protocol
