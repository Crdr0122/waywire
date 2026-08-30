{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Wayland where

import Language.Haskell.TH
import Text.XML
import Text.XML.Cursor
import Wayland.Protocol
import Wayland.Protocol.Parser
import Wayland.TH

$(testProtocol)

main :: IO ()
main = do
  putStrLn ""
