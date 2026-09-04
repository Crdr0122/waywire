{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
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

testMessage :: Message
testMessage =
  Message
    (ObjectId 3)
    (Opcode 2)
    [ ValueUInt 42
    , ValueString (Just "hello")
    , ValueObject (ObjectId 7)
    , ValueArray "abc"
    ]
    []

testTypes :: [ValueType]
testTypes =
  [ ValueTypeUInt
  , ValueTypeString
  , ValueTypeObject
  , ValueTypeArray
  ]

data TestType
--
-- data TestTypeHandlers = TestTypeHandlers
--   { onEnter :: Surface -> Output -> IO ()
--   , onLeave :: Surface -> Output -> IO ()
--   }
