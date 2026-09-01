module Wayland.Types where

import Data.Int
import Data.Word

type Fixed = Int32

data NewObject -- TODO Placeholder for new_id

data ObjectId = ObjectId Word32 deriving (Eq, Ord, Show)
data Opcode = Opcode Word16 deriving (Eq, Ord, Show)

data Object a = Object
  { objectId :: ObjectId
  , objectConnection :: Connection
  }
  deriving (Eq, Show)

data Connection = Connection deriving (Eq, Show)
