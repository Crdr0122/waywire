module Wayland.Protocol where

import Data.List (elemIndex)
import Data.Map (Map, findWithDefault, fromList)
import Data.Text (Text)

data Protocol = Protocol
  { protoName :: Text
  , protoCopyright :: Maybe Text
  , protoDescription :: Maybe Description
  , protoInterfaces :: [Interface]
  }
  deriving (Eq, Show)

data Interface = Interface
  { ifaceName :: Text
  , ifaceVersion :: Int
  , ifaceDescription :: Maybe Description
  , ifaceRequests :: [Request]
  , ifaceEvents :: [Event]
  , ifaceEnums :: [Enum']
  }
  deriving (Eq, Show)

data Request = Request
  { reqName :: Text
  , reqDescription :: Maybe Description
  , reqSince :: Int
  , reqDeprecatedSince :: Maybe Int
  , reqDestructor :: Bool -- Request Type
  , reqArguments :: [Argument]
  }
  deriving (Eq, Show)

data Event = Event
  { eventName :: Text
  , eventDescription :: Maybe Description
  , eventSince :: Int
  , eventDeprecatedSince :: Maybe Int
  , eventDestructor :: Bool -- Event Type
  , eventArguments :: [Argument]
  }
  deriving (Eq, Show)

data Argument = Argument
  { argName :: Text
  , argDescription :: Maybe Description
  , argSummary :: Maybe Text
  , argType :: ArgType
  , argEnum :: Maybe EnumRef
  }
  deriving (Eq, Show)

data ArgType
  = TypeInt
  | TypeUInt
  | TypeFixed
  | TypeString {stringNullable :: Bool}
  | TypeArray
  | TypeFileDescriptor
  | TypeObject
      { objectInterface :: Maybe Text
      , objectNullable :: Bool
      }
  | TypeNewId {newIdInterface :: Maybe Text}
  deriving (Eq, Show)

data Enum' = Enum'
  { enumName :: Text
  , enumDescription :: Maybe Description
  , enumSince :: Int
  , enumBitfield :: Bool
  , enumEntries :: [EnumEntry]
  }
  deriving (Eq, Show)

data EnumEntry = EnumEntry
  { enumEntryName :: Text
  , enumEntryValue :: Int
  , enumEntryDescription :: Maybe Description
  , enumEntrySummary :: Maybe Text
  , enumEntrySince :: Int
  , enumEntryDeprecatedSince :: Maybe Int
  }
  deriving (Eq, Show)

data Description = Description
  { descSummary :: Maybe Text
  , descText :: Text
  }
  deriving (Eq, Show)

data EnumRef
  = LocalEnum Text
  | ExternalEnum Text Text
  deriving (Eq, Show)

type EnumTable = Map (Text, Text) Enum'

buildEnumTable :: Protocol -> EnumTable
buildEnumTable Protocol{protoInterfaces = ifaces} =
  fromList [((ifaceName i, enumName e), e) | i <- ifaces, e <- ifaceEnums i]

resolveEnumRef :: EnumTable -> Text -> EnumRef -> (Text, Enum')
resolveEnumRef table selfIface ref = case ref of
  LocalEnum en -> (selfIface, look (selfIface, en))
  ExternalEnum ifaceN en -> (ifaceN, look (ifaceN, en))
 where
  look key = findWithDefault (error ("waywire: unresolved enum ref " ++ show key)) key table

isNewID :: Argument -> Bool
isNewID Argument{argType = TypeNewId _} = True
isNewID _ = False

requestOpCode :: Interface -> Request -> Int
requestOpCode iface req = case elemIndex req (ifaceRequests iface) of
  Just n -> n
  Nothing -> error "request does not belong to interface"

eventOpCode :: Interface -> Event -> Int
eventOpCode iface event = case elemIndex event (ifaceEvents iface) of
  Just n -> n
  Nothing -> error "event does not belong to interface"
