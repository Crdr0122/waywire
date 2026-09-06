{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Wayland.Types where

import Control.Concurrent.MVar
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Int
import Data.Kind (Type)
import Data.Map (Map, empty, insert)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Word
import Network.Socket
import Network.Socket.ByteString.Lazy (sendWithFds)
import System.Environment (lookupEnv)
import System.IO
import System.Posix.Types (Fd)

type Fixed = Int32

newtype ObjectId = ObjectId Word32 deriving (Eq, Ord, Show)
newtype Opcode = Opcode Word16 deriving (Eq, Ord, Show)

newtype Object a = Object
  { unObject :: ObjectId
  }
  deriving (Eq, Show)

data Value
  = ValueInt Int32
  | ValueUInt Word32
  | ValueFixed Fixed
  | ValueString (Maybe Text)
  | ValueArray ByteString
  | ValueObject ObjectId
  | ValueNewId ObjectId
  | ValueNewIdUntyped Text Word32 ObjectId
  | ValueFd Fd
  deriving (Show)

data ValueType
  = ValueTypeInt
  | ValueTypeUInt
  | ValueTypeFixed
  | ValueTypeString
  | ValueTypeArray
  | ValueTypeObject
  | ValueTypeNewId
  | ValueTypeFd

data DecodeError
  = NotEnoughBytes
  | NotEnoughFds
  | InvalidMessageSize Word16
  | DecodeHeaderFailed String
  | DecodeArgFailed String
  | InvalidString
  | UnknownEventOpcode Word16
  | ValueToEventFailure
  | ExtraBytes
  | UnexpectedFdArg
  deriving (Eq, Show)

data Message = Message
  { messageObject :: ObjectId
  , messageOpcode :: Opcode
  , messagePayload :: [Value]
  , messageFd :: [Fd]
  }
  deriving (Show)

{- | Every generated interface marker type (WlSurface, WlCompositor, ...)
gets an instance of this. 'Handlers' is declared injective (@r -> a@)
so that passing a concrete handler record (e.g. a 'WlSurfaceHandlers')
to 'mkEntry' or to a polymorphic request like bind is enough for GHC
to infer which interface @a@ we mean -- no Proxy or TypeApplications
needed at ordinary call sites.
-}
class InterfaceType a where
  type Handlers a = (r :: Type) | r -> a
  ifaceNameT :: Proxy a -> Text
  ifaceVersionT :: Proxy a -> Int
  mkEntry :: Object a -> Handlers a -> ObjectEntry

data Env = Env
  { envRegistry :: MVar (Map ObjectId ObjectEntry)
  , envIdAlloc :: MVar Word32
  , envSocket :: Socket
  , envSocketLock :: MVar ()
  }

{- | Takes the connection's current fd queue and, on success, hands back
whatever's left of it after this message took what it needed --
see 'Wayland.Decode.decodeValues'. A 'Left NotEnoughFds' (or
'NotEnoughBytes', which won't happen here since the caller already
confirmed a full message is available) means "try again once more
data has arrived," not "fatal."
-}
newtype ObjectEntry = ObjectEntry
  {dispatchEvent :: Opcode -> ByteString -> [Fd] -> Either DecodeError (W (), [Fd])}

type W a = ReaderT Env IO a

mkNewEnv :: IO Env
mkNewEnv = do
  reg <- newMVar empty
  i <- newMVar 1
  display <- lookupEnv "WAYLAND_DISPLAY"
  runtime <- lookupEnv "XDG_RUNTIME_DIR"
  p <- case (display, runtime) of
    (Just d@('/' : _), _) -> pure d
    (Just d, Just x) -> pure (x ++ "/" ++ d)
    (Nothing, Just x) -> pure (x ++ "/wayland-0")
    _ -> error "XDG_RUNTIME_DIR not set"
  soc <- socket AF_UNIX Stream defaultProtocol
  connect soc (SockAddrUnix p)
  lock <- newMVar ()
  pure $ Env reg i soc lock

allocateNewId :: W ObjectId
allocateNewId = do
  i <- asks envIdAlloc
  liftIO $ modifyMVar i (\x -> pure (x + 1, ObjectId (x + 1)))

sendMessage :: (BL.ByteString, [Fd]) -> W ()
sendMessage (msg, fds) = do
  s <- asks envSocket
  lock <- asks envSocketLock
  liftIO $ withMVar lock $ \_ -> sendWithFds s msg fds

registerObject :: ObjectId -> ObjectEntry -> W ()
registerObject oid entry = do
  ref <- asks envRegistry
  liftIO $ (modifyMVar_ ref $ pure . insert oid entry)

pad4 :: Int -> Int
pad4 n = (n + 3) `div` 4 * 4
