module Wayland.Dispatch (
  recvLoop,
) where

import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Monad.Reader
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Network.Socket
import Network.Socket.ByteString
import System.IO (hPutStrLn, stderr)
import System.Posix.Types (Fd)
import Wayland.Decode (decodeMessageHeader)
import Wayland.Types

{- | Everything not yet consumed: bytes belonging to an in-progress or
not-yet-started message, and fds that arrived but haven't been
claimed by an 'fd'-typed argument yet. These two queues are
completely decoupled -- see the note on 'drainAll'.
-}
data RecvState = RecvState
  { pendingBytes :: BL.ByteString
  , pendingFds :: [Fd]
  }

emptyState :: RecvState
emptyState = RecvState BL.empty []

{- | Reads from the connection's socket and dispatches every complete
message it can decode, forever. Meant to be run on its own thread
(e.g. via 'forkIO' at connection setup, passing the same 'Env' that
requests use to send).
-}
recvLoop :: W ()
recvLoop = go emptyState
 where
  go st = drainAll st >>= fillMore >>= go

{- | Decodes and dispatches as many complete messages as are already
sitting in 'pendingBytes' + 'pendingFds', then returns once only a
partial message (or a message that's still missing fds) is left.
-}
drainAll :: RecvState -> W RecvState
drainAll st = case decodeMessageHeader (pendingBytes st) of
  Left NotEnoughBytes -> pure st
  Left err -> fatal ("header decode error: " <> show err)
  Right (oid, opcode, body, rest) -> do
    reg <- asks envRegistry
    entries <- liftIO (readMVar reg)
    case Map.lookup oid entries of
      Nothing -> do
        -- Not a protocol error on its own (e.g. a race between us
        -- destroying an object and an in-flight event for it) --
        -- log and move past this message.
        liftIO $ hPutStrLn stderr ("wayland: event for unknown object " <> show oid)
        drainAll st{pendingBytes = rest}
      Just entry -> case dispatchEvent entry opcode (BL.toStrict body) (pendingFds st) of
        -- Not enough fds yet: the sendmsg() carrying them hasn't
        -- arrived. Stop draining -- same as NotEnoughBytes -- and go
        -- read more. Crucially we do NOT advance past this message
        -- (pendingBytes stays as-is), since we haven't dispatched it.
        Left NotEnoughFds -> pure st
        Left err -> fatal ("decode error: " <> show err)
        Right (action, leftoverFds) -> do
          action
          drainAll st{pendingBytes = rest, pendingFds = leftoverFds}
 where
  fatal msg = liftIO $ ioError (userError ("wayland: fatal protocol error: " <> msg))

{- | One recvmsg() call: appends whatever bytes and fds it returns to
the running queues. See the module-level note on sizing the cmsg
buffer -- 512 bytes comfortably covers libwayland's 28-fds-per-flush
cap; MSG_CTRUNC here means fds were already dropped by the kernel
and is unrecoverable, so it's treated as fatal rather than retried.
-}
fillMore :: RecvState -> W RecvState
fillMore st = do
  sock <- asks envSocket
  (chunk, fds, flags) <- liftIO $ do
    (_addr, bs, cmsgs, flags) <- recvMsg sock 4096 512 mempty
    let newFds = concat [fs | c <- cmsgs, cmsgId c == CmsgIdFds, Just fs <- [decodeCmsg c :: Maybe [Fd]]]
    pure (bs, newFds, flags)
  when (truncatedControl flags) $
    liftIO (ioError (userError "wayland: ancillary data truncated, fds were dropped by the kernel"))
  when (BS.null chunk) $
    liftIO (ioError (userError "wayland: connection closed"))
  pure
    st
      { pendingBytes = pendingBytes st <> BL.fromStrict chunk
      , pendingFds = pendingFds st ++ fds
      }
 where
  truncatedControl :: MsgFlag -> Bool
  truncatedControl flags = (flags .&. MSG_CTRUNC) /= mempty
