module Wayland.Connection (
  Display (..),
  connect,
  roundtrip,
  registerObject,
  Object (..),
  ObjectId (..),
  Fixed,
  W,
  Env (..),
) where

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.MVar
import Control.Monad (void)
import Control.Monad.Reader
import Wayland.Dispatch (recvLoop)
import Wayland.Generated
import Wayland.Types

{- | Everything you need to keep going after connecting: the shared
'Env' (pass it to 'runReaderT' for any further request), and the
two objects every client always has.
-}
data Display = Display
  { displayEnv :: Env
  , displayObject :: Object WlDisplay
  , displayRegistry :: Object WlRegistry
  }

{- | Connects, starts the background read loop, registers wl_display
itself at id 1 (see note above -- nothing else will ever do this
for you), fetches the registry, and does one 'roundtrip' before
returning so every global advertised at connect time has already
reached 'onWlRegistryGlobal' by the time you get 'Display' back.
-}
connect :: WlDisplayHandlers -> WlRegistryHandlers -> IO (Display, Async ())
connect displayHandlers registryHandlers = do
  env <- mkNewEnv
  let display = Object (ObjectId 1) :: Object WlDisplay
  runReaderT (registerObject (ObjectId 1) (mkWlDisplayEntry display displayHandlers)) env
  dispatchThread <- async (runReaderT recvLoop env)
  registry <- runReaderT (wlDisplayGetRegistry display registryHandlers) env
  let disp = Display env display registry
  roundtrip disp
  pure (disp, dispatchThread)

{- | The standard wl_display.sync dance: send sync, block until its
wl_callback.done fires. Since events are processed strictly in
order, that's a guarantee that everything sent *before* this call
has already been fully handled by the compositor -- including any
async wl_registry.global events, which have no "I'm done"
notification of their own. Call this again later too, e.g. after
binding wl_seat, before you assume you know its capabilities.
-}
roundtrip :: Display -> IO ()
roundtrip (Display env display _) = do
  done <- newEmptyMVar
  let callbackHandlers = WlCallbackHandlers{onWlCallbackDone = \_ _serial -> liftIO (putMVar done ())}
  runReaderT (void (wlDisplaySync display callbackHandlers)) env
  takeMVar done
