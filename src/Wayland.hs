{-# LANGUAGE OverloadedStrings #-}

module Wayland where

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Reader (ask, liftIO, runReaderT)
import Data.Text (Text)
import Data.Word (Word32)
import Wayland.Connection
import Wayland.Generated
import Wayland.Types

bindCompositor :: Object WlRegistry -> Word32 -> Text -> Word32 -> W ()
bindCompositor obj name iface version = do
  env <- ask
  when (iface == "wl_compositor") $ liftIO $ do
    compositor <-
      runReaderT
        (wlRegistryBind obj name version WlCompositorHandlers{})
        env
    putStrLn ("bound wl_compositor as " <> show compositor)

main :: IO ()
main = do
  let displayHandlers =
        WlDisplayHandlers
          { onWlDisplayError = \_ obj code msg ->
              liftIO $
                putStrLn ("FATAL wl_display.error: object=" <> show obj <> " code=" <> show code <> " msg=" <> show msg)
          , onWlDisplayDeleteId = \_ _ -> pure ()
          }
      registryHandlers =
        WlRegistryHandlers
          { onWlRegistryGlobal = bindCompositor
          , onWlRegistryGlobalRemove = \_ name ->
              liftIO $ putStrLn ("removed: " <> show name)
          }

  (disp, aThread) <- connect displayHandlers registryHandlers
  wait aThread
