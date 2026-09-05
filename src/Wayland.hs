{-# LANGUAGE OverloadedStrings #-}

module Wayland where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Wayland.Dispatch
import Wayland.Generated
import Wayland.Types

main :: IO ()
main = do
  env <- mkNewEnv

  let display = Object (ObjectId 1) :: Object WlDisplay
      handlers =
        WlRegistryHandlers
          { onWlRegistryGlobal = \name iface version ->
              liftIO $ putStrLn (show name <> ": " <> show iface <> " v" <> show version)
          , onWlRegistryGlobalRemove = \name ->
              liftIO $ putStrLn ("removed: " <> show name)
          }
  _ <- forkIO (runReaderT recvLoop env)
  runReaderT (void (wlDisplayGetRegistry display handlers)) env
  threadDelay 500_000
