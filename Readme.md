# How to use
Pass your protocols containing folder into generateModules to generate all the stuff
Generated stuff:
- A data type for each interface: eg. data WlDisplay 
- A function for each request that takes in the called object itself and the required arguments: eg. wlDisplaySync
- A function for new_id requests also returns the objectId for that new object
- A handler type for each interface with fields for each event: eg. WlDisplayHandlers{onWlDisplayError, onWlDisplayDeleteId} 
  - For normal events it needs to returns W()
  - For new_id events you need to give it a function that returns the handlers for that object, then that will be registered
- Enums, with bitfield enums being passed as lists of the enums

# Notes
- Fixedt is currently represented as a word32, make own conversion functions
- Bitfield enums are represented as lists of enum values, don't repeat
- Only supports generating all the protocols in one generatedModules call, or else external enums and cross protocol calls error
- If two events can make new objects of each other, the handlers infinitely wrap down
- Destructors not distinguished out yet, won't actually matter since you should also remember yourself which stuff are destroyed, the internal map shouldn't be relied on
- There should be a more ergonomic way to put env, so that handlers wont need W() but just IO(), however requests need the env or else they do not have a way to send messages
  - I can do liftIO in the generated code, but that means I cannot do ask in the handler anymore and have to make handlers in scope of the env passed by connect

# Example
``` haskell
$(generateModules "relative path of folder containing protocols")

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
```
