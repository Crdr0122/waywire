# How to use
Pass your protocols containing folder into generateModules to generate all the stuff
Generated stuff:
- A data type for each interface: eg. data WlDisplay 
- A function for each request that takes in the called object itself and the required arguments: eg. wlDisplaySync
- A function for new_id requests also returns the objectId for that new object and needs the handlers
- A handler type for each interface with fields for each event: eg. WlDisplayHandlers{onWlDisplayError, onWlDisplayDeleteId} 
  - For normal events it needs to returns W()
  - For new_id events you need to give it a function that returns the handlers for that object, then that will be registered
- Enums, with bitfield enums being passed as lists of the enums
  - Internal functions to convert numbers to enums and back, exposed because your protocols may need those as well (if they reference wayland.xml enums)

# Notes
- Fixedt is currently represented as a word32, make your own conversion functions
- Bitfield enums are represented as lists of enum values, don't repeat
- All the enums are quietly using uint even though the functions will take int, uses fromIntegral, I don't think there are enums using negative numbers or higher than int max?
  - Isn't this a protocol problem? Why is the uint/int type set at the event/request site and not the enum? What happens if two different use sites uses different types?
- Only supports generating all the protocols in one generatedModules call, or else external enums and cross protocol calls error
  - wayland.xml is included and all its enums will be merged into the generatedModules call
- The design forces you to register something in the registry for an new id event/request that is automatically registered, which disallow ignoring like in libwayland
  - If two events can make new objects of each other, the handlers infinitely wrap down
- You can use registerObject to override a registered handler. This is used in connect to register the first WlDisplayHandler
- Destructors not distinguished out yet, won't actually matter since you should also remember yourself which stuff are destroyed, the internal map shouldn't be relied on
- Do I need to add something for the user to pass in their own data like the void* ptr in libwayland?

# Example
``` haskell
$(generateModules "relative path of folder containing protocols eg. files")

bindCompositor :: Object WlRegistry -> Word32 -> Text -> Word32 -> W ()
bindCompositor obj name iface version = do
  when (iface == "wl_compositor") $ do
    compositor <- wlRegistryBind obj name version WlCompositorHandlers{}
    liftIO $ putStrLn ("bound wl_compositor as " <> show compositor)

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
