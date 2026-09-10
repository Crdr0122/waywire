# How to use
Pass your protocols containing folder into generateModules to generate all the stuff
## Generated stuff:
- A data type for each interface: eg. data WlDisplay 
- A function for each request that takes in the called object itself and the required arguments: eg. wlDisplaySync
- A function for new_id requests also returns the objectId for that new object and needs the handlers
- A handler type for each interface with fields for each event: eg. WlDisplayHandlers{onWlDisplayError, onWlDisplayDeleteId} 
  - For normal events it needs to returns W()
  - For new_id events you need to give it a function that returns maybe the handlers for that object, then that will be registered
- Enums, with bitfield enums being passed as sets of the enums
  - Internal functions to convert numbers to enums and back, exposed because your protocols may need those as well (if they reference wayland.xml enums)
- Haddock documentation for functions, events and enums

# Notes
- Fixedt is currently represented as a word32, make your own conversion functions
- All the enums are quietly using uint even though the functions will take int, uses fromIntegral, I don't think there are enums using negative numbers or higher than int max?
  - Isn't this a protocol problem? Why is the uint/int type set at the event/request site and not the enum? What happens if two different use sites uses different types?
- Only supports generating all the protocols in one generatedModules call, or else external enums and cross protocol calls error, if you're sure non of them do that you can do multiple calls
  - wayland.xml is included and all its enums will be merged into the generatedModules call
- You can use registerObject to override a registered handler, or add a handler after ignoring it in the event it spawned in. This is used in connect to register the first WlDisplayHandler
- Destructors not distinguished out yet, won't actually matter since you should also remember yourself which stuff are destroyed, the internal map shouldn't be relied on
- Do I need to add something for the user to pass in their own data like the void* ptr in libwayland?
- If there are any funnily named requests or events that cause name collision, I would need to change the namings. Right now I haven't met any protocols that do that
- File descriptors passed in from events are marked as CLOEXEC, so set that to false if you need to hand it off to another process. 
- There is a race condition when receiving FD and opening the wayland socket, since recvmsg and socket don't expose MSG_CMSG_CLOEXEC and SOCK_CLOEXEC. Open the wayland socket before spawning processes, as for event FDs good luck

# Example
``` haskell
$(generateModules "path of folder containing protocols relative to project root eg. files")

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
