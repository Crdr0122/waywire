{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Wayland.TH where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Char as C
import Data.Int (Int32)
import Data.List (findIndex)
import Data.List qualified as L
import Data.Proxy
import Data.Text (Text, cons, splitOn, uncons, unpack)
import Data.Text qualified as T
import Data.Word (Word32)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Posix.Types (Fd)
import Text.XML
import Text.XML.Cursor
import Wayland.Decode (decodeValues)
import Wayland.Encode (encodeMessage)
import Wayland.Protocol
import Wayland.Protocol.Parser
import Wayland.Types

-- TODO Enum Args

testProtocol :: FilePath -> Q [Dec]
testProtocol fp = do
  addDependentFile fp
  fileContent <- runIO $ Text.XML.readFile def fp
  case parseProtocol $ fromDocument fileContent of
    Right a -> generateProtocol a
    Left _ -> pure []

generateProtocol :: Protocol -> Q [Dec]
generateProtocol Protocol{protoInterfaces = ifaces} = do
  types <- flattenQ $ generateIfaceType <$> ifaces
  events <- flattenQ $ generateIfaceEvents <$> ifaces
  reqs <- flattenQ $ generateIfaceReqs <$> ifaces
  enums <- flattenQ $ generateIfaceEnums <$> ifaces
  instances <- flattenQ $ generateIfaceInstance <$> ifaces
  pure $ types ++ events ++ reqs ++ instances ++ enums

{- | The empty marker type for an interface, e.g. @data WlSurface@.
|Used only as the phantom parameter of 'Object' and 'Handlers'.
-}
generateIfaceType :: Interface -> Q [Dec]
generateIfaceType Interface{ifaceName = n} = do
  let hsName = mkName . unpack . toCamelU $ n
  dec <- dataD (cxt []) hsName [] Nothing [] []
  pure [dec]

--------------------------------------------------------------------------------
-- Events: one handler record + one fused decode-and-dispatch function
-- per interface. No event ADT, no SomeEvent, no Typeable.
--------------------------------------------------------------------------------

{- | For a given interface, generates:

> data WlSurfaceHandlers = WlSurfaceHandlers
>   { onWlSurfaceEnter :: Object WlOutput -> W ()
>   , onWlSurfaceLeave :: Object WlOutput -> W ()
>   }
>
> dispatchWlSurface :: WlSurfaceHandlers -> Opcode -> ByteString -> [Fd] -> Either DecodeError (W (),[Fd])
> dispatchWlSurface handlers (Opcode 0) bs fds = do
>   (values, leftoverFds) <- decodeValues [ValueTypeObject] fds (BL.fromStrict bs)
>   case values of
>     [ValueObject x] -> Right (onWlSurfaceEnter handlers (Object x), leftoverFds)
>     _ -> Left ValueToEventFailure
> ...
> dispatchWlSurface _ (Opcode o) _ _ = Left (UnknownEventOpcode o)
>
> mkWlSurfaceEntry :: WlSurfaceHandlers -> ObjectEntry
> mkWlSurfaceEntry = ObjectEntry . dispatchWlSurface

Events with a new_id argument of known interface (e.g. wl_data_device's
data_offer) instead get a handler field of type
@Object Child -> ...otherArgs... -> W (Handlers Child)@, and the
generated dispatch clause registers the child object for you:

> [ValueNewId newId, ...] -> Right $ do
>   h <- onWlDataDeviceDataOffer handlers (Object newId) ...
>   registerObject newId (mkEntry h)
-}
generateIfaceEvents :: Interface -> Q [Dec]
generateIfaceEvents Interface{ifaceName = n, ifaceEvents = events} = do
  let uName = unpack . toCamelU $ n
      handlersName = mkName (uName ++ "Handlers")
      dispatchName = mkName ("dispatch" ++ uName)
      mkEntryName = mkName ("mk" ++ uName ++ "Entry")

  fields <- sequence $ generateHandlerField uName <$> events
  handlersDec <- dataD (cxt []) handlersName [] Nothing [recC handlersName (pure <$> fields)] []

  let clauses = zipWith (generateEventClause uName) [0 ..] events
      fallback = generateFallbackClause
  dispatchSig <- sigD dispatchName [t|$(conT handlersName) -> Opcode -> ByteString -> [Fd] -> Either DecodeError (W (), [Fd])|]
  dispatchFun <- funD dispatchName (clauses ++ [fallback])

  mkEntrySig <- sigD mkEntryName [t|$(conT handlersName) -> ObjectEntry|]
  mkEntryFun <- funD mkEntryName [clause [] (normalB [|ObjectEntry . $(varE dispatchName)|]) []]

  pure [handlersDec, dispatchSig, dispatchFun, mkEntrySig, mkEntryFun]

-- | Build one record field for the handlers type. eg. onWlSurfaceEnter
generateHandlerField :: String -> Event -> Q VarBangType
generateHandlerField uName Event{eventName = n, eventArguments = args} = do
  let fieldName = mkName ("on" ++ uName ++ (unpack . toCamelU $ n))
  varBangType
    fieldName
    (bangType (bang noSourceUnpackedness noSourceStrictness) (generateHandlerFieldType args))

{- | Non-spawning event: @arg1 -> arg2 -> ... -> W ()@.
Spawning event (one new_id arg with a known interface): the new
object is passed first, and the result is @W (Handlers Child)@ so
the dispatch clause knows what to register.
-}
generateHandlerFieldType :: [Argument] -> Q Type
generateHandlerFieldType args = case findNewIdArg args of
  Nothing -> buildArrow (generateArgType <$> args) [t|W ()|]
  Just (childIfaceText, otherArgs) ->
    let childTy = conT (mkName . unpack . toCamelU $ childIfaceText)
        argTys = [t|Object $childTy|] : (generateArgType <$> otherArgs)
     in buildArrow argTys [t|W (Handlers $childTy)|]

{- | Finds the single new_id-with-known-interface argument, if any,
and returns it along with the remaining arguments in their
original relative order. Errors (at generation time) if there's
more than one, or if there's a new_id with no interface -- the
latter is only valid in requests (wl_registry.bind), never events.
-}
findNewIdArg :: [Argument] -> Maybe (Text, [Argument])
findNewIdArg args = case [(t, a) | a@Argument{argType = TypeNewId (Just t)} <- args] of
  [] -> Nothing
  [(t, spawnArg)] -> Just (t, L.delete spawnArg args)
  _ -> error "waywire: multiple new_id arguments in a single event is not supported"

-- | One clause of 'dispatchWlSurface' for one event/opcode.
generateEventClause :: String -> Integer -> Event -> Q Clause
generateEventClause uName i Event{eventName = n, eventArguments = args} = do
  handlersName <- newName "handlers"
  fdsName <- newName "fds"
  bsName <- newName "bs"
  valuesName <- newName "values"
  leftoverFdsName <- newName "leftoverFds"
  decodePats <- sequence $ generateDecodePattern <$> args
  let (valuePats, varNames) = unzip decodePats
      fieldName = mkName ("on" ++ uName ++ (unpack . toCamelU $ n))
      valueTypesExp = listE (generateValueTypeExp . argType <$> args)
      bodyExp = generateEventBody fieldName args varNames handlersName leftoverFdsName
      matchOk = TH.match (listP valuePats) (normalB bodyExp) []
      matchFallback = TH.match wildP (normalB [|Left ValueToEventFailure|]) []
      decodeStmt = bindS (tupP [varP valuesName, varP leftoverFdsName]) [|decodeValues $valueTypesExp $(varE fdsName) (BL.fromStrict $(varE bsName))|]
      caseStmt = noBindS (caseE (varE valuesName) [matchOk, matchFallback])
  clause
    [varP handlersName, conP 'Opcode [litP (integerL i)], varP bsName, varP fdsName]
    (normalB (doE [decodeStmt, caseStmt]))
    []

{- | Builds the @Right (onXEvent handlers arg1 arg2 ..., leftoverFds)@ or, for a
spawning event, the @Right (do { h <- onXEvent handlers (Object newId)
...; registerObject newId (mkEntry h) }, leftoverFds)@ body. Decoded object
arguments come back as raw 'ObjectId's from 'decodeValues', so any
argument whose handler-field type is @Object iface@ gets wrapped
here to match. leftoverFdsName' is whatever 'decodeValues' didn't
need for this message's own fd arguments -- it's just threaded
through untouched, for the next message to consume.
-}
generateEventBody :: TH.Name -> [Argument] -> [TH.Name] -> TH.Name -> TH.Name -> Q Exp
generateEventBody fieldName args varNames handlersName leftoverFdsName =
  case newIdIndex of
    Nothing ->
      let argExps = zipWith wrapArgExp args varNames
          applyHandler = foldl' appE (appE (varE fieldName) (varE handlersName)) argExps
       in [|Right ($applyHandler, $(varE leftoverFdsName))|]
    Just idx ->
      let newIdVar = varNames !! idx
          otherArgs = [a | (j, a) <- zip [0 :: Int ..] args, j /= idx]
          otherVars = [v | (j, v) <- zip [0 :: Int ..] varNames, j /= idx]
          argExps = [|Object $(varE newIdVar)|] : zipWith wrapArgExp otherArgs otherVars
          applyHandler = foldl' appE (appE (varE fieldName) (varE handlersName)) argExps
       in [|
            Right
              ( do
                  h <- $applyHandler
                  registerObject $(varE newIdVar) (mkEntry h)
              , $(varE leftoverFdsName)
              )
            |]
 where
  newIdIndex = case [i | (i, a) <- zip [0 :: Int ..] args, isSpawning a] of
    [] -> Nothing
    [i] -> Just i
    _ -> error "waywire: multiple new_id arguments in a single event is not supported"
  isSpawning Argument{argType = TypeNewId (Just _)} = True
  isSpawning _ = False

{- | 'decodeValues' always hands back a raw 'ObjectId' for both
ValueObject and ValueNewId; wrap it in 'Object' wherever the
handler-field type (built by 'generateArgType') expects that.
-}
wrapArgExp :: Argument -> TH.Name -> Q Exp
wrapArgExp Argument{argType = TypeObject (Just _) False} v = [|Object $(varE v)|]
wrapArgExp Argument{argType = TypeObject (Just _) True} v =
  [|if $(varE v) == ObjectId 0 then Nothing else Just (Object $(varE v))|]
wrapArgExp Argument{argType = TypeObject Nothing True} v =
  [|if $(varE v) == ObjectId 0 then Nothing else Just $(varE v)|]
wrapArgExp _ v = varE v

-- Fallback clause
generateFallbackClause :: Q Clause
generateFallbackClause = do
  oName <- newName "o"
  clause
    [wildP, conP 'Opcode [varP oName], wildP, wildP]
    (normalB [|Left (UnknownEventOpcode $(varE oName))|])
    []

generateValueTypeExp :: ArgType -> Q Exp
generateValueTypeExp t =
  conE $ case t of
    TypeInt -> 'ValueTypeInt
    TypeUInt -> 'ValueTypeUInt
    TypeFixed -> 'ValueTypeFixed
    TypeString _ -> 'ValueTypeString
    TypeArray -> 'ValueTypeArray
    TypeFileDescriptor -> 'ValueTypeFd
    TypeObject _ _ -> 'ValueTypeObject
    TypeNewId _ -> 'ValueTypeNewId

--------------------------------------------------------------------------------
-- Interface instances
--------------------------------------------------------------------------------

{- | > instance Interface WlSurface where
  >   type Handlers WlSurface = WlSurfaceHandlers
  >   ifaceNameT _ = "wl_surface"
  >   ifaceVersionT _ = 5
  >   mkEntry = mkWlSurfaceEntry
-}
generateIfaceInstance :: Interface -> Q [Dec]
generateIfaceInstance Interface{ifaceName = n, ifaceVersion = v} = do
  let uName = unpack . toCamelU $ n
      ifaceTy = conT (mkName uName)
      handlersTy = conT (mkName (uName ++ "Handlers"))
      mkEntryName = mkName ("mk" ++ uName ++ "Entry")
  tyInst <- tySynInstD (tySynEqn Nothing [t|Handlers $ifaceTy|] handlersTy)
  nameImpl <- funD 'ifaceNameT [clause [wildP] (normalB (litE (stringL (T.unpack n)))) []]
  versionImpl <- funD 'ifaceVersionT [clause [wildP] (normalB (litE (integerL (fromIntegral v)))) []]
  mkEntryImpl <- funD 'mkEntry [clause [] (normalB (varE mkEntryName)) []]
  inst <-
    instanceD
      (cxt [])
      [t|InterfaceType $ifaceTy|]
      [pure tyInst, pure nameImpl, pure versionImpl, pure mkEntryImpl]
  pure [inst]

--------------------------------------------------------------------------------
-- Requests
--------------------------------------------------------------------------------

generateIfaceReqs :: Interface -> Q [Dec]
generateIfaceReqs Interface{ifaceName = n, ifaceRequests = reqs} = do
  let uName = unpack . toCamelU $ n
      lName = transformFstStr C.toLower uName
  flattenQ $ zipWith (generateReq uName lName) [0 ..] reqs

{- | Dispatches to one of three shapes depending on the request's
new_id argument (if any):

  * no new_id             -> plain request, returns @W ()@
  * new_id, known iface   -> allocates + registers + returns @W (Object Child)@
  * new_id, no iface      -> bind-style: polymorphic in the caller-chosen
                             interface, takes an explicit version
-}
generateReq :: String -> String -> Int -> Request -> Q [Dec]
generateReq uName lName opcode Request{reqName = n, reqArguments = args} = do
  let rName = mkName (lName ++ (unpack . toCamelU $ n))
      (newIdArgs, restArgs) = L.partition isNewID args
      position = findIndex isNewID args
  case (position, newIdArgs) of
    (Nothing, _) -> do
      sig <- sigD rName (generateSimpleReqType uName args)
      fun <- funD rName [generateSimpleClause opcode args]
      pure [sig, fun]
    (Just i, [Argument{argType = TypeNewId (Just childIfaceText)}]) -> do
      sig <- sigD rName (generateSpawnReqType uName childIfaceText restArgs)
      fun <- funD rName [generateSpawnClause opcode i childIfaceText restArgs]
      pure [sig, fun]
    (Just i, [Argument{argType = TypeNewId Nothing}]) ->
      generateBindReq rName uName opcode i restArgs
    -- pure []
    _ -> error "waywire: multiple new_id arguments in a single request is not supported"

generateSimpleReqType :: String -> [Argument] -> Q Type
generateSimpleReqType uName args = buildArrow ([t|Object $(conT (mkName uName))|] : (generateArgType <$> args)) [t|W ()|]

generateSimpleClause :: Int -> [Argument] -> Q Clause
generateSimpleClause opcode args = do
  selfName <- newName "self"
  encoded <- sequence (generateEncodePattern <$> args)
  let (valueExps, argNames) = unzip encoded
      fds = generateFdList args argNames
      pats = varP selfName : (varP <$> argNames)
      msgExp = [|Message (unObject $(varE selfName)) (Opcode opcode) $(listE valueExps) $fds|]
  clause pats (normalB [|sendMessage (encodeMessage $msgExp)|]) []

generateSpawnReqType :: String -> Text -> [Argument] -> Q Type
generateSpawnReqType uName childIfaceText restArgs =
  let childTy = conT (mkName . unpack . toCamelU $ childIfaceText)
      selfTy = conT (mkName uName)
      handlersTy = [t|Handlers $childTy|]
   in buildArrow
        ([t|Object $selfTy|] : (generateArgType <$> restArgs) ++ [handlersTy])
        [t|W (Object $childTy)|]

{- | createSurface self ...otherArgs... handlers = do
    newId <- allocateNewId
    registerObject newId (mkEntry handlers)
    sendMessage (encodeMessage (Message (unObject self) (Opcode opcode) [..args with ValueNewId newId spliced back in..] []))
    pure (Object newId)
-}
generateSpawnClause :: Int -> Int -> Text -> [Argument] -> Q Clause
generateSpawnClause opcode newIdPos _childIfaceText restArgs = do
  selfName <- newName "self"
  handlersName <- newName "handlers"
  newIdName <- newName "newId"
  encoded <- sequence (generateEncodePattern <$> restArgs)
  let (restExps, restNames) = unzip encoded
      fds = generateFdList restArgs restNames
      pats = varP selfName : (varP <$> restNames) ++ [varP handlersName]
      newIdValueExp = [|ValueNewId $(varE newIdName)|]
      allExps = insertAt newIdPos newIdValueExp restExps
      bodyStmts =
        [ bindS (varP newIdName) [|allocateNewId|]
        , noBindS [|registerObject $(varE newIdName) (mkEntry $(varE handlersName))|]
        , noBindS [|sendMessage (encodeMessage (Message (unObject $(varE selfName)) (Opcode opcode) $(listE allExps) $fds))|]
        , noBindS [|pure (Object $(varE newIdName))|]
        ]
  clause pats (normalB (doE bodyStmts)) []

{- | bindWlRegistryBind :: forall i. Interface i =>
    Object WlRegistry -> Word32 {\- name -\} -> Word32 {\- version -\} -> Handlers i -> W (Object i)
bindWlRegistryBind self name version handlers = do
  newId <- allocateNewId
  registerObject newId (mkEntry handlers)
  sendMessage (encodeMessage (Message (unObject self) (Opcode opcode)
    [ValueUInt name, ValueNewIdUntyped (ifaceNameT (Proxy :: Proxy i)) version newId] []))
  pure (Object newId)
-}
generateBindReq :: TH.Name -> String -> Int -> Int -> [Argument] -> Q [Dec]
generateBindReq rName uName opcode newIdPos restArgs = do
  iName <- newName "i"
  selfName <- newName "self"
  versionName <- newName "version"
  newIdName <- newName "newId"
  handlersName <- newName "handlers"
  encoded <- sequence (generateEncodePattern <$> restArgs)
  let (restExps, restNames) = unzip encoded
      fds = generateFdList restArgs restNames
      selfTy = conT (mkName uName)
      resultTy = [t|W (Object $(varT iName))|]
      allArgTys =
        [t|Object $selfTy|]
          : (generateArgType <$> restArgs)
          ++ [ [t|Word32|] -- version, since it isn't known statically for this new_id
             , [t|Handlers $(varT iName)|] -- the caller's handlers, which pin down `i`
             ]
      arrowTy = foldr (\a r -> [t|$a -> $r|]) resultTy allArgTys
  fullTy <- forallT [plainTV iName] (cxt [[t|InterfaceType $(varT iName)|]]) arrowTy
  sig <- sigD rName (pure fullTy)

  let pats = varP selfName : (varP <$> restNames) ++ [varP versionName, varP handlersName]
      proxyExp = sigE (conE 'Proxy) [t|Proxy $(varT iName)|]
      newIdValueExp =
        [|ValueNewIdUntyped (ifaceNameT $proxyExp) $(varE versionName) $(varE newIdName)|]
      allExps = insertAt newIdPos newIdValueExp restExps
      msgExp =
        [|Message (unObject $(varE selfName)) (Opcode opcode) $(listE allExps) $fds|]
      bodyStmts =
        [ bindS (varP newIdName) [|allocateNewId|]
        , noBindS [|registerObject $(varE newIdName) (mkEntry $(varE handlersName))|]
        , noBindS [|sendMessage (encodeMessage $msgExp)|]
        , noBindS [|pure (Object $(varE newIdName))|]
        ]
  fun <- funD rName [clause pats (normalB (doE bodyStmts)) []]
  pure [sig, fun]

generateFdList :: [Argument] -> [TH.Name] -> Q Exp
generateFdList exps names = do
  let isFd (Argument{argType = TypeFileDescriptor}, _) = True
      isFd _ = False
      fds = varE . snd <$> filter isFd (zip exps names)
  listE fds

--------------------------------------------------------------------------------
-- Enums
--------------------------------------------------------------------------------

generateIfaceEnums :: Interface -> Q [Dec]
generateIfaceEnums Interface{ifaceName = n, ifaceEnums = enums} = do
  let uName = unpack . toCamelU $ n
  flattenQ $ generateEnum uName <$> enums

generateEnum :: String -> Enum' -> Q [Dec]
generateEnum uName Enum'{enumName = n, enumEntries = entries} = do
  let eName = (uName ++) . unpack . toCamelU $ n
      entryNames = ((\e -> normalC e []) . mkName . (eName ++) . unpack . toCamelU . enumEntryName) <$> entries
  dec <- dataD (cxt []) (mkName (eName ++ "Enum")) [] Nothing entryNames []
  pure [dec]

generateDecodePattern :: Argument -> Q (Q Pat, TH.Name)
generateDecodePattern Argument{argType = t} = do
  nameX <- newName "x"
  let x = varP nameX
      r = case t of
        TypeInt -> [p|ValueInt $x|]
        TypeUInt -> [p|ValueUInt $x|]
        TypeFixed -> [p|ValueFixed $x|]
        TypeString False -> [p|ValueString (Just $x)|]
        TypeString True -> [p|ValueString $x|]
        TypeFileDescriptor -> [p|ValueFd $x|]
        TypeArray -> [p|ValueArray $x|]
        TypeObject _ _ -> [p|ValueObject $x|]
        TypeNewId _ -> [p|ValueNewId $x|]
  pure (r, nameX)

generateEncodePattern :: Argument -> Q (Q Exp, TH.Name)
generateEncodePattern Argument{argType = t} = do
  nameX <- newName "x"
  let x = varE nameX
      r = case t of
        TypeInt -> [|ValueInt $x|]
        TypeUInt -> [|ValueUInt $x|]
        TypeFixed -> [|ValueFixed $x|]
        TypeString False -> [|ValueString (Just $x)|]
        TypeString True -> [|ValueString $x|]
        TypeFileDescriptor -> [|ValueFd $x|]
        TypeArray -> [|ValueArray $x|]
        TypeObject (Just _) False -> [|ValueObject (unObject $x)|]
        TypeObject (Just _) True -> [|ValueObject (maybe (ObjectId 0) unObject $x)|]
        TypeObject Nothing _ -> error "Request objects need a interface name"
        TypeNewId _ -> [|ValueNewId $x|]
  pure (r, nameX)

generateArgType :: Argument -> Q Type
generateArgType Argument{argType = t} = case t of
  TypeInt -> [t|Int32|]
  TypeUInt -> [t|Word32|]
  TypeFixed -> [t|Fixed|] -- Alias for Int32
  TypeString False -> [t|Text|]
  TypeString True -> [t|Maybe Text|]
  TypeFileDescriptor -> [t|Fd|]
  TypeArray -> [t|ByteString|]
  TypeObject (Just iface) False -> [t|Object $(uName iface)|]
  TypeObject (Just iface) True -> [t|Maybe (Object $(uName iface))|]
  TypeObject Nothing False -> [t|ObjectId|]
  TypeObject Nothing True -> [t|Maybe ObjectId|]
  TypeNewId Nothing -> error "Events should not have this, requests should have had this striped out"
  TypeNewId (Just iface) -> [t|Object $(uName iface)|]
 where
  uName = conT . mkName . unpack . toCamelU

transformFst :: (Char -> Char) -> T.Text -> T.Text
transformFst f t = case uncons t of
  Nothing -> ""
  Just (c, t') -> cons (f c) t'

transformFstStr :: (a -> a) -> [a] -> [a]
transformFstStr f str = case str of
  [] -> []
  (x : xs) -> f x : xs

toCamelU :: Text -> Text
toCamelU t = T.concat $ transformFst C.toUpper <$> splitOn "_" t

toCamelL :: Text -> Text
toCamelL t = transformFst C.toLower $ toCamelU t

flattenQ :: [Q [a]] -> Q [a]
flattenQ = fmap concat . sequence

notWrittenYetExp :: Q Exp
notWrittenYetExp = [|error "not written yet"|]

buildArrow :: [Q Type] -> Q Type -> Q Type
buildArrow argTys result = foldr (\a r -> [t|$a -> $r|]) result argTys

insertAt :: Int -> a -> [a] -> [a]
insertAt n e xs = let (before, after) = splitAt n xs in before ++ (e : after)
