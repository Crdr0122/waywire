{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Wayland.TH where

import Data.ByteString (ByteString)
import Data.Char as C
import Data.Int (Int32)
import Data.List qualified as L
import Data.Text (Text, cons, splitOn, uncons, unpack)
import Data.Text qualified as T
import Data.Typeable
import Data.Word (Word32)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (addDependentFile, lift)
import System.Posix.Types (Fd)
import Text.XML
import Text.XML.Cursor
import Wayland.Protocol
import Wayland.Protocol.Parser
import Wayland.Types

testProtocol :: Q [Dec]
testProtocol = do
  addDependentFile "files/test.xml"
  fileContent <- runIO $ Text.XML.readFile def "files/test.xml"
  case parseProtocol $ fromDocument fileContent of
    Right a -> generateProtocol a
    Left _ -> pure []

generateProtocol :: Protocol -> Q [Dec]
generateProtocol Protocol{protoInterfaces = ifaces} = do
  types <- flattenQ $ generateIfaceType <$> ifaces
  dispatcheRecords <- flattenQ $ generateIfaceDispatch <$> ifaces
  events <- flattenQ $ generateIfaceEvents <$> ifaces
  reqs <- flattenQ $ generateIfaceReqs <$> ifaces
  enums <- flattenQ $ generateIfaceEnums <$> ifaces
  pure $ types ++ dispatcheRecords ++ reqs ++ events ++ enums

generateIfaceType :: Interface -> Q [Dec]
generateIfaceType Interface{ifaceName = n} = do
  let hsName = mkName . unpack . toCamelU $ n
  dec <- dataD (cxt []) hsName [] Nothing [] []
  pure [dec]

generateIfaceDispatch :: Interface -> Q [Dec]
generateIfaceDispatch Interface{ifaceName = n, ifaceVersion = i} = do
  let eName = mkName . (++ "Dispatch") . unpack . toCamelL $ n
      dName = mkName . ("decode" ++) . (++ "Event") . unpack . toCamelU $ n
      decodeField = fieldExp 'interfaceDecodeEvent (varE dName)
      nameField = fieldExp 'interfaceName (litE . stringL . T.unpack $ n)
      versionField = fieldExp 'interfaceVersion (litE . integerL . fromIntegral $ i)
      encodeField = fieldExp 'interfaceEncodeRequest [|error "not implemented yet"|]
      body = normalB (recConE 'InterfaceType [nameField, versionField, decodeField, encodeField])
  sig <- sigD eName [t|InterfaceType|]
  dec <- funD eName [clause [] body []]
  pure [sig, dec]

generateIfaceReqs :: Interface -> Q [Dec]
generateIfaceReqs Interface{ifaceName = n, ifaceRequests = reqs} = do
  let uName = unpack . toCamelU $ n
      lName = transformFstStr C.toLower uName
  flattenQ $ generateReq uName lName <$> reqs

generateReq :: String -> String -> Request -> Q [Dec]
generateReq uName lName Request{reqName = n, reqArguments = args} = do
  let rName = mkName . (lName ++) . unpack . toCamelU $ n
      uType = [t|Object $(conT $ mkName uName)|]
      (newID, rest) = L.partition isNewID args
      argTypes = uType : (generateArgType <$> rest)
      resultType = case newID of -- TODO Additional internal function that remembers where newID is
        [] -> [t|IO ()|]
        [x] -> [t|IO ($(generateArgType x))|]
        _ -> error "More than one new_id arg"
      argWithTypes = foldr (\arg res -> [t|$arg -> $res|]) resultType argTypes
  sig <- sigD rName argWithTypes
  fun <- funD rName [clause [] (normalB notWrittenYetExp) []]
  pure [sig, fun]

generateIfaceEvents :: Interface -> Q [Dec]
generateIfaceEvents Interface{ifaceName = n, ifaceEvents = events} = do
  let uName = unpack . toCamelU $ n
      eventName = mkName (uName ++ "Event")
  (conList, funs) <- unzip <$> (sequence $ generateEvent uName <$> events)
  dec <- dataD (cxt []) eventName [] Nothing conList [derivClause Nothing [conT ''Show, conT ''Typeable]]
  let clauses = zipWith (\i f -> f i) [0 ..] funs
      fallBackClause = clause [wildP, wildP] (normalB $ (appE $ conE 'Left) $ conE 'ValueToEventFailure) []
      decoderName = mkName ("decode" ++ uName ++ "Event")
  decoders <- funD decoderName (clauses ++ [fallBackClause])
  decoderSig <- sigD decoderName [t|Opcode -> [Value] -> Either DecodeError SomeEvent|]
  pure [dec, decoderSig, decoders]

generateEventDecoder :: TH.Name -> ([Q Pat], [Q Exp]) -> Integer -> Q Clause
generateEventDecoder eName (pats, exps) i = do
  let opcode = [p|Opcode $(litP (integerL i))|]
      p = listP pats
      e = foldl' appE (conE eName) exps
  -- cl <- clause [opcode, p] (normalB $ (appE $ conE 'Right) $ e) []
  cl <- clause [opcode, p] (normalB [|Right (SomeEvent $e)|]) []
  pure cl

generateEvent :: String -> Event -> Q (Q Con, Integer -> Q Clause)
generateEvent uName Event{eventName = n, eventArguments = args} = do
  let eName = mkName . (uName ++) . unpack . toCamelU $ n
      argTypes = generateArgType <$> args
      argBangTypes = bangType (bang noSourceUnpackedness noSourceStrictness) <$> argTypes
  decodePats <- sequence $ generateDecodePattern <$> args
  let funs = generateEventDecoder eName $ unzip decodePats
  pure (normalC eName argBangTypes, funs)

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

generateDecodePattern :: Argument -> Q (Q Pat, Q Exp)
generateDecodePattern Argument{argType = t} = do
  nameX <- newName "x"
  let x = varP nameX
      (r, e) = case t of
        TypeInt -> ([p|ValueInt $x|], varE nameX)
        TypeUInt -> ([p|ValueUInt $x|], varE nameX)
        TypeFixed -> ([p|ValueFixed $x|], varE nameX)
        TypeString False -> ([p|ValueString (Just $x)|], varE nameX)
        TypeString True -> ([p|ValueString $x|], varE nameX)
        TypeFileDescriptor -> ([p|ValueFd|], [|(-1)|])
        TypeArray -> ([p|ValueArray $x|], varE nameX)
        TypeObject _ _ -> ([p|ValueObject $x|], varE nameX)
        TypeNewId _ -> ([p|ValueNewId $x|], varE nameX)
  pure (r, e)

generateArgType :: Argument -> Q Type
generateArgType Argument{argType = t} = case t of
  TypeInt -> [t|Int32|]
  TypeUInt -> [t|Word32|]
  TypeFixed -> [t|Fixed|] -- Alias for Int32
  TypeString False -> [t|Text|]
  TypeString True -> [t|Maybe Text|]
  TypeFileDescriptor -> [t|Fd|]
  TypeArray -> [t|ByteString|]
  TypeObject iface False -> [t|Object $(uName iface)|]
  TypeObject iface True -> [t|Maybe (Object $(uName iface))|]
  TypeNewId Nothing -> [t|NewObject|]
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
