{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Wayland.TH where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import Data.Char as C
import Data.Int (Int32)
import Data.List (findIndex)
import Data.List qualified as L
import Data.Text (Text, cons, splitOn, uncons, unpack)
import Data.Text qualified as T
import Data.Typeable
import Data.Word (Word32)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (addDependentFile)
import Network.Socket.ByteString.Lazy (sendAll)
import System.Posix.Types (Fd)
import Text.XML
import Text.XML.Cursor
import Wayland.Encode (encodeMessage)
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
      body = normalB (recConE 'InterfaceType [nameField, versionField, decodeField])
  sig <- sigD eName [t|InterfaceType|]
  dec <- funD eName [clause [] body []]
  pure [sig, dec]

generateIfaceReqs :: Interface -> Q [Dec]
generateIfaceReqs Interface{ifaceName = n, ifaceRequests = reqs} = do
  let uName = unpack . toCamelU $ n
      lName = transformFstStr C.toLower uName
  flattenQ $ zipWith (generateReq uName lName) [0 ..] reqs

generateReq :: String -> String -> Int -> Request -> Q [Dec]
generateReq uName lName opcode Request{reqName = n, reqArguments = args} = do
  let rName = mkName . (lName ++) . unpack . toCamelU $ n
      uType = [t|Object $(conT $ mkName uName)|]
      (newID, rest) = L.partition isNewID args
      position = findIndex isNewID args
      argTypes = uType : (generateArgType <$> rest)
      resultType = case newID of -- TODO Additional internal function that remembers where newID is
        [] -> [t|W ()|]
        [x] -> [t|W ($(generateArgType x))|]
        _ -> error "More than one new_id arg"
      argWithTypes = foldr (\arg res -> [t|$arg -> $res|]) resultType argTypes
  sig <- sigD rName argWithTypes
  fun <- funD rName [generateEncoder opcode position rest]
  pure [sig, fun]

generateEncoder :: Int -> Maybe Int -> [Argument] -> Q Clause
generateEncoder opcode position args = do
  argTypes <- sequence $ generateEncodePattern <$> args
  let obVar = varP (mkName "ob")
      (exps, pats) = unzip argTypes
      insert i e xs = let (before, after) = splitAt i xs in before ++ (e : after)
      (res, resBody) = case position of
        Nothing -> ([|()|], exps)
        Just i -> ([|(Object $ ObjectId 1)|], insert i [|ValueNewId (ObjectId 1)|] exps)
      p = obVar : (varP <$> pats)
      body =
        normalB
          [|
            do
              let m = Message (unObject ob) (Opcode opcode) $(listE resBody) []
                  bs = encodeMessage m
              socket <- asks connSocket
              liftIO $ sendAll socket bs
              pure $res
            |]
  clause p body []

-- encodeWlDisplaySyncRequest :: Object WlDisplay -> Int32 -> Object WlCallback -> IO Message
-- encodeWlDisplaySyncRequest display testInt callback =
--   pure $
--     Message
--       (unObject display) -- Use the actual object ID
--       (Opcode 0)
--       [ValueNewId (unObject callback), ValueInt testInt]
--       []

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

generateEventDecoder :: TH.Name -> ([Q Pat], [TH.Name]) -> Integer -> Q Clause
generateEventDecoder eName (pats, exps) i = do
  let opcode = [p|Opcode $(litP (integerL i))|]
      p = listP pats
      e = foldl' appE (conE eName) (varE <$> exps)
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
        TypeFileDescriptor -> [|ValueFd (-1)|]
        TypeArray -> [|ValueArray $x|]
        TypeObject _ _ -> [|ValueObject $x|]
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
