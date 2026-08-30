{-# LANGUAGE OverloadedStrings #-}

module Wayland.Protocol.Parser (
  parseProtocol,
  parseInterface,
) where

import Data.Text as T
import Text.Read (readEither)
import Text.XML
import Text.XML.Cursor
import Wayland.Protocol

-- TODO copyright and description parsing formats

data ParseError
  = MissingAttribute Text
  | InvalidAttribute Text Text
  | UnknownArgumentType Text
  deriving (Eq, Show)

readInt :: Text -> Text -> Either ParseError Int
readInt name txt = case (readEither . T.unpack) txt of
  Right i -> Right i
  Left e -> Left $ InvalidAttribute name (T.pack e)

readBool :: Text -> Text -> Either ParseError Bool
readBool name value =
  case value of
    "true" -> Right True
    "false" -> Right False
    _ -> Left $ InvalidAttribute name value

parseProtocol :: Cursor -> Either ParseError Protocol
parseProtocol c =
  Protocol
    <$> parseName c
    <*> pure copyright
    <*> pure (parseDescription c)
    <*> parseChildren "interface" parseInterface c
 where
  copyright = case c $/ element "copyright" of
    (copyCursor : _) -> Just $ T.strip . T.concat $ copyCursor $/ content
    [] -> Nothing

parseInterface :: Cursor -> Either ParseError Interface
parseInterface c =
  Interface
    <$> parseName c
    <*> version
    <*> pure (parseDescription c)
    <*> parseChildren "request" parseRequest c
    <*> parseChildren "event" parseEvent c
    <*> parseChildren "enum" parseEnum c
 where
  version = case reqAttr "version" c of
    Left e -> Left e
    Right vt -> readInt "version" vt

parseRequest :: Cursor -> Either ParseError Request
parseRequest c =
  Request
    <$> parseName c
    <*> pure (parseDescription c)
    <*> parseSince c
    <*> parseDeprecated c
    <*> pure destruct
    <*> parseChildren "arg" parseArgument c
 where
  destruct = case optAttr "type" c of
    Just "destructor" -> True
    _ -> False

parseEvent :: Cursor -> Either ParseError Event
parseEvent c =
  Event
    <$> parseName c
    <*> pure (parseDescription c)
    <*> parseSince c
    <*> parseDeprecated c
    <*> pure destruct
    <*> parseChildren "arg" parseArgument c
 where
  destruct = case optAttr "type" c of
    Just "destructor" -> True
    _ -> False

parseArgument :: Cursor -> Either ParseError Argument
parseArgument c =
  Argument
    <$> parseName c
    <*> pure (parseDescription c)
    <*> pure (parseSummary c)
    <*> parsedType
    <*> pure (parseEnumRef <$> optAttr "enum" c)
 where
  interface = optAttr "interface" c
  isNullable = case optAttr "allow-null" c of
    Nothing -> Right False
    Just b -> readBool "allow-null" b
  parsedType = do
    typeName <- reqAttr "type" c
    parseArgType typeName interface isNullable

parseArgType :: Text -> Maybe Text -> Either ParseError Bool -> Either ParseError ArgType
parseArgType _ _ (Left e) = Left e
parseArgType "int" _ _ = Right TypeInt
parseArgType "uint" _ _ = Right TypeUInt
parseArgType "fixed" _ _ = Right TypeFixed
parseArgType "string" _ (Right null') = Right $ TypeString null'
parseArgType "array" _ _ = Right TypeArray
parseArgType "fd" _ _ = Right TypeFileDescriptor
parseArgType "object" iface (Right null') = Right $ TypeObject iface null'
parseArgType "new_id" iface _ = Right $ TypeNewId iface
parseArgType unknown _ _ = Left $ UnknownArgumentType unknown

parseEnumRef :: Text -> EnumRef
parseEnumRef name = case T.splitOn "." name of
  [iface, enumName'] -> ExternalEnum iface enumName'
  _ -> LocalEnum name

parseEnum :: Cursor -> Either ParseError Enum'
parseEnum c =
  Enum'
    <$> parseName c
    <*> pure (parseDescription c)
    <*> parseSince c
    <*> bit
    <*> parseChildren "entry" parseEnumEntry c
 where
  bit = case optAttr "bitfield" c of
    Nothing -> Right False
    Just b -> readBool "bitfield" b

parseEnumEntry :: Cursor -> Either ParseError EnumEntry
parseEnumEntry c =
  EnumEntry
    <$> parseName c
    <*> value
    <*> pure (parseDescription c)
    <*> pure (parseSummary c)
    <*> parseSince c
    <*> parseDeprecated c
 where
  value = case reqAttr "value" c of
    Left e -> Left e
    Right vt -> readInt "value" vt

parseDescription :: Cursor -> Maybe Description
parseDescription c = case c $/ element "description" of
  (descCursor : _) ->
    let summary = parseSummary descCursor
        textVal = T.strip . T.concat $ descCursor $/ content
     in Just $ Description summary textVal
  [] -> Nothing

parseSummary :: Cursor -> Maybe Text
parseSummary = optAttr "summary"

parseName :: Cursor -> Either ParseError Text
parseName = reqAttr "name"

parseSince :: Cursor -> Either ParseError Int
parseSince c = maybe (Right 1) (readInt "since") $ optAttr "since" c

parseDeprecated :: Cursor -> Either ParseError (Maybe Int)
parseDeprecated c = case optAttr "deprecated-since" c of
  Nothing -> Right Nothing
  Just t -> case readInt "deprecated-since" t of
    Left e -> Left e
    Right i -> Right $ Just i

parseChildren :: Name -> (Cursor -> Either ParseError a) -> Cursor -> Either ParseError [a]
parseChildren name parser c = traverse parser (c $/ element name)

optAttr :: Name -> Cursor -> Maybe Text
optAttr name c = case attribute name c of
  (val : _) -> Just val
  [] -> Nothing

reqAttr :: Name -> Cursor -> Either ParseError Text
reqAttr name c = case attribute name c of
  (val : _) -> Right val
  [] -> Left $ MissingAttribute (nameLocalName name)
