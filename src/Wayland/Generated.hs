{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Wayland.Generated where

import Control.Monad (filterM, forM, forM_)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Directory
import System.FilePath ((</>))
import Text.XML
import Text.XML.Cursor
import Wayland.Protocol
import Wayland.Protocol.Parser
import Wayland.TH

$(generateModule "files/wayland.xml")

generateModules :: FilePath -> Q [Dec]
generateModules fp = do
  allFilesWithoutDir <- runIO $ listDirectory fp
  let allFiles = (fp </>) <$> allFilesWithoutDir
  files <- runIO $ filterM doesFileExist allFiles
  forM_ files addDependentFile
  maybeProtocols <- sequence $ single <$> files
  let (ets, protocols) = unzip $ catMaybes maybeProtocols
      table = M.unions (waylandXmlEnumTable : ets)
  concat <$> (forM protocols $ generateProtocol table)
 where
  single f = do
    fileContent <- runIO $ Text.XML.readFile def f
    case parseProtocol $ fromDocument fileContent of
      Right a -> let et = buildEnumTable a in pure (Just (et, a))
      Left _ -> pure Nothing
