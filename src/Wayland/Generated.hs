{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Wayland.Generated where

import Wayland.TH

$(testProtocol "files/wayland.xml")
