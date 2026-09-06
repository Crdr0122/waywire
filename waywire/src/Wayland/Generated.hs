{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Wayland.Generated where

import Wayland.TH

$(generateModule "files/wayland.xml")
