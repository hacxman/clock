{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clock.Types where

import Ivory.Language
--import Ivory.HW
import Ivory.Tower

[ivory|
struct pixel
  { x :: Stored Uint8
  ; y :: Stored Uint8
  ; pixel_c :: Stored Uint8
  }
|]

pixelTypes :: Module
pixelTypes = package "pixel_types" $ do
  defStruct (Proxy :: Proxy "pixel")
