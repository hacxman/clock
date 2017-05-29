{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Clock.OledTest where

import Ivory.Language
import Ivory.Tower
import Ivory.HW.Module

import Ivory.BSP.STM32.ClockConfig

import Clock.LED
import Clock.Platforms

import Ivory.Tower.HAL.Bus.Interface
import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Peripheral.I2C (i2cName)
import Data.Bits ((.|.))
import Data.Char (ord)
import BSP.Tests.UART.Buffer
import BSP.Tests.UART.Types
import Clock.Types
import Ivory.Stdlib
import Data.Char (ord)

import Clock.SSD1306


stringArray :: String -> [Uint8]
stringArray = map (fromIntegral . ord)

app :: (e -> ClockConfig)
    -> (e -> TestI2C)
    -> Tower e ()
app tocc toi2c = do
  towerDepends pixelTypes
  towerModule pixelTypes

  i2c <- fmap toi2c getEnv
  cc <- fmap tocc getEnv

  (i2cRequest, i2cReady) <- i2cTower tocc (testI2C i2c) (testI2CPins i2c)

  (putpixel_ch, ssd_ready, putch_ch, blit_ch) <- ssd1306Tower i2cRequest i2cReady ssd1306_i2c_addr
  per <- period (Milliseconds 300)
  monitor "putpixelmon" $ do
    go <- stateInit "go_state" $ ival false
    counter <- stateInit "counter" $ ival (0 :: Uint8)
    handler ssd_ready "go_state_handler" $ do
      putch_e <- emitter putch_ch 16
      callback $ const $ do
        store go true
        mapM_ (emitV putch_e) $stringArray "zec mi pec"

    handler per "putpixelmon_handler" $ do
      pp_e <- emitter putpixel_ch 32
      blit_e <- emitter blit_ch 16
      callback $ \timeref -> do
        go_d <- deref go
        time_d <- deref timeref
        counter_d <- deref counter
        counter %= (+1)
        p <- local $ istruct []
        when go_d $ do
          let
            time :: Uint64
            time  = signCast $ toIMicroseconds time_d
          store (p ~> x) $ (counter_d * 2) .% 128
          store (p ~> y) $ (counter_d * 3) .% 64
          store (p ~> pixel_c) 0
          emit pp_e $ constRef p

          p <- local $ istruct []
          store (p ~> x) $ ((counter_d+3) * 2) .% 128
          store (p ~> y) $ ((counter_d+3) * 3) .% 64
          store (p ~> pixel_c) 1
          emit pp_e $ constRef p
          emitV blit_e  true
  return ()
