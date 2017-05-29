{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Clock.Blink where

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
------------------------------

-- | LED Controller: Given a set of leds and a control channel of booleans,
--   setup the pin hardware, and turn the leds on when the control channel is
--   true.
ledController :: [LED] -> ChanOutput ('Stored IBool) -> Monitor e ()
ledController leds rxer = do
  -- Bookkeeping: this task uses Ivory.HW.Module.hw_moduledef
  monitorModuleDef $ hw_moduledef
  -- Setup hardware before running any event handlers
  handler systemInit "hardwareinit" $
    callback $ const $ mapM_ ledSetup leds
  -- Run a callback on each message posted to the channel
  handler rxer "newoutput" $ callback $ \outref -> do
    out <- deref outref
    -- Turn pins on or off according to event value
    ifte_ out
      (mapM_ ledOn  leds)
      (mapM_ ledOff leds)

-- | Blink task: Given a period and a channel source, output an alternating
--   stream of true / false on each period.
blinker :: Time a => a -> Tower e (ChanOutput ('Stored IBool))
blinker t = do
  p_chan <- period t
  (cin, cout) <- channel
  monitor "blinker" $ do
    lastled <- stateInit "lastled" (ival false)
    handler p_chan "per" $  do
      e <- emitter cin 1
      callback $ \timeref -> do
        time <- deref timeref
        -- Emit boolean value which will alternate each period.
        store lastled (time .% (2*p) <? p)
        emitV e (time .% (2*p) <? p)
  return cout
  where p = toITime t

blink :: Time a => a -> [LED] -> Tower p ()
blink per pins = do
  onoff <- blinker per
  monitor "led" $ ledController pins onoff

stringArray :: String -> [Uint8]
stringArray = map (fromIntegral . ord)

uartTestTypes :: Module
uartTestTypes = package "uartTestTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)

app :: (e -> ClockConfig)
    -> (e -> ColoredLEDs)
    -> (e -> TestI2C)
    -> (e -> TestUART)
    -> Tower e ()
app tocc toleds toi2c touart = do
  towerDepends uartTestTypes
  towerModule uartTestTypes

  towerDepends pixelTypes
  towerModule pixelTypes

  leds <- fmap toleds getEnv
  i2c <- fmap toi2c getEnv
  cc <- fmap tocc getEnv
  uart <- fmap touart getEnv
  blink (Milliseconds 137) [redLED leds]
  blink (Milliseconds 666) [blueLED leds]

  (i2cRequest, i2cReady) <- i2cTower tocc (testI2C i2c) (testI2CPins i2c)

  (buffered_ostream, istream, mon) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200
  ostream <- uartUnbuffer (buffered_ostream :: BackpressureTransmit UARTBuffer ('Stored IBool))
  monitor "dma" mon


  (putpixel_ch, ssd_ready, putch_ch, blit_ch) <- ssd1306Tower i2cRequest i2cReady ssd1306_i2c_addr
  per <- period (Milliseconds 300)
  monitor "putpixelmon" $ do
    go <- stateInit "go_state" $ ival false
    counter <- stateInit "counter" $ ival (0 :: Uint8)
    handler ssd_ready "go_state_handler" $ do
      putch_e <- emitter putch_ch 16
      callback $ const $ do
        store go true
        mapM_ (emitV putch_e) $stringArray "zec mi pec" --[80,69,78,73,83,32,105,110,32,105,118,111,114,121,254]

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
  --        store (p ~> x) $ (bitCast :: Uint64 -> Uint8) $ castDefault $ 30.0 .+30.0 .*(sin :: IDouble -> IDouble) $ safeCast time
          store (p ~> x) $ (counter_d * 2) .% 128
          store (p ~> y) $ (counter_d * 3) .% 64
          store (p ~> pixel_c) 0
          emit pp_e $ constRef p
          --store (p ~> x) $ (bitCast :: Uint64 -> Uint8) $ safeCast (time ./ 3000)
          --store (p ~> y) $ bitCast $ (time ./ 13000)
  --        p <- local $ istruct [ x .= ival $ time .% 128
  --                             , y .= ival $ time .% 64 ]
          p <- local $ istruct []
          store (p ~> x) $ ((counter_d+3) * 2) .% 128
          store (p ~> y) $ ((counter_d+3) * 3) .% 64
          store (p ~> pixel_c) 1
          emit pp_e $ constRef p
          emitV blit_e  true
  return ()

  --tasks <- forM exti2cs $ \ExternalSensor{..} -> do
  --  (t, req) <- task ext_sens_name
  --  ext_sens_init req i2cReady
  --  return t
  --Async.schedule (i2cName fmu24sens_ext_i2c_periph)
  --  tasks i2cReady i2cRequest (Milliseconds 1)


