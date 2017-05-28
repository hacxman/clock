
module Main where

import Ivory.Tower.Config
import Ivory.Tower.Options
import Ivory.OS.FreeRTOS.Tower.STM32

import Clock.Platforms
import Clock.Blink (app)

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p $
  app testplatform_clockconfig testplatform_leds testplatform_i2c testplatform_uart
  where
  p :: TOpts -> IO TestPlatform
  p topts = getConfig topts testPlatformParser

