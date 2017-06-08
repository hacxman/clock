{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module Clock.SSD1306 where

import Ivory.Language
import Ivory.Tower
--import Ivory.HW.Module

import Ivory.Tower.HAL.Bus.Interface
import Ivory.BSP.STM32.Driver.I2C
--import Ivory.BSP.STM32.Peripheral.I2C (i2cName)
import Clock.Fonts
--import Data.Bits ((.|.))
import Data.Char (ord)
--import BSP.Tests.UART.Buffer
--import BSP.Tests.UART.Types
import Ivory.Stdlib
import Ivory.Language.Uint
import Ivory.Language.Effects
import Ivory.Language.Monad

import Clock.Types

ssd1306_i2c_addr :: I2CDeviceAddr
ssd1306_i2c_addr = I2CDeviceAddr 0x3c

data OledChannels = OledChannels (ChanInput ('Struct "pixel"))
                         (ChanOutput ('Stored IBool))
                         (ChanInput ('Stored Uint8))
                         (ChanInput ('Stored IBool))
                         (ChanInput ('Stored IBool))

data OledEmitters = OledEmitters
             ((Emitter ('Struct "pixel")))
             ((Emitter ('Stored Uint8)))
             ((Emitter ('Stored IBool)))
             ((Emitter ('Stored IBool)))
oledEmitter :: OledChannels -> Handler b e OledEmitters
oledEmitter (OledChannels a b c d e) = do
  putpixel_e <- Ivory.Tower.emitter a 1
  putchar_e <- Ivory.Tower.emitter c 1
  blit_e <- Ivory.Tower.emitter d 1
  clr_e <- Ivory.Tower.emitter e 1
  return $ OledEmitters putpixel_e putchar_e blit_e clr_e

oled_putpixel
  :: forall s1 (s2 :: s) s3 (eff :: Ivory.Language.Effects.Effects).
     Ivory.Language.Effects.GetAlloc eff
     ~
     'Ivory.Language.Effects.Scope s2 =>
     OledEmitters
     -> Ivory.Language.Uint.Uint8
     -> Ivory.Language.Uint.Uint8
     -> Ivory.Language.Uint.Uint8
     -> Ivory.Language.Monad.Ivory eff ()
oled_putpixel (OledEmitters e _ _ _) xx yy cc = do
  p <- local $ istruct []
  store (p ~> x) $ xx .% 128
  store (p ~> y) $ yy .% 64
  store (p ~> pixel_c) cc
  Ivory.Tower.emit e $ constRef p

data SSD1306Cmd = EXTERNAL_VCC
                | SWITCH_CAP_VCC
                | SET_LOW_COLUMN
                | SET_HIGH_COLUMN
                | SET_MEMORY_MODE Uint8
                | SET_COL_ADDRESS Uint8 Uint8
                | SET_PAGE_ADDRESS Uint8 Uint8
                | RIGHT_HORIZ_SCROLL
                | LEFT_HORIZ_SCROLL
                | VERT_AND_RIGHT_HORIZ_SCROLL
                | VERT_AND_LEFT_HORIZ_SCROLL
                | DEACTIVATE_SCROLL
                | ACTIVATE_SCROLL
                | SET_START_LINE Uint8
                | SET_CONTRAST Uint8
                | CHARGE_PUMP Uint8
                | SEG_REMAP Uint8
                | SET_VERT_SCROLL_AREA
                | DISPLAY_ALL_ON_RESUME
                | DISPLAY_ALL_ON
                | NORMAL_DISPLAY
                | INVERT_DISPLAY
                | DISPLAY_OFF
                | DISPLAY_ON
                | COM_SCAN_INC
                | COM_SCAN_DEC
                | SET_DISPLAY_OFFSET Uint8
                | SET_COM_PINS Uint8
                | SET_VCOM_DETECT Uint8
                | SET_DISPLAY_CLOCK_DIV Uint8
                | SET_PRECHARGE Uint8
                | SET_MULTIPLEX Uint8

--MEMORY_MODE_HORIZ = 0x00
--MEMORY_MODE_VERT  = 0x01
--MEMORY_MODE_PAGE  = 0x02

ssd1306cmd_toint :: SSD1306Cmd -> Uint8
ssd1306cmd_toint EXTERNAL_VCC   = 0x1
ssd1306cmd_toint SWITCH_CAP_VCC = 0x2

ssd1306cmd_toint SET_LOW_COLUMN        = 0x00
ssd1306cmd_toint SET_HIGH_COLUMN       = 0x10
ssd1306cmd_toint (SET_MEMORY_MODE    x)= 0x20
ssd1306cmd_toint (SET_COL_ADDRESS    x y)   = 0x21
ssd1306cmd_toint (SET_PAGE_ADDRESS   x y)   = 0x22
ssd1306cmd_toint RIGHT_HORIZ_SCROLL    = 0x26
ssd1306cmd_toint LEFT_HORIZ_SCROLL     = 0x27
ssd1306cmd_toint VERT_AND_RIGHT_HORIZ_SCROLL = 0x29
ssd1306cmd_toint VERT_AND_LEFT_HORIZ_SCROLL = 0x2A
ssd1306cmd_toint DEACTIVATE_SCROLL     = 0x2E
ssd1306cmd_toint ACTIVATE_SCROLL       = 0x2F
ssd1306cmd_toint (SET_START_LINE    x) = 0x40
ssd1306cmd_toint (SET_CONTRAST      x) = 0x81
ssd1306cmd_toint (CHARGE_PUMP       x) = 0x8D
ssd1306cmd_toint (SEG_REMAP         x) = 0xA0
ssd1306cmd_toint SET_VERT_SCROLL_AREA  = 0xA3
ssd1306cmd_toint DISPLAY_ALL_ON_RESUME = 0xA4
ssd1306cmd_toint DISPLAY_ALL_ON        = 0xA5
ssd1306cmd_toint NORMAL_DISPLAY        = 0xA6
ssd1306cmd_toint INVERT_DISPLAY        = 0xA7
ssd1306cmd_toint DISPLAY_OFF           = 0xAE
ssd1306cmd_toint DISPLAY_ON            = 0xAF
ssd1306cmd_toint COM_SCAN_INC          = 0xC0
ssd1306cmd_toint COM_SCAN_DEC          = 0xC8
ssd1306cmd_toint (SET_DISPLAY_OFFSET x)= 0xD3
ssd1306cmd_toint (SET_COM_PINS       x)= 0xDA
ssd1306cmd_toint (SET_VCOM_DETECT    x)= 0xDB
ssd1306cmd_toint (SET_DISPLAY_CLOCK_DIV x) = 0xD5
ssd1306cmd_toint (SET_PRECHARGE      x)= 0xD9
ssd1306cmd_toint (SET_MULTIPLEX      x)= 0xA8


--puts :: (GetAlloc eff ~ 'Scope cs)
--     => Emitter ('Stored Uint8) -> String -> Ivory eff ()
--puts e str = mapM_ (\c -> putc e (fromIntegral (ord c))) str
--
--putc :: (GetAlloc eff ~ 'Scope cs)
--     => Emitter ('Stored Uint8) -> Uint8 -> Ivory eff ()
--putc = Ivory.Tower.emitV


ssd1306Tower :: BackpressureTransmit ('Struct "i2c_transaction_request")
                                     ('Struct "i2c_transaction_result")
             -> ChanOutput ('Stored ITime)
             -> I2CDeviceAddr
             -> Tower e OledChannels
ssd1306Tower (BackpressureTransmit req_chan res_chan) init_chan addr = do
  putpixel_chan <- channel
  blit_chan <- channel
  ready_chan <- channel
  putch_chan <- channel
  clr_chan <- channel
  monitor "ssd1306mon" $ do
    (framebuffer :: Ref 'Global ('Array 1024 ('Stored Uint8)))
      <- stateInit "framebuffer" (iarray [ival (0 :: Uint8) | _ <- [0..1024]])

    (font_mem :: Ref 'Global ('Array 1275 ('Stored Uint8)))
      <- stateInit "fontmem" $ iarray $ fmap ival font

    blit_state <- stateInit "blit_state" $ ival false

    handler (snd putpixel_chan) "ssd1306_putpixel" $ do
      req_e <- Ivory.Tower.emitter req_chan 1
      blit_e <- Ivory.Tower.emitter (fst blit_chan) 1
      callback $ \p -> do
        xpos <- p ~>* x
        ypos <- p ~>* y
        color <- p ~>* pixel_c
        let row :: Uint16
            row = (safeCast ypos) `iDiv` 8
            index :: Uint16
            index = (safeCast xpos) * 32 + row
        current_pix <- deref $ framebuffer ! (toIx index)
        when (color ==? 1) $ do
          let newpix = current_pix .| (1 `iShiftL` (xpos .% 8))
          store (framebuffer ! (toIx index)) newpix
        when (color ==? 0) $ do
          let newpix = current_pix .& (iComplement (1 `iShiftL` (xpos .% 8)))
          store (framebuffer ! (toIx index)) newpix

    text_pos <- stateInit "text_pos" $ ival (0 :: Sint32)
    handler (snd putch_chan) "ssd1306_putch" $ do
      req_e <- Ivory.Tower.emitter req_chan 1
      blit_e <- Ivory.Tower.emitter (fst blit_chan) 1
      callbackV $ \ch -> do
        text_pos_d <- deref text_pos
        arrayMap $ \(i :: Ix 5) -> do
          c <- deref $ font_mem ! (toIx $ (signCast :: Uint32 -> Sint32) (((safeCast :: Uint8 -> Uint32) ch)*5
                      + ((bitCast :: Uint32 -> Uint32) $ signCast $ fromIx i)))
          oc <- deref (framebuffer ! (toIx (8*(text_pos_d*6+(fromIx i)))))
          store (framebuffer ! (toIx (8*(text_pos_d*6+(fromIx i))))) (oc .| c)
        Ivory.Tower.emitV blit_e true
        text_pos %= (+1)


    handler (snd blit_chan) "blit" $ do
      req_e <- Ivory.Tower.emitter req_chan 1
      callback $ const $ do
        store blit_state true
        i2c_req_i addr 0 >>= Ivory.Tower.emit req_e

    handler (snd clr_chan) "clear" $ do
      req_e <- Ivory.Tower.emitter req_chan 1
      callback $ const $ do
        arrayMap $ \(i :: Ix 1024) -> do
          store (framebuffer ! i) 0


    coroutineHandler init_chan res_chan "ssd1306" $ do
      req_e <- Ivory.Tower.emitter req_chan 1
      ready_e <- Ivory.Tower.emitter (fst ready_chan) 1
      return $ CoroutineBody $ \ yield -> do
        let em d = i2c_req addr d >>= Ivory.Tower.emit req_e >> yield in do
          em DISPLAY_OFF
          -- TODO: FIXME: display goes wrong with this line
--          em $ SET_DISPLAY_CLOCK_DIV 0x80
          em $ SET_MULTIPLEX 0x3f
          em $ SET_COM_PINS 0x12
          em $ SET_DISPLAY_OFFSET 0
          em $ SET_START_LINE 0
          em $ CHARGE_PUMP 0x14
          em $ SET_MEMORY_MODE 0x00
          em $ SEG_REMAP 0x01
          em COM_SCAN_DEC
          em $ SET_CONTRAST 0x8f
          em $ SET_PRECHARGE 0xf1
          em $ SET_VCOM_DETECT 0x40
          em DISPLAY_ALL_ON_RESUME
          em NORMAL_DISPLAY
          em DISPLAY_ON
          em DEACTIVATE_SCROLL

          ssd_clear req_e addr yield
          Ivory.Tower.emitV ready_e true

          forever $ do
            blit_st <- deref blit_state
            when (blit_st) $ do
              store blit_state false
              let em d = i2c_req addr d >>= Ivory.Tower.emit req_e >> yield in do
                em $ SET_MEMORY_MODE 0x1
                em $ SET_PAGE_ADDRESS 0x0 7
                em $ SET_COL_ADDRESS 0x0 127

                arrayMap $ \(ixx :: Ix 1024) -> noBreak $ do
                  pix <- deref $ framebuffer ! ixx
                  i2c_data_1 addr pix >>= Ivory.Tower.emit req_e >> yield
            yield
            return ()
          return ()
  return $ OledChannels (fst putpixel_chan) (snd ready_chan) (fst putch_chan) (fst blit_chan) (fst clr_chan)


ssd_clear req_e addr yield = do
  let em d = i2c_req addr d >>= Ivory.Tower.emit req_e >> yield in do
    em $ SET_MEMORY_MODE 0x1
    em $ SET_PAGE_ADDRESS 0x0 7
    em $ SET_COL_ADDRESS 0x0 127

    arrayMap $ \(ixx :: Ix 1024) -> noBreak $ do
      i2c_data_1 addr 0x00 >>= Ivory.Tower.emit req_e >> yield

    return ()

-----------------------------
-- I2C stuff
-----------------------------

i2c_read_1 addr = fmap constRef $ local $ istruct
                [ tx_addr .= ival addr
                , tx_buf .= iarray []
                , tx_len .= ival 0
                , rx_len .= ival 1
                ]

i2c_data_1 addr dat = fmap constRef $ local $ istruct
                    [ tx_addr .= ival addr
                    , tx_buf  .= iarray [ival 0x40, ival dat]
                    , tx_len  .= ival (1+1)
                    , rx_len  .= ival 0
                    ]
i2c_req :: (GetAlloc eff ~ 'Scope s)
        => I2CDeviceAddr
        -> SSD1306Cmd
        -> Ivory eff (ConstRef ('Stack s) ('Struct "i2c_transaction_request"))
i2c_req addr cmd@(SET_MULTIPLEX x) = i2c_req_1 addr cmd x
i2c_req addr cmd@(SET_COM_PINS x) = i2c_req_1 addr cmd x
i2c_req addr cmd@(SET_DISPLAY_OFFSET x) = i2c_req_1 addr cmd x
i2c_req addr cmd@(SET_START_LINE x) = i2c_req_i addr (ssd1306cmd_toint cmd .| x)
i2c_req addr cmd@(SEG_REMAP x) = i2c_req_i addr (ssd1306cmd_toint cmd .| x)
i2c_req addr cmd@(CHARGE_PUMP x) = i2c_req_1 addr cmd x
i2c_req addr cmd@(SET_MEMORY_MODE x) = i2c_req_1 addr cmd x
i2c_req addr cmd@(SET_CONTRAST x) = i2c_req_1 addr cmd x
i2c_req addr cmd@(SET_PRECHARGE x) = i2c_req_1 addr cmd x
i2c_req addr cmd@(SET_VCOM_DETECT x) = i2c_req_1 addr cmd x
i2c_req addr cmd@(SET_PAGE_ADDRESS x y) = i2c_req_2 addr cmd x y
i2c_req addr cmd@(SET_COL_ADDRESS x y) = i2c_req_2 addr cmd x y
i2c_req addr cmd = i2c_req_0 addr cmd

i2c_req_i addr cmd = fmap constRef $ local $ istruct
                    [ tx_addr .= ival addr
                    , tx_buf  .= iarray [ival 0x00, ival cmd ]
                    , tx_len  .= ival 2
                    , rx_len  .= ival 0
                    ]

i2c_req_0 addr cmd = fmap constRef $ local $ istruct
                    [ tx_addr .= ival addr
                    , tx_buf  .= iarray [ival 0x00, ival $ ssd1306cmd_toint cmd ]
                    , tx_len  .= ival 2
                    , rx_len  .= ival 0
                    ]

i2c_req_1 addr cmd dat = fmap constRef $ local $ istruct
                    [ tx_addr .= ival addr
                    , tx_buf  .= iarray [ival 0x00, ival $ ssd1306cmd_toint cmd
                                        , ival dat ]
                    , tx_len  .= ival 3
                    , rx_len  .= ival 0
                    ]

i2c_req_2 addr cmd dat1 dat2 = fmap constRef $ local $ istruct
                    [ tx_addr .= ival addr
                    , tx_buf  .= iarray [ival 0x00, ival $ ssd1306cmd_toint cmd
                                        , ival dat1, ival dat2 ]
                    , tx_len  .= ival 4
                    , rx_len  .= ival 0
                    ]
