{-|
  Module      : $Header$
  Copyright   : (c) 2015 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable
  This module encapsulates types libFtdi library functions.
-}

{-# LANGUAGE Trustworthy #-}

module LibFtdi.Types ( FtdiChipType(..)
                     , FtdiParityType(..)
                     , FtdiStopBitsType(..)
                     , FtdiBitsType(..)
                     , FtdiBreakType(..)
                     , FtdiMPSSEMode(..)
                     , FtdiInterface(..)
                     , FtdiModuleDetactMode(..)
		     , FtdiEEPROMValue(..)
                     ) where


-- import Bindings.LibFtdi

-- import Foreign.C.Types
-- import Data.Word
-- import Data.Maybe
-- import Data.Tuple

-- import Data.Coerce
-- import GHC.Generics

-- | FTDI chip type
data FtdiChipType = TYPE_AM    -- ^ ..
                  | TYPE_BM    -- ^ ..
                  | TYPE_2232C -- ^ ..
                  | TYPE_R     -- ^ ..
                  | TYPE_2232H -- ^ ..
                  | TYPE_4232H -- ^ ..
                  | TYPE_232H  -- ^ ..
                  | TYPE_230X  -- ^ ..
                   deriving (Eq, Enum)

-- instance Enum FtdiChipType where
--   fromEnum = fromJust . flip lookup chiptypes
--   toEnum   = fromJust . flip lookup (map swap chiptypes)
--
-- chiptypes = [ (TYPE_AM,    C'TYPE_AM)
--             , (TYPE_BM,    C'TYPE_BM)
--             , (TYPE_2232C, C'TYPE_2232C)
--             , (TYPE_R,     C'TYPE_R)
--             , (TYPE_2232H, C'TYPE_2232H)
--             , (TYPE_4232H, C'TYPE_4232H)
--             , (TYPE_232H,  C'TYPE_232H)
--             , (TYPE_230X,  C'TYPE_230X)
--             ]

-- | Parity mode for ftdi_set_line_property()
data FtdiParityType = None
                    | Odd
                    | Even
                    | Mark
                    | Space
		     deriving (Eq, Enum)

-- | Number of stop bits for ftdi_set_line_property()
data FtdiStopBitsType = StopBit1
                      | StopBit15
                      | StopBit2
		       deriving (Eq, Enum)

-- | Number of bits for ftdi_set_line_property()
data FtdiBitsType = Bits7
                  | Bits8
		   deriving Eq

instance Enum FtdiBitsType where
   fromEnum Bits7 = 7
   fromEnum Bits8 = 8
   toEnum 7 = Bits7
   toEnum 8 = Bits8

-- | Break type for ftdi_set_line_property2()
data FtdiBreakType = BreakOff
                   | BreakOn
		    deriving (Eq, Enum)

-- | MPSSE bitbang modes
data FtdiMPSSEMode = BITMODE_RESET   -- ^ switch off bitbang mode, back to regular serial/FIFO
                   | BITMODE_BITBANG -- ^ classical asynchronous bitbang mode, introduced with B-type chips
                   | BITMODE_MPSSE   -- ^ MPSSE mode, available on 2232x chips
                   | BITMODE_SYNCBB  -- ^ synchronous bitbang mode, available on 2232x and R-type chips
                   | BITMODE_MCU     -- ^ MCU Host Bus Emulation mode, available on 2232x chips
-- CPU-style fifo mode gets set via EEPROM
                   | BITMODE_OPTO    -- ^ Fast Opto-Isolated Serial Interface Mode, available on 2232x chips
                   | BITMODE_CBUS    -- ^ Bitbang on CBUS pins of R-type chips, configure in EEPROM before
                   | BITMODE_SYNCFF  -- ^ Single Channel Synchronous FIFO mode, available on 2232H chips
                   | BITMODE_FT1284  -- ^ FT1284 mode, available on 232H chips
                    deriving Eq

instance Enum FtdiMPSSEMode where
  fromEnum BITMODE_RESET   = 0x00
  fromEnum BITMODE_BITBANG = 0x01
  fromEnum BITMODE_MPSSE   = 0x02
  fromEnum BITMODE_SYNCBB  = 0x04
  fromEnum BITMODE_MCU     = 0x08
  fromEnum BITMODE_OPTO    = 0x10
  fromEnum BITMODE_CBUS    = 0x20
  fromEnum BITMODE_SYNCFF  = 0x40
  fromEnum BITMODE_FT1284  = 0x80

-- | Port interface for chips with multiple interfaces
data FtdiInterface = INTERFACE_ANY
                   | INTERFACE_A
                   | INTERFACE_B
                   | INTERFACE_C
                   | INTERFACE_D
                    deriving (Eq, Enum)

-- | Automatic loading / unloading of kernel modules
data FtdiModuleDetactMode = AUTO_DETACH_SIO_MODULE
                          | DONT_DETACH_SIO_MODULE
                           deriving (Eq, Enum)


-- | List all handled EEPROM values.
data FtdiEEPROMValue = VENDOR_ID
                     | PRODUCT_ID
                     | SELF_POWERED
                     | REMOTE_WAKEUP
                     | IS_NOT_PNP
                     | SUSPEND_DBUS7
                     | IN_IS_ISOCHRONOUS
                     | OUT_IS_ISOCHRONOUS
                     | SUSPEND_PULL_DOWNS
                     | USE_SERIAL
                     | USB_VERSION
                     | USE_USB_VERSION
                     | MAX_POWER
                     | CHANNEL_A_TYPE
                     | CHANNEL_B_TYPE
                     | CHANNEL_A_DRIVER
                     | CHANNEL_B_DRIVER
                     | CBUS_FUNCTION_0
                     | CBUS_FUNCTION_1
                     | CBUS_FUNCTION_2
                     | CBUS_FUNCTION_3
                     | CBUS_FUNCTION_4
                     | CBUS_FUNCTION_5
                     | CBUS_FUNCTION_6
                     | CBUS_FUNCTION_7
                     | CBUS_FUNCTION_8
                     | CBUS_FUNCTION_9
                     | HIGH_CURRENT
                     | HIGH_CURRENT_A
                     | HIGH_CURRENT_B
                     | INVERT
                     | GROUP0_DRIVE
                     | GROUP0_SCHMITT
                     | GROUP0_SLEW
                     | GROUP1_DRIVE
                     | GROUP1_SCHMITT
                     | GROUP1_SLEW
                     | GROUP2_DRIVE
                     | GROUP2_SCHMITT
                     | GROUP2_SLEW
                     | GROUP3_DRIVE
                     | GROUP3_SCHMITT
                     | GROUP3_SLEW
                     | CHIP_SIZE
                     | CHIP_TYPE
                     | POWER_SAVE
                     | CLOCK_POLARITY
                     | DATA_ORDER
                     | FLOW_CONTROL
                     | CHANNEL_C_DRIVER
                     | CHANNEL_D_DRIVER
                     | CHANNEL_A_RS485
                     | CHANNEL_B_RS485
                     | CHANNEL_C_RS485
                     | CHANNEL_D_RS485
                     | RELEASE_NUMBER
                      deriving (Eq, Enum)
