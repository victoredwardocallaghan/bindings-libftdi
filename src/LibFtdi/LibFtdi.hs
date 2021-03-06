{-|
  Module      : $Header$
  Copyright   : (c) 2015 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates the foundational libftdi functions
  commonly used into a Monadic style. This Monadic style avoids
  passing around references of indirection to the device type and
  so on.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK prune #-}

module LibFtdi.LibFtdi ( withFtdi
		       , ftdiInit
		       , ftdiDeInit
                       , DeviceHandle(..)
                       , VendorID(..)
                       , ProductID(..)
                       , FtdiError(..)
                       , FtdiReturnType(..)
                       , ftdiErrorValue
                       , ftdiErrorTy
                       , ftdiSetInterface
		       , ftdiUSBOpen
                       , ftdiUSBOpenString
		       , ftdiUSBClose
                       , ftdiUSBReset
                       ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Control.Exception
import Data.Typeable (Typeable, cast)
import Data.Maybe
import Data.Tuple

import LibFtdi.Types

import Bindings.LibFtdi

-- | Vendor ID
type VendorID = Int

-- | Product ID
type ProductID = Int

-- | Error codes returned by internal libftdi functions.
data FtdiError = FTDI_ERR_ALLOC_READ_BUF   -- ^ Could not allocate read buffer
               | FTDI_ERR_ALLOC_STRUCT_BUF -- ^ Coult not allocate struct buffer
               | FTDI_ERR_INIT_FAILED      -- ^ libusb_init() failed
                deriving (Eq, Typeable)

instance Show FtdiError where
  show FTDI_ERR_ALLOC_READ_BUF   = "Could not allocate read buffer"
  show FTDI_ERR_ALLOC_STRUCT_BUF = "Coult not allocate struct buffer"
  show FTDI_ERR_INIT_FAILED      = "libusb_init() failed"

instance Exception FtdiError

-- | Returned C Error codes
--
-- ftdi library routines return negative values to indicate errors.
-- Values >= 0 are used to indicate success.
instance Enum FtdiError where
  fromEnum = fromJust . flip lookup errors
  toEnum   = fromJust . flip lookup (map swap errors)

errors = [ (FTDI_ERR_ALLOC_READ_BUF, -1)
         , (FTDI_ERR_ALLOC_STRUCT_BUF, -2)
         , (FTDI_ERR_INIT_FAILED, -3)
         ]

-- | (For internal use) Obtain a 'FtdiError' type of a C value from the Error codes list.
ftdiErrorValue :: CInt -> FtdiReturnType Int
ftdiErrorValue c | c >= 0 = (Right . fromIntegral) c         -- Success (on ret == 0)
                 | c <  0 = (Left . toEnum . fromIntegral) c -- C ret code to typed error

-- | (For internal use) Obtain a 'FtdiError' type of a C value from the Error codes list.
ftdiErrorTy :: CInt -> FtdiReturnType ()
ftdiErrorTy c | c >= 0 = return ()                        -- Success (on ret == 0)
              | c <  0 = (Left . toEnum . fromIntegral) c -- C ret code to typed error

-- | Short-hand type for brevity and clarity.
type FtdiReturnType a = Either FtdiError a

-- | DeviceHandle wrapper around C device descriptor pointer
newtype DeviceHandle = DeviceHandle { unDeviceHandle :: Ptr C'ftdi_context }

-- | Essential wrapper
withFtdi :: (DeviceHandle -> IO c) -> IO c
withFtdi  = bracket openFtdi closeFtdi

-- | Handy helper to wrap around Either results
openFtdi :: IO DeviceHandle
openFtdi = do
  r <- ftdiInit
  case r of
    Left e -> throwIO e
    Right dev -> do ftdiSetInterface dev INTERFACE_ANY
                    ftdiUSBOpen dev (0x0403, 0x6010)    -- XXX
                    return dev

-- | ..
ftdiInit :: IO (FtdiReturnType DeviceHandle)
ftdiInit = do
  ptr <- c'ftdi_new
  ret <- c'ftdi_init ptr
  if ret /= 0 then return $ (Left . toEnum . fromIntegral) ret
  else return (Right (DeviceHandle ptr))

-- | ..
ftdiDeInit :: DeviceHandle -> IO ()
ftdiDeInit d = do
  _ <- c'ftdi_deinit (unDeviceHandle d)
  return () -- XXX ignores errors

-- | ..
ftdiSetInterface :: DeviceHandle -> FtdiInterface -> IO ()
ftdiSetInterface d i = do
  _ <- c'ftdi_set_interface (unDeviceHandle d) ((fromIntegral . fromEnum) i)
  return () -- XXX ignores errors

-- | ..
ftdiUSBOpen :: DeviceHandle -> (VendorID, ProductID)-> IO ()
ftdiUSBOpen d (vid, pid) = do
  _ <- c'ftdi_usb_open (unDeviceHandle d) (fromIntegral vid) (fromIntegral pid)
  return () -- XXX ignores errors

-- | Open specified device using a device identifier string, e.g. "i:0x0403:0x6010"
ftdiUSBOpenString :: DeviceHandle -> String -> IO ()
ftdiUSBOpenString d s = withCString s $ \str -> do
  _ <- c'ftdi_usb_open_string (unDeviceHandle d) str
  return () -- XXX ignores errors

-- | ..
ftdiUSBClose :: DeviceHandle -> IO ()
ftdiUSBClose d = do
  _ <- c'ftdi_usb_close (unDeviceHandle d)
  return () -- XXX ignores errors

-- | Close device. Deallocates the memory allocated by openFtdi when called.
closeFtdi :: DeviceHandle -> IO ()
closeFtdi d = do
  ftdiUSBClose d
  ftdiDeInit d

-- | Resets the ftdi device.
-- XXX fix error handling
ftdiUSBReset :: DeviceHandle -> IO ()
ftdiUSBReset d = do
  r <- c'ftdi_usb_reset $ unDeviceHandle d
  case r of
    (0)  -> return ()
    (-1) -> putStrLn "FTDI reset failed"
    (-2) -> putStrLn "USB device unavailable"
