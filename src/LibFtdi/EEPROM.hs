{-|
  Module      : $Header$
  Copyright   : (c) 2015 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates the EEPROM libftdi functions
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK prune #-}

module LibFtdi.EEPROM ( ftdiReadChipID
                      , ftdiReadEEPROMLocation
                      , ftdiReadEEPROM
                      , ftdiWriteEEPROMLocation
                      , ftdiWriteEEPROM
                      , ftdiEraseEEPROM
                      , ftdiGetErrorString
                      , FtdiEEPROMError(..)
                      ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Control.Exception
import Data.Typeable (Typeable, cast)
import Data.Maybe
import Data.Tuple

import Bindings.LibFtdi

import LibFtdi.LibFtdi

-- | Error codes returned by internal libftdi functions.
data FtdiEEPROMError = FTDI_ERR_EEPROM_READ      -- ^ Read failed
                     | FTDI_ERR_EEPROM_WRITE     -- ^ Write failed
                     | FTDI_ERR_EEPROM_USB       -- ^ USB device unavailable
                     | FTDI_ERR_EEPROM_INIT      -- ^ EEPROM not initialized for the connected device
                     | FTDI_ERR_EEPROM_INVACCESS -- ^ Invalid access to checksum protected area below 0x80
                     | FTDI_ERR_EEPROM_PROTECTED -- ^ Device can't access unprotected area
                     | FTDI_ERR_EEPROM_TYPFAILED -- ^ Reading chip type failed
                      deriving (Eq, Typeable)

instance Show FtdiEEPROMError where
  show FTDI_ERR_EEPROM_READ      = "Read failed"
  show FTDI_ERR_EEPROM_WRITE     = "Write failed"
  show FTDI_ERR_EEPROM_USB       = "USB device unavailable"
  show FTDI_ERR_EEPROM_INIT      = "EEPROM not initialized for the connected device"
  show FTDI_ERR_EEPROM_INVACCESS = "Invalid access to checksum protected area below 0x80"
  show FTDI_ERR_EEPROM_PROTECTED = "Device can't access unprotected area"
  show FTDI_ERR_EEPROM_TYPFAILED = "Reading chip type failed"

instance Exception FtdiEEPROMError

-- | Read EEPROM Location
ftdiReadEEPROMLocation :: DeviceHandle
                       -> Int -- ^ Address of eeprom location to be read
                       -> IO (Either FtdiEEPROMError Int) -- ^ Value to be read
ftdiReadEEPROMLocation d addr = alloca $ \ptr -> do
  r <- c'ftdi_read_eeprom_location (unDeviceHandle d) (fromIntegral addr) ptr
  case r of
    (0)  -> do val <- peek ptr
               return $ Right (fromIntegral val)
    (-1) -> return $ Left FTDI_ERR_EEPROM_READ
    (-2) -> return $ Left FTDI_ERR_EEPROM_USB

-- | Read EEPROM
ftdiReadEEPROM :: DeviceHandle -> IO (Either FtdiEEPROMError ())
ftdiReadEEPROM d = do
  r <- c'ftdi_read_eeprom $ unDeviceHandle d
  case r of
    (0)  -> return $ Right ()
    (-1) -> return $ Left FTDI_ERR_EEPROM_READ
    (-2) -> return $ Left FTDI_ERR_EEPROM_USB

-- | Read the FTDIChip-ID from R-type devices
ftdiReadChipID :: DeviceHandle -> IO (Either FtdiEEPROMError Int)
ftdiReadChipID d = alloca $ \ptr -> do
  r <- c'ftdi_read_chipid (unDeviceHandle d) ptr
  case r of
    (0)  -> do cid <- peek ptr
               return $ Right (fromIntegral cid)
    (-1) -> return $ Left FTDI_ERR_EEPROM_READ
    (-2) -> return $ Left FTDI_ERR_EEPROM_USB

-- | Write EEPROM Location
ftdiWriteEEPROMLocation :: DeviceHandle
                        -> Int -- ^ Address of eeprom location to be written 
			-> Int -- ^ Value to be written
                        -> IO (Either FtdiEEPROMError ())
ftdiWriteEEPROMLocation d addr val = do
  r <- c'ftdi_write_eeprom_location (unDeviceHandle d) (fromIntegral addr) (fromIntegral val)
  case r of
    (0)  -> return $ Right ()
    (-1) -> return $ Left FTDI_ERR_EEPROM_WRITE
    (-2) -> return $ Left FTDI_ERR_EEPROM_USB
    (-3) -> return $ Left FTDI_ERR_EEPROM_INVACCESS
    (-4) -> return $ Left FTDI_ERR_EEPROM_PROTECTED
    (-5) -> return $ Left FTDI_ERR_EEPROM_TYPFAILED

-- | Write EEPROM
ftdiWriteEEPROM :: DeviceHandle -> IO (Either FtdiEEPROMError ())
ftdiWriteEEPROM d = do
  r <- c'ftdi_write_eeprom $ unDeviceHandle d
  case r of
    (0)  -> return $ Right ()
    (-1) -> return $ Left FTDI_ERR_EEPROM_READ
    (-2) -> return $ Left FTDI_ERR_EEPROM_USB
    (-3) -> return $ Left FTDI_ERR_EEPROM_INIT

-- | Erase EEPROM
ftdiEraseEEPROM :: DeviceHandle -> IO (Either FtdiEEPROMError ())
ftdiEraseEEPROM d = do
  r <- c'ftdi_erase_eeprom $ unDeviceHandle d
  return $ Right () -- XXX ignore errors
    
-- | Get string representation for last error code
ftdiGetErrorString :: DeviceHandle -> IO String
ftdiGetErrorString d = do
  pstr <- c'ftdi_get_error_string $ unDeviceHandle d
  (peekCString pstr) >>= return
