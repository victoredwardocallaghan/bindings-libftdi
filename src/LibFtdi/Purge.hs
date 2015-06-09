{-|
  Module      : $Header$
  Copyright   : (c) 2015 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates the ... libftdi functions
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK prune #-}

module LibFtdi.Purge ( usbPurgeRXBuffer
                     , usbPurgeTXBuffer
                     , usbPurgeBuffers
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
data FtdiPurgeError = FTDI_ERR_PURGE_BUF_READ  -- ^ read buffer purge failed
                    | FTDI_ERR_PURGE_BUF_WRITE -- ^ write buffer purge failed
                    | FTDI_ERR_PURGE_BUF_USB   -- ^ USB device unavailable
                     deriving (Eq, Typeable)

instance Show FtdiPurgeError where
  show FTDI_ERR_PURGE_BUF_READ   = "read buffer purge failed"
  show FTDI_ERR_PURGE_BUF_WRITE  = "write buffer purge failed"
  show FTDI_ERR_PURGE_BUF_USB    = "USB device unavailable"

instance Exception FtdiPurgeError

-- | Clears the read buffer on the chip and the internal read buffer.
usbPurgeRXBuffer :: DeviceHandle -> IO (Either FtdiPurgeError ())
usbPurgeRXBuffer d = do
  r <- c'ftdi_usb_purge_rx_buffer $ unDeviceHandle d
  case r of
    (0)  -> return $ Right ()
    (-1) -> return $ Left FTDI_ERR_PURGE_BUF_READ
    (-2) -> return $ Left FTDI_ERR_PURGE_BUF_USB

-- | Clears the write buffer on the chip.
usbPurgeTXBuffer :: DeviceHandle -> IO (Either FtdiPurgeError ())
usbPurgeTXBuffer d = do
  r <- c'ftdi_usb_purge_tx_buffer $ unDeviceHandle d
  case r of
    (0)  -> return $ Right ()
    (-1) -> return $ Left FTDI_ERR_PURGE_BUF_WRITE
    (-2) -> return $ Left FTDI_ERR_PURGE_BUF_USB

-- | Clears the buffers on the chip and the internal read buffer.
usbPurgeBuffers :: DeviceHandle -> IO (Either FtdiPurgeError ())
usbPurgeBuffers d = do
  r <- c'ftdi_usb_purge_buffers $ unDeviceHandle d
  case r of
    (0)  -> return $ Right ()
    (-1) -> return $ Left FTDI_ERR_PURGE_BUF_READ
    (-2) -> return $ Left FTDI_ERR_PURGE_BUF_WRITE
    (-3) -> return $ Left FTDI_ERR_PURGE_BUF_USB
