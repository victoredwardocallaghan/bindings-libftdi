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

module LibFtdi.BitBang ( ftdiSetBitMode
                       , ftdiReadPins
                       , ftdiDisableBitBang
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
import LibFtdi.Types

-- | Enable/disable bitbang modes.
ftdiSetBitMode :: DeviceHandle
               -> Int -- ^ Bitmask to configure lines. HIGH/ON value configures a line as output.
               -> Int -- ^ Bitbang mode: use the values defined in ftdi_mpsse_mode
               -> IO ()
ftdiSetBitMode d mask mode = do
  r <- c'ftdi_set_bitmode (unDeviceHandle d) (fromIntegral mask) (fromIntegral mode)
--  -1	can't enable bitbang mode
--  -2	USB device unavailable
  return () -- XXX ignores errors

-- | Disable bitbang mode.
ftdiDisableBitBang :: DeviceHandle -> IO ()
ftdiDisableBitBang d = do
  r <- c'ftdi_disable_bitbang (unDeviceHandle d)
--  -1	can't disable bitbang mode
--  -2	USB device unavailable
  return () -- XXX ignores errors

-- | Directly read pin state, circumventing the read buffer.
-- Useful for bitbang mode.
ftdiReadPins :: DeviceHandle
             -> IO Int
ftdiReadPins d = alloca $ \ptr -> do
  r <- c'ftdi_read_pins (unDeviceHandle d) ptr
--  -1	read pins failed
--  -2	USB device unavailable
  pins <- peek ptr
  return $ fromIntegral pins -- XXX ignores errors
