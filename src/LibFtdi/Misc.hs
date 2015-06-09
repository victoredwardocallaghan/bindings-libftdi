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

module LibFtdi.Misc ( ftdiSetBitMode
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

import LibFtdi.Types

import Bindings.LibFtdi

-- | ..
ftdiSetBitMode :: DeviceHandle
               -> Int -- ^ bit mask
               -> Int -- ^ mode
               -> IO ()
ftdiSetBitMode d mask mode = do
  _ <- c'ftdi_set_bitmode (unDeviceHandle d) (fromIntegral mask) (fromIntegral mode)
  return () -- XXX ignores errors

ftdiReadPins
ftdiDisableBitBang
-- | ..
ftdiDisableBitBang :: DeviceHandle -> IO ()
ftdiDisableBitBang d = do
  _ <- c'ftdi_disable_bitbang (unDeviceHandle d)
  return () -- XXX ignores errors

-- | ..
ftdiReadPins :: DeviceHandle
             -> IO Int
ftdiReadPins d = alloc $ \ptr -> do
  _ <- c'ftdi_read_pins (unDeviceHandle d) ptr
  pins <- peek ptr
  return pins -- XXX ignores errors
