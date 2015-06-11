{-|
  Module      : $Header$
  Copyright   : (c) 2015 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates the ... libftdi functions
-}

{-# OPTIONS_HADDOCK prune #-}

module LibFtdi.Data ( ftdiReadData
                    , ftdiWriteData
                    ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Control.Exception
import Data.Typeable (Typeable, cast)
import Data.Maybe
import Data.Tuple

import qualified Data.ByteString as BS

import Bindings.LibFtdi

import LibFtdi.LibFtdi
import LibFtdi.Types

-- | Reads data in chunks (see ftdi_read_data_set_chunksize()) from the chip.
-- Automatically strips the two modem status bytes transfered during every read.
ftdiReadData :: DeviceHandle
             -> Int -- ^ Size of the buffer
             -> IO (Maybe BS.ByteString)
ftdiReadData d sz = allocaBytes sz $ \ptr -> do
  r <- c'ftdi_read_data (unDeviceHandle d) ptr (fromIntegral sz)
  if r < 0 then undefined -- XXX handle error
  else if r == 0 then return Nothing
  else do bs <- BS.packCStringLen (castPtr ptr, fromIntegral r)
          (return . Just) bs

-- | ..
ftdiWriteData :: DeviceHandle
              -> BS.ByteString
              -> IO Int
ftdiWriteData d s = BS.useAsCStringLen s $ \(p, len) -> do 
  r <- c'ftdi_write_data (unDeviceHandle d) (castPtr p) (fromIntegral len)
-- -666	USB device unavailable
-- <0	error code from usb_bulk_write()
  if r < 0 then undefined -- XXX handle error
  else return $ fromIntegral r -- >0	number of bytes written 

-- int ftdi_read_data(struct ftdi_context *ftdi, unsigned char *buf, int size);
-- int ftdi_read_data_set_chunksize(struct ftdi_context *ftdi, unsigned int chunksize);
-- int ftdi_read_data_get_chunksize(struct ftdi_context *ftdi, unsigned int *chunksize);

-- int ftdi_write_data(struct ftdi_context *ftdi, const unsigned char *buf, int size);
-- int ftdi_write_data_set_chunksize(struct ftdi_context *ftdi, unsigned int chunksize);
-- int ftdi_write_data_get_chunksize(struct ftdi_context *ftdi, unsigned int *chunksize);
