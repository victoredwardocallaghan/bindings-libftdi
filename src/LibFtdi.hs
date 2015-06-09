{-|
  Module      : $Header$
  Copyright   : (c) 2015 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module collects together libftdi high-level
  actions and primitives into a common namespace.
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module LibFtdi (module X) where

import LibFtdi.LibFtdi as X
import LibFtdi.Types as X
import LibFtdi.Misc as X
import LibFtdi.Purge as X
import LibFtdi.EEPROM as X
