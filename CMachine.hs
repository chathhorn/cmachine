module CMachine
      ( initCMaState
      , CMachine (..)
      , CMaState
      , showRegs, dumpStack, dumpData, dumpDataSwatch, dumpCode
      , clear, clearData, clearCode, resizeData
      , execute
      ) where

import CMachine.Core
import CMachine.Basic

