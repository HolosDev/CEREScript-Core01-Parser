module Test.Data.CERES.Script.Parser where


import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit.Base

import qualified Data.IntMap                   as IM
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T


import           Data.CERES.Type
import           Data.CERES.Script
import           Data.CERES.Script.Parser
import           Data.CERES.Value
import           Data.CERES.VariablePosition


tests = $(testGroupGenerator)


case_readInitVariable01 = assertEqual "Reading valid InitVariable"
      (Right ((CRSInitVariable (VP AtHere (VII 1)) (VP AtTricky (VIN "WorldTime"))),""))
      (parseCERES "InitVariable AtHere[VII=1] AtTricky[VIN=\"WorldTime\"]")
