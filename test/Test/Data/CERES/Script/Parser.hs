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


case_readInitVariable01 = assertEqual
      "Reading valid InitVariable"
      (Right
            ( CRSInitVariable (VP AtHere (VII 1))
                              (VP AtTricky (VIN "WorldTime"))
            , ""
            )
      )
      (parseCERES "InitVariable AtHere[VII=1] AtTricky[VIN=\"WorldTime\"]")
case_readConvertValue01 = assertEqual
      "Reading valid ConvertValue"
      (Right (CRSConvertValue (VP AtWorld (VIIT 2 3)) VTAtom, ""))
      (parseCERES "ConvertValue AtWorld[VIIT=2:3] CAtom")
case_readCEREScript01 = assertEqual
      "Reading valid CEREScript"
      (Right
            ( [ CRSInitVariable (VP AtHere (VII 1))
                                (VP AtTricky (VIN "WorldTime"))
              , CRSConvertValue (VP AtWorld (VIIT 2 3)) VTAtom
              ]
            , ""
            )
      )
      (parseCEREScript
            "InitVariable AtHere[VII=1] AtTricky[VIN=\"WorldTime\"]\nConvertValue AtWorld[VIIT=2:3] CAtom\n"
      )
