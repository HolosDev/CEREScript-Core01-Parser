module Test.Data.CERES.VariablePosition.Parser where


import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit.Base

import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T


import           Data.CERES.Type
import           Data.CERES.Script
import           Data.CERES.Value
import           Data.CERES.VariablePosition
import           Data.CERES.VariablePosition.Parser


tests = $(testGroupGenerator)

case_nothing = assertEqual "Nothing" 0 0
case_readVII_01 = assertEqual "Read VariableIndex VII=1"
                              (Right ((VII 1), ""))
                              (readVariableIndex "VII=1")
case_readVIN_01 = assertEqual "Read VariableIndex VII=1"
                              (Right ((VIN "=ABC"), ""))
                              (readVariableIndex "VIN=\"=ABC\"")
case_readVIIT_01 = assertEqual "Read VariableIndex VIIT=-1:2"
                               (Right ((VIIT (-1) 2), ""))
                               (readVariableIndex "VIIT=-1:2")
case_readVINT_01 = assertEqual "Read VariableIndex VINT=\"CRH\":-4"
                               (Right ((VINT "CRH" (-5)), ""))
                               (readVariableIndex "VINT=\"CRH\":-5")

-- TODO: Regression test for unsorted RI
case_readVIIRI_01 = assertEqual "Read VariableIndex VIIRI=100:[-1,4,-3]"
                                (Right ((VIIRI 100 [-3, -1, 4]), ""))
                                (readVariableIndex "VIIRI=100:[-1,4,-3]")
case_readVINRI_01 = assertEqual "Read VariableIndex VINRI=\"BCH\":[-5,5,1]"
                                (Right ((VINRI "BCH" [-5, 1, 5]), ""))
                                (readVariableIndex "VINRI=\"BCH\":[-5,5,1]")
case_readVIIRIT_01 = assertEqual "Read VariableIndex VIIRIT=-3:[-7,8,0]:4"
                                 (Right ((VIIRIT (-3) [-7, 0, 8] 4), ""))
                                 (readVariableIndex "VIIRIT=-3:[-7,8,0]:4")
case_readVINRIT_01 = assertEqual
  "Read VariableIndex VINRIT=\"ZHO\":[-2,-5,-3]:999"
  (Right ((VINRIT "ZHO" [-5, -3, -2] 999), ""))
  (readVariableIndex "VINRIT=\"ZHO\":[-2,-5,-3]:999")


case_readVIV_01 = assertEqual "Read VariableIndex VI VIV=SV<| SRCH |>"
                              (Right ((VIV (StrValue "SRCH")), ""))
                              (readVariableIndex "VIV=SV<| SRCH |>")
case_readVIAtom_01_01 = assertEqual "Read VariableIndex VIAtom"
                                    (Right (VIAtom, ""))
                                    (readVariableIndex "VIAtom")
case_readVINull_01_01 = assertEqual "Read VariableIndex VINull"
                                    (Right (VINull, ""))
                                    (readVariableIndex "VINull")
case_readPVII_01 = assertEqual "Read VariableIndex PVII=2"
                               (Right ((PVII 2), ""))
                               (readVariableIndex "PVII=2")
case_readPVIN_01 = assertEqual "Read VariableIndex PVIN=\"BVD\""
                               (Right ((PVIN "BVD"), ""))
                               (readVariableIndex "PVIN=\"BVD\"")
case_readPVIT_01 = assertEqual "Read VariableIndex PVIT=-4"
                               (Right ((PVIT (-4)), ""))
                               (readVariableIndex "PVIT=-4")
case_readPVIIRI_01 = assertEqual "Read VariableIndex PVIIRI=3:[-2,6,3]"
                                 (Right ((PVIIRI 3 [-2, 3, 6]), ""))
                                 (readVariableIndex "PVIIRI=3:[-2,6,3]")
case_readPVINRI_01 = assertEqual "Read VariableIndex PVINRI=\"V<\":[9,2,5]"
                                 (Right ((PVINRI "V<" [2, 5, 9]), ""))
                                 (readVariableIndex "PVINRI=\"V<\":[9,2,5]")
case_readPVIIRIT_01 = assertEqual "Read VariableIndex PVIIRIT=0:[6,2,1]:-0"
                                  (Right ((PVIIRIT 0 [1, 2, 6] (-0)), ""))
                                  (readVariableIndex "PVIIRIT=0:[6,2,1]:-0")
case_readPVINRIT_01 = assertEqual "Read VariableIndex PVINRIT=\"QDO\":[]:4"
                                  (Right ((PVINRIT "QDO" [] 4), ""))
                                  (readVariableIndex "PVINRIT=\"QDO\":[]:4")
