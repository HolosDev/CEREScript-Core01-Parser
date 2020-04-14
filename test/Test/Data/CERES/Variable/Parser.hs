module Test.Data.CERES.Variable.Parser where


import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit.Base


import qualified Data.IntMap                   as IM
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T


import           Data.CERES.Type
import           Data.CERES.Data
import           Data.CERES.Variable.Parser


tests = $(testGroupGenerator)

case_nothing = assertEqual "Nothing" 0 0
case_readVII_01 = assertEqual "Read VariableIndex VII=1"
                              (Right (VII 1, ""))
                              (readVariableIndex "VII=1")
case_readVIN_01 = assertEqual "Read VariableIndex VII=1"
                              (Right (VIN "=ABC", ""))
                              (readVariableIndex "VIN=\"=ABC\"")
case_readVIIT_01 = assertEqual "Read VariableIndex VIIT=-1:2"
                               (Right (VIIT (-1) 2, ""))
                               (readVariableIndex "VIIT=-1:2")
case_readVINT_01 = assertEqual "Read VariableIndex VINT=\"CRH\":-4"
                               (Right (VINT "CRH" (-5), ""))
                               (readVariableIndex "VINT=\"CRH\":-5")

-- TODO: Regression test for unsorted RI
case_readVIIRI_01 = assertEqual "Read VariableIndex VIIRI=100:[-1,4,-3]"
                                (Right (VIIRI 100 [-3, -1, 4], ""))
                                (readVariableIndex "VIIRI=100:[-1,4,-3]")
case_readVINRI_01 = assertEqual "Read VariableIndex VINRI=\"BCH\":[-5,5,1]"
                                (Right (VINRI "BCH" [-5, 1, 5], ""))
                                (readVariableIndex "VINRI=\"BCH\":[-5,5,1]")
case_readVIIRIT_01 = assertEqual "Read VariableIndex VIIRIT=-3:[-7,8,0]:4"
                                 (Right (VIIRIT (-3) [-7, 0, 8] 4, ""))
                                 (readVariableIndex "VIIRIT=-3:[-7,8,0]:4")
case_readVINRIT_01 = assertEqual
  "Read VariableIndex VINRIT=\"ZHO\":[-2,-5,-3]:999"
  (Right (VINRIT "ZHO" [-5, -3, -2] 999, ""))
  (readVariableIndex "VINRIT=\"ZHO\":[-2,-5,-3]:999")


case_readVIV_01 = assertEqual "Read VariableIndex VI VIV=SV<| SRCH |>"
                              (Right (VIV (StrValue "SRCH"), ""))
                              (readVariableIndex "VIV=SV<| SRCH |>")
case_readVIAtom_01_01 = assertEqual "Read VariableIndex VIAtom"
                                    (Right (VIAtom, ""))
                                    (readVariableIndex "VIAtom")
case_readVINull_01_01 = assertEqual "Read VariableIndex VINull"
                                    (Right (VINull, ""))
                                    (readVariableIndex "VINull")
case_readPVII_01 = assertEqual "Read VariableIndex PVII=2"
                               (Right (PVII 2, ""))
                               (readVariableIndex "PVII=2")
case_readPVIN_01 = assertEqual "Read VariableIndex PVIN=\"BVD\""
                               (Right (PVIN "BVD", ""))
                               (readVariableIndex "PVIN=\"BVD\"")
case_readPVIT_01 = assertEqual "Read VariableIndex PVIT=-4"
                               (Right (PVIT (-4), ""))
                               (readVariableIndex "PVIT=-4")
case_readPVIIRI_01 = assertEqual "Read VariableIndex PVIIRI=3:[-2,6,3]"
                                 (Right (PVIIRI 3 [-2, 3, 6], ""))
                                 (readVariableIndex "PVIIRI=3:[-2,6,3]")
case_readPVINRI_01 = assertEqual "Read VariableIndex PVINRI=\"V<\":[9,2,5]"
                                 (Right (PVINRI "V<" [2, 5, 9], ""))
                                 (readVariableIndex "PVINRI=\"V<\":[9,2,5]")
case_readPVIIRIT_01 = assertEqual "Read VariableIndex PVIIRIT=0:[6,2,1]:-0"
                                  (Right (PVIIRIT 0 [1, 2, 6] (-0), ""))
                                  (readVariableIndex "PVIIRIT=0:[6,2,1]:-0")
case_readPVINRIT_01 = assertEqual "Read VariableIndex PVINRIT=\"QDO\":[]:4"
                                  (Right (PVINRIT "QDO" [] 4, ""))
                                  (readVariableIndex "PVINRIT=\"QDO\":[]:4")



case_readIntValue01 = assertEqual "Reading valid & positive IntValue"
                                  (Right (IntValue 123456789, ""))
                                  (parseValue "IV<| 123456789 |>")
case_readIntValue02 = assertEqual "Reading valid & negative IntValue"
                                  (Right (IntValue (-456789), ""))
                                  (parseValue "IV<| -456789 |>")
case_readIntValue03 = assertEqual
  "Bypass unreadable text after reading IntValue"
  (Right (IntValue (-456789), " |>)))"))
  (parseValue "IV<| -456789 |> |>)))")

case_readDblValue01 = assertEqual "Reading valid & positive DblValue"
                                  (Right (DblValue 123456789, ""))
                                  (parseValue "DV<| 123456789 |>")
case_readDblValue01_1 = assertEqual "Reading valid & positive DblValue"
                                    (Right (DblValue 1234.6789, ""))
                                    (parseValue "DV<| 1234.6789 |>")
case_readDblValue02 = assertEqual "Reading valid & negative DblValue"
                                  (Right (DblValue (-456.89), ""))
                                  (parseValue "DV<| -456.89 |>")
case_readDblValue02_2 = assertEqual "Reading valid & negative DblValue"
                                    (Right (DblValue (-456.89), ""))
                                    (parseValue "DV<| -456.89 |>")
case_readDblValue03 = assertEqual
  "Bypass unreadable text after reading DblValue"
  (Right (DblValue (-456789), " |>)))"))
  (parseValue "DV<| -456789 |> |>)))")

case_readBoolValue01 = assertEqual "Reading valid True  BoolValue"
                                   (Right (BoolValue True, ""))
                                   (parseValue "BV<| True |>")
case_readBoolValue02 = assertEqual "Reading valid False BoolValue"
                                   (Right (BoolValue False, ""))
                                   (parseValue "BV<| False |>")
case_readBoolValue03 = assertEqual "Reading valid true  BoolValue"
                                   (Right (BoolValue True, ""))
                                   (parseValue "BV<| true |>")
case_readBoolValue04 = assertEqual "Reading valid false BoolValue"
                                   (Right (BoolValue False, ""))
                                   (parseValue "BV<| false |>")

case_readAtomValue01 = assertEqual "Reading valid AtomValue"
                                   (Right (AtomValue, ""))
                                   (parseValue "AV<| - |>")

case_readArrValue01 = assertEqual
  "Reading valid ArrValue"
  (Right (ArrValue (IM.fromList [(0, IntValue 1), (3, BoolValue False)]), "o"))
  (parseValue "A[|| 3:BV<| False |> || 0:IV<| 1 |> ||]o")
