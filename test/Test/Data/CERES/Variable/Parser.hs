module Test.Data.CERES.Variable.Parser where


import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit.Base


import qualified Data.IntMap.Strict            as IM
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T


import           Data.CERES.Type
import           Data.CERES.Data
import           Data.CERES.Variable.Parser

import           Test.Data.CERES.Parser


tests = $(testGroupGenerator)


vVII_01 = VII 1
tVII_01 = "VII=1"
case_readVII_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tVII_01)
  (Right (vVII_01, ""))
  readVariableIndex
  tVII_01
vVIN_01 = VIN "=ABC"
tVIN_01 = "VIN=\"=ABC\""
case_readVIN_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tVIN_01)
  (Right (vVIN_01, ""))
  readVariableIndex
  tVIN_01
vVIIT_01 = VIIT (-1) 2
tVIIT_01 = "VIIT=-1:2"
case_readVIIT_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tVIIT_01)
  (Right (vVIIT_01, ""))
  readVariableIndex
  tVIIT_01
vVINT_01 = VINT "CRH" (-4)
tVINT_01 = "VINT=\"CRH\":-4"
case_readVINT_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tVINT_01)
  (Right (vVINT_01, ""))
  readVariableIndex
  tVINT_01

-- TODO: Regression test for unsorted RI
vVIIRI_01 = VIIRI 100 [-3, -1, 4]
tVIIRI_01 = "VIIRI=100:[-1,4,-3]"
case_readVIIRI_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tVIIRI_01)
  (Right (vVIIRI_01, ""))
  readVariableIndex
  tVIIRI_01
vVINRI_01 = VINRI "BCH" [-5, 1, 5]
tVINRI_01 = "VINRI=\"BCH\":[-5,5,1]"
case_readVINRI_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tVINRI_01)
  (Right (vVINRI_01, ""))
  readVariableIndex
  tVINRI_01
vVIIRIT_01 = VIIRIT (-3) [-7, 0, 8] 4
tVIIRIT_01 = "VIIRIT=-3:[-7,8,0]:4"
case_readVIIRIT_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tVIIRIT_01)
  (Right (vVIIRIT_01, ""))
  readVariableIndex
  tVIIRIT_01
vVINRIT_01 = VINRIT "ZHO" [-5, -3, -2] 999
tVINRIT_01 = "VINRIT=\"ZHO\":[-2,-5,-3]:999"
case_readVINRIT_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tVINRIT_01)
  (Right (vVINRIT_01, ""))
  readVariableIndex
  tVINRIT_01


vVIV_01 = VIV (StrValue "SRCH")
tVIV_01 = "VIV=SV<| SRCH |>"
case_readVIV_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tVIV_01)
  (Right (vVIV_01, ""))
  readVariableIndex
  tVIV_01

vVIAtom_01 = VIAtom
tVIAtom_01 = "VIAtom"
case_readVIAtom_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tVIAtom_01)
  (Right (vVIAtom_01, ""))
  readVariableIndex
  tVIAtom_01

vVINull_01 = VINull
tVINull_01 = "VINull"
case_readVINull_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tVINull_01)
  (Right (vVINull_01, ""))
  readVariableIndex
  tVINull_01

vPVII_01 = PVII 2
tPVII_01 = "PVII=2"
case_readPVII_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tPVII_01)
  (Right (vPVII_01, ""))
  readVariableIndex
  tPVII_01

vPVIN_01 = PVIN "BVD"
tPVIN_01 = "PVIN=\"BVD\""
case_readPVIN_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tPVIN_01)
  (Right (vPVIN_01, ""))
  readVariableIndex
  tPVIN_01

vPVIT_01 = PVIT (-4)
tPVIT_01 = "PVIT=-4"
case_readPVIT_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tPVIT_01)
  (Right (vPVIT_01, ""))
  readVariableIndex
  tPVIT_01

vPVIIRI_01 = PVIIRI 3 [-2, 3, 6]
tPVIIRI_01 = "PVIIRI=3:[-2,6,3]"
case_readPVIIRI_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tPVIIRI_01)
  (Right (vPVIIRI_01, ""))
  readVariableIndex
  tPVIIRI_01

vPVINRI_01 = PVINRI "V<" [2, 5, 9]
tPVINRI_01 = "PVINRI=\"V<\":[9,2,5]"
case_readPVINRI_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tPVIIRI_01)
  (Right (vPVIIRI_01, ""))
  readVariableIndex
  tPVIIRI_01

vPVIIRIT_01 = PVIIRIT 0 [1, 2, 6] (-0)
tPVIIRIT_01 = "PVIIRIT=0:[6,2,1]:-0"
case_readPVIIRIT_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tPVIIRIT_01)
  (Right (vPVIIRIT_01, ""))
  readVariableIndex
  tPVIIRIT_01

vPVINRIT_01 = PVINRIT "QDO" [] 4
tPVINRIT_01 = "PVINRIT=\"QDO\":[]:4"
case_readPVINRIT_01 = eqParsingTester
  (T.unpack $ "Read VariableIndex " <> tPVINRIT_01)
  (Right (vPVINRIT_01, ""))
  readVariableIndex
  tPVINRIT_01


vIntValue01 = IntValue 123456789
tIntValue01 = "IV<| 123456789 |>"
case_readIntValue01 = eqParsingTester
  (T.unpack $ "Reading valid & positive IntValue " <> tIntValue01)
  (Right (vIntValue01, ""))
  parseValue
  tIntValue01

vIntValue02 = IntValue (-456789)
tIntValue02 = "IV<| -456789 |>"
case_readIntValue02 = eqParsingTester
  (T.unpack $ "Reading valid & negative IntValue " <> tIntValue02)
  (Right (vIntValue02, ""))
  parseValue
  tIntValue02

vIntValue03 = IntValue (-456789)
tIntValue03 = "IV<| -456789 |>"
case_readIntValue03 = eqParsingTester
  ( T.unpack
  $ "Bypass unreadable text after reading IntValue " <> tIntValue03
  )
  (Right (vIntValue03, " <> |>)))"))
  parseValue
  tIntValue03


vDblValue01_1 = DblValue 123456789
tDblValue01_1 = "DV<| 123456789 |>"
case_readDblValue01_1 = eqParsingTester
  (T.unpack $ "Reading valid & positive DblValue " <> tDblValue01_1)
  (Right (vDblValue01_1, ""))
  parseValue
  tDblValue01_1

vDblValue01_2 = DblValue 1234.6789
tDblValue01_2 = "DV<| 1234.6789 |>"
case_readDblValue01_2 = eqParsingTester
  (T.unpack $ "Reading valid & positive DblValue " <> tDblValue01_2)
  (Right (vDblValue01_2, ""))
  parseValue
  tDblValue01_2

vDblValue02_1 = DblValue (-456.89)
tDblValue02_1 = "DV<| -456.89 |>"
case_readDblValue02_1 = eqParsingTester
  (T.unpack $ "Reading valid & negative DblValue " <> tDblValue02_1)
  (Right (vDblValue02_1, ""))
  parseValue
  tDblValue02_1

vDblValue02_2 = DblValue (-456.89)
tDblValue02_2 = "DV<| -456.89 |>"
case_readDblValue02_2 = eqParsingTester
  (T.unpack $ "Reading valid & negative DblValue " <> tDblValue02_2)
  (Right (vDblValue02_2, ""))
  parseValue
  tDblValue02_2

vDblValue03_1 = DblValue (-456789)
tDblValue03_1 = "DV<| -456789 |>"
case_readDblValue03_1 = eqParsingTester
  ( T.unpack
  $ "Bypass unreadable text after reading DblValue " <> tDblValue03_1
  )
  (Right (vDblValue03_1, " |>)))"))
  parseValue
  tDblValue03_1


vBoolValue01_1 = BoolValue True
tBoolValue01_1 = "BV<| True |>"
case_readBoolValue01_1 = eqParsingTester
  (T.unpack $ "Reading valid True  BoolValue " <> tBoolValue01_1)
  (Right (vBoolValue01_1, ""))
  parseValue
  tBoolValue01_1


vBoolValue01_2 = BoolValue True
tBoolValue01_2 = "BV<| true |>"
case_readBoolValue01_2 = eqParsingTester
  (T.unpack $ "Reading valid true  BoolValue " <> tBoolValue01_2)
  (Right (vBoolValue01_2, ""))
  parseValue
  tBoolValue01_2

vBoolValue02_1 = BoolValue False
tBoolValue02_1 = "BV<| False |>"
case_readBoolValue02_1 = eqParsingTester
  (T.unpack $ "Reading valid False BoolValue " <> tBoolValue02_1)
  (Right (vBoolValue02_1, ""))
  parseValue
  tBoolValue02_1

vBoolValue02_2 = BoolValue False
tBoolValue02_2 = "BV<| false |>"
case_readBoolValue02_2 = eqParsingTester
  (T.unpack $ "Reading valid false BoolValue " <> tBoolValue02_2)
  (Right (vBoolValue02_2, ""))
  parseValue
  tBoolValue02_2



vAtomValue01 = AtomValue
tAtomValue01 = "AV<| - |>"
case_readAtomValue01 = eqParsingTester
  (T.unpack $ "Reading valid AtomValue " <> tAtomValue01)
  (Right (vAtomValue01, ""))
  parseValue
  tAtomValue01

vArrValue01 = ArrValue (IM.fromList [(0, IntValue 1), (3, BoolValue False)])
tArrValue01 = "A[|| 3:BV<| False |> || 0:IV<| 1 |> ||]"
case_readArrValue01 = eqParsingTester
  (T.unpack $ "Reading valid ArrValue " <> tArrValue01)
  (Right (vArrValue01, "o"))
  parseValue
  tArrValue01
