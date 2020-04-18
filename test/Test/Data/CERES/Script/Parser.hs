module Test.Data.CERES.Script.Parser where


import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit.Base


import           Data.CERES.Type
import           Data.CERES.Data
import           Data.CERES.Data.Method
import           Data.CERES.Script.Parser

import           Test.Data.CERES.Parser


tests = $(testGroupGenerator)


vInitVariable01 =
  CRSInitVariable (VP AtHere (VII 1)) (VP AtTricky (VIN "WorldTime"))
tInitVariable01 = "InitVariable AtHere[VII=1] AtTricky[VIN=\"WorldTime\"]"
case_readInitVariable01 = eqParsingTester
  "Reading valid InitVariable        "
  (Right (vInitVariable01, ""))
  parseCERES
  tInitVariable01
case_readInitVariable01' = eqInverseParsingTester
  "Reading valid InitVariable"
  (Right (vInitVariable01, ""))
  vInitVariable01
  parseCERES
  showtl

vConvertValue01 = CRSConvertValue (VP AtWorld (VIIT 2 3)) VTAtom
tConvertValue01 = "ConvertValue AtWorld[VIIT=2:3] CAtom"
case_readConvertValue01 = eqParsingTester
  "Reading valid ConvertValue        "
  (Right (vConvertValue01, ""))
  parseCERES
  tConvertValue01
case_readConvertValue01' = eqInverseParsingTester
  "Reading valid ConvertValue"
  (Right (vConvertValue01, ""))
  vConvertValue01
  parseCERES
  showtl

vCEREScript01 =
  [ CRSInitVariable (VP AtHere (VII 1)) (VP AtTricky (VIN "WorldTime"))
  , CRSConvertValue (VP AtWorld (VIIT 2 3)) VTAtom
  ]
tCEREScript01 =
  "InitVariable AtHere[VII=1] AtTricky[VIN=\"WorldTime\"]\nConvertValue AtWorld[VIIT=2:3] CAtom\n"
case_readCEREScript01 = eqParsingTester
  "Reading valid CEREScript            "
  (Right (vCEREScript01, ""))
  parseCEREScript
  tCEREScript01
case_readCEREScript01' = eqInverseParsingTester
  "Reading valid CEREScript        "
  (Right (vCEREScript01, ""))
  vCEREScript01
  parseCEREScript
  showtlCEREScript

vCEREScript02 =
  [ CRSExtend3 "COX"
               (VP AtHere (VII 2))
               (VP AtDict (VII 3))
               (VP AtLTemp (VIN "ABC"))
  ]
tCEREScript02 =
  "Extend3 COX AtHere[VII=2] AtDict[VII=3] AtLTemp[VIN=\"ABC\"]\n"
case_readCEREScript02 = eqParsingTester
  "Reading valid CEREScript            "
  (Right (vCEREScript02, ""))
  parseCEREScript
  tCEREScript02
case_readCEREScript02' = eqInverseParsingTester
  "Reading valid CEREScript    "
  (Right (vCEREScript02, ""))
  vCEREScript02
  parseCEREScript
  showtlCEREScript
