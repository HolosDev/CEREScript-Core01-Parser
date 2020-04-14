module Test.Data.CERES.Value.Parser where


import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit.Base

import qualified Data.IntMap                   as IM
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T


import           Data.CERES.Type
import           Data.CERES.Data
import           Data.CERES.Value.Parser


tests = $(testGroupGenerator)


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
