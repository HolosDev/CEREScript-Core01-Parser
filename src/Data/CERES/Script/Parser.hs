module Data.CERES.Script.Parser where


import           Data.Maybe
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Read           as TR

import           TextShow

import           Data.CERES.Parser
import           Data.CERES.Type
import           Data.CERES.Script
import           Data.CERES.Value
import           Data.CERES.Value.Parser
import           Data.CERES.VariablePosition
import           Data.CERES.VariablePosition.Parser


parseCERES :: Text -> Result CERES
parseCERES aText = case pHeader of
  "InitVariable" ->
    ceresHeaderWrapper CRSInitVariable
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "InitVariableAt" ->
    ceresHeaderWrapper CRSInitVariableAt
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "SetValue" ->
    ceresHeaderWrapper CRSSetValue
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "DeleteVariable" ->
    ceresHeaderWrapper CRSDeleteVariable >>= addVariablePosition
  "ModifyValue" ->
    ceresHeaderWrapper CRSModifyValue
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addCERESOperator
  "CopyValue" ->
    ceresHeaderWrapper CRSCopyValue
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "ConvertValue" ->
    ceresHeaderWrapper CRSConvertValue
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addValueType
  "ConvertValueBy" ->
    ceresHeaderWrapper CRSConvertValueBy
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "ConvertValueWith" ->
    ceresHeaderWrapper CRSConvertValueWith
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "ReplaceText" -> ceresHeaderWrapper CRSReplaceText >>= addVariablePosition
  "ReplaceTextTo" ->
    ceresHeaderWrapper CRSReplaceTextTo
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "Random" ->
    ceresHeaderWrapper CRSRandom
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addValueType
  "RandomBy" ->
    ceresHeaderWrapper CRSRandomBy
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "RandomWith" ->
    ceresHeaderWrapper CRSRandomWith
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addValueType
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "RandomWithBy" ->
    ceresHeaderWrapper CRSRandomWithBy
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "ElapseTime" ->
    ceresHeaderWrapper CRSElapseTime
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "SPControl" -> ceresHeaderWrapper CRSSPControl >>= addVariablePosition
  "SIControl" ->
    ceresHeaderWrapper CRSSIControl
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "SIInit" ->
    ceresHeaderWrapper CRSSIInit
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
 where
  (pHeader, pRest) = T.breakOn " " aText
  ceresHeaderWrapper cH =
    findPattern " " "[Fail] Reading CERES Header fails" (cH, pRest)
  parseCERESOperator  = readCERESOperatorWrapper
  parseValueType      = readValueTypeWrapper
  readCERESDelimiter  = findPattern " " "[Fail] Reading CERES delimiter"
  addVariablePosition = readAppliable parseVariablePosition
  addValueType        = readAppliable parseValueType
  addCERESOperator    = readAppliable parseCERESOperator


readCERESOperatorWrapper :: Text -> Result CERESOperator
readCERESOperatorWrapper = readWrapper "[Fail] Reading CERESOperator fails" readCERESOperator

readCERESOperator :: Text -> Maybe (CERESOperator, Text)
readCERESOperator aText
  | T.isPrefixOf "Add" aText = coReader "Add" COAAdd aText
  | T.isPrefixOf "Sub" aText = coReader "Sub" COASub aText
  | T.isPrefixOf "Mul" aText = coReader "Mul" COAMul aText
  | T.isPrefixOf "Div" aText = coReader "Div" COADiv aText
  | T.isPrefixOf "Mod" aText = coReader "Mod" COAMod aText
  | otherwise                  = Nothing
  where coReader key tf body = Just (tf, fromJust . T.stripPrefix key $ body)

