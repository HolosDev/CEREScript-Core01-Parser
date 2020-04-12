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
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
  "InitVariableAt" ->
    ceresHeaderWrapper CRSInitVariableAt
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
  "SetValue" ->
    ceresHeaderWrapper CRSSetValue
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
  "DeleteVariable" ->
    ceresHeaderWrapper CRSDeleteVariable
    >>= readAppliable parseVariablePosition
  "ModifyValue" ->
    ceresHeaderWrapper CRSModifyValue
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseCERESOperator
  "CopyValue" ->
    ceresHeaderWrapper CRSCopyValue
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
  "ConvertValue" ->
    ceresHeaderWrapper CRSConvertValue
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseValueType
  "ConvertValueBy" ->
    ceresHeaderWrapper CRSConvertValueBy
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
  "ConvertValueWith" ->
    ceresHeaderWrapper CRSConvertValueWith
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
  "ReplaceText" ->
    ceresHeaderWrapper CRSReplaceText
      >>= readAppliable parseVariablePosition
  "ReplaceTextTo" ->
    ceresHeaderWrapper CRSReplaceTextTo
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
  "Random" ->
    ceresHeaderWrapper CRSRandom
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseValueType
  "RandomBy" ->
    ceresHeaderWrapper CRSRandomBy
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
  "RandomWith" ->
    ceresHeaderWrapper CRSRandomWith
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseValueType
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
  "RandomWithBy" ->
    ceresHeaderWrapper CRSRandomWithBy
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
  "ElapseTime" ->
    ceresHeaderWrapper CRSElapseTime
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
  "SPControl" ->
    ceresHeaderWrapper CRSSPControl
      >>= readAppliable parseVariablePosition
  "SIControl" ->
    ceresHeaderWrapper CRSSIControl
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
  "SIInit" ->
    ceresHeaderWrapper CRSSIInit
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
      >>= readCERESDelimiter
      >>= readAppliable parseVariablePosition
 where
  (pHeader, pRest) = T.breakOn " " aText
  ceresHeaderWrapper cH =
    findPattern " " "[Fail] Reading CERES Header fails" (cH, pRest)
  parseCERESOperator = readCERESOperatorWrapper
  parseValueType     = readValueTypeWrapper
  readCERESDelimiter = findPattern " " "[Fail] Reading CERES delimiter"


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

