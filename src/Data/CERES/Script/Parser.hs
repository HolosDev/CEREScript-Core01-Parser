module Data.CERES.Script.Parser where


import           Control.Monad

import           Data.Maybe
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T

import           TextShow

import           Data.CERES.Data
import           Data.CERES.Operator
import           Data.CERES.Parser
import           Data.CERES.Type
import           Data.CERES.Operator.Parser
import           Data.CERES.Variable.Parser


--TODO: Read CEREScript from line.
--NOTE: Problem: How to implement this as recursively?
parseCEREScript :: Text -> Result CEREScript
parseCEREScript aText = parseCEREScriptSub ([], aText)

parseCEREScriptSub :: (CEREScript, Text) -> Result CEREScript
parseCEREScriptSub acc@(cScript, aText) = if isEnd
  then Right (reverse cScript, aText)
  else
    compositeResult (:) parseCERES acc
    >>= readCEREScriptDelimiter
    >>= parseCEREScriptSub
 where
  isEnd = aText `elem` ["", "\n"]
  readCEREScriptDelimiter =
    findPattern "\n" "[Fail] Reading CEREScript delimiter"


parseCERES :: Text -> Result CERES
parseCERES aText = case pHeader of
  "InitVariable" ->
    ceresHeaderWrapper CRSInitVariable
      >>= addVariablePosition
      >>= andVariablePosition
  "InitVariableAt" ->
    ceresHeaderWrapper CRSInitVariableAt
      >>= addVariablePosition
      >>= andVariablePosition
  "SetValue" ->
    ceresHeaderWrapper CRSSetValue
      >>= addVariablePosition
      >>= andVariablePosition
  "DeleteVariable" ->
    ceresHeaderWrapper CRSDeleteVariable >>= addVariablePosition
  "CheckVariable" ->
    ceresHeaderWrapper CRSCheckVariable
      >>= addVariablePosition
      >>= andVariablePosition
  "ModifyValue1" ->
    ceresHeaderWrapper CRSModifyValue1
      >>= addVariablePosition
      >>= andCERESOperator
  "ModifyValue2" ->
    ceresHeaderWrapper CRSModifyValue2
      >>= addVariablePosition
      >>= andVariablePosition
      >>= andCERESOperator
  "ModifyValue3" ->
    ceresHeaderWrapper CRSModifyValue3
      >>= addVariablePosition
      >>= andVariablePosition
      >>= andCERESOperator
      >>= andVariablePosition
  "CopyValue" ->
    ceresHeaderWrapper CRSCopyValue
      >>= addVariablePosition
      >>= andVariablePosition
  "ConvertValue" ->
    ceresHeaderWrapper CRSConvertValue
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addValueType
  "ConvertValueBy" ->
    ceresHeaderWrapper CRSConvertValueBy
      >>= addVariablePosition
      >>= andVariablePosition
  "ConvertValueWith" ->
    ceresHeaderWrapper CRSConvertValueWith
      >>= addVariablePosition
      >>= andVariablePosition
  "ReplaceText" -> ceresHeaderWrapper CRSReplaceText >>= addVariablePosition
  "ReplaceTextTo" ->
    ceresHeaderWrapper CRSReplaceTextTo
      >>= addVariablePosition
      >>= andVariablePosition
  "GetVPosition" ->
    ceresHeaderWrapper CRSGetVPosition
      >>= addVariablePosition
      >>= andVariablePosition
  "SetVPosition" ->
    ceresHeaderWrapper CRSSetVPosition
      >>= addVariablePosition
      >>= andVariablePosition
  "Random" ->
    ceresHeaderWrapper CRSRandom
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addValueType
  "RandomBy" ->
    ceresHeaderWrapper CRSRandomBy
      >>= addVariablePosition
      >>= andVariablePosition
  "RandomWith" ->
    ceresHeaderWrapper CRSRandomWith
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addValueType
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
  "RandomWithBy" ->
    ceresHeaderWrapper CRSRandomWithBy
      >>= addVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
  "ElapseTime" ->
    ceresHeaderWrapper CRSElapseTime
      >>= addVariablePosition
      >>= andVariablePosition
  "SPControl" -> ceresHeaderWrapper CRSSPControl >>= addVariablePosition
  "SIControl" ->
    ceresHeaderWrapper CRSSIControl
      >>= addVariablePosition
      >>= andVariablePosition
  "SIInit" ->
    ceresHeaderWrapper CRSSIInit
      >>= addVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
  "SIEnd" -> ceresHeaderWrapper CRSSIEnd >>= addVariablePosition
  "Noop"  -> ceresHeaderWrapper CRSNoop
  "Log" ->
    ceresHeaderWrapper CRSLog >>= addVariablePosition >>= andVariablePosition
  "ParseScript" ->
    ceresHeaderWrapper CRSParseScript
      >>= addVariablePosition
      >>= andVariablePosition
  "ToInterpreter1" ->
    ceresHeaderWrapper CRSToInterpreter1 >>= addCHeader >>= andVariablePosition
  "ToInterpreter2" ->
    ceresHeaderWrapper CRSToInterpreter2
      >>= addCHeader
      >>= andVariablePosition
      >>= andVariablePosition
  "ToInterpreter3" ->
    ceresHeaderWrapper CRSToInterpreter3
      >>= addCHeader
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
  "ToInterpreter4" ->
    ceresHeaderWrapper CRSToInterpreter4
      >>= addCHeader
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
  "ToInterpreter5" ->
    ceresHeaderWrapper CRSToInterpreter5
      >>= addCHeader
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
  "ToInterpreter6" ->
    ceresHeaderWrapper CRSToInterpreter6
      >>= addCHeader
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
  "ToInterpreter7" ->
    ceresHeaderWrapper CRSToInterpreter7
      >>= addCHeader
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
  "ToInterpreter8" ->
    ceresHeaderWrapper CRSToInterpreter8
      >>= addCHeader
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
  "Extend1" ->
    ceresHeaderWrapper CRSExtend1 >>= addCHeader >>= andVariablePosition
  "Extend2" ->
    ceresHeaderWrapper CRSExtend2
      >>= addCHeader
      >>= andVariablePosition
      >>= andVariablePosition
  "Extend3" ->
    ceresHeaderWrapper CRSExtend3
      >>= addCHeader
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
  "Extend4" ->
    ceresHeaderWrapper CRSExtend4
      >>= addCHeader
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
  "Extend5" ->
    ceresHeaderWrapper CRSExtend5
      >>= addCHeader
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
  "Extend6" ->
    ceresHeaderWrapper CRSExtend6
      >>= addCHeader
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
  "Extend7" ->
    ceresHeaderWrapper CRSExtend7
      >>= addCHeader
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
  "Extend8" ->
    ceresHeaderWrapper CRSExtend8
      >>= addCHeader
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
      >>= andVariablePosition
  _ -> Left ("[Fail] No such CERES Header", aText)
 where
  (pHeader, pRest) = T.breakOn " " aText
  ceresHeaderWrapper cH =
    findPattern " " "[Fail] Reading CERES Header fails" (cH, pRest)
  parseValueType      = readValueTypeWrapper
  readCERESDelimiter  = findPattern " " "[Fail] Reading CERES delimiter"
  addVariablePosition = readAppliable parseVariablePosition
  addValueType        = readAppliable parseValueType
  addCERESOperator    = readAppliable parseCERESOperator
  addCHeader          = readAppliable parseCHeader
  andVariablePosition = readCERESDelimiter >=> addVariablePosition
  andCERESOperator    = readCERESDelimiter >=> addCERESOperator

parseCHeader aText = if T.null pRest
  then Left ("[Fail] Reading CERES CHeader fails", aText)
  else Right (T.toStrict pHeader, pRest)
  where (pHeader, pRest) = T.breakOn " " aText
