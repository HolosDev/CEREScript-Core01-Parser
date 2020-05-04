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
      >>= moreVariablePosition
  "InitVariableAt" ->
    ceresHeaderWrapper CRSInitVariableAt
      >>= addVariablePosition
      >>= moreVariablePosition
  "SetValue" ->
    ceresHeaderWrapper CRSSetValue
      >>= addVariablePosition
      >>= moreVariablePosition
  "DeleteVariable" ->
    ceresHeaderWrapper CRSDeleteVariable >>= addVariablePosition
  "CheckVariable" ->
    ceresHeaderWrapper CRSCheckVariable
      >>= addVariablePosition
      >>= moreVariablePosition
  "ModifyValue1" ->
    ceresHeaderWrapper CRSModifyValue1
      >>= addVariablePosition
      >>= moreCERESOperator
  "ModifyValue2" ->
    ceresHeaderWrapper CRSModifyValue2
      >>= addVariablePosition
      >>= moreVariablePosition
      >>= moreCERESOperator
  "ModifyValue3" ->
    ceresHeaderWrapper CRSModifyValue3
      >>= addVariablePosition
      >>= moreVariablePosition
      >>= moreCERESOperator
      >>= moreVariablePosition
  "CopyValue" ->
    ceresHeaderWrapper CRSCopyValue
      >>= addVariablePosition
      >>= moreVariablePosition
  "ConvertValue" ->
    ceresHeaderWrapper CRSConvertValue
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addValueType
  "ConvertValueBy" ->
    ceresHeaderWrapper CRSConvertValueBy
      >>= addVariablePosition
      >>= moreVariablePosition
  "ConvertValueWith" ->
    ceresHeaderWrapper CRSConvertValueWith
      >>= addVariablePosition
      >>= moreVariablePosition
  "ReplaceText" -> ceresHeaderWrapper CRSReplaceText >>= addVariablePosition
  "ReplaceTextTo" ->
    ceresHeaderWrapper CRSReplaceTextTo
      >>= addVariablePosition
      >>= moreVariablePosition
  "GetVPosition" ->
    ceresHeaderWrapper CRSGetVPosition
      >>= addVariablePosition
      >>= moreVariablePosition
  "SetVPosition" ->
    ceresHeaderWrapper CRSSetVPosition
      >>= addVariablePosition
      >>= moreVariablePosition
  "Random" ->
    ceresHeaderWrapper CRSRandom
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addValueType
  "RandomBy" ->
    ceresHeaderWrapper CRSRandomBy
      >>= addVariablePosition
      >>= moreVariablePosition
  "RandomWith" ->
    ceresHeaderWrapper CRSRandomWith
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addValueType
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
  "RandomWithBy" ->
    ceresHeaderWrapper CRSRandomWithBy
      >>= addVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
  "ElapseTime" ->
    ceresHeaderWrapper CRSElapseTime
      >>= addVariablePosition
      >>= moreVariablePosition
  "SPControl" -> ceresHeaderWrapper CRSSPControl >>= addVariablePosition
  "SIControl" ->
    ceresHeaderWrapper CRSSIControl
      >>= addVariablePosition
      >>= moreVariablePosition
  "SIInit" ->
    ceresHeaderWrapper CRSSIInit
      >>= addVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
  "SIEnd" -> ceresHeaderWrapper CRSSIEnd >>= addVariablePosition
  "Noop"  -> ceresHeaderWrapper CRSNoop
  "Log" ->
    ceresHeaderWrapper CRSLog >>= addVariablePosition >>= moreVariablePosition
  "ParseScript" ->
    ceresHeaderWrapper CRSParseScript
      >>= addVariablePosition
      >>= moreVariablePosition
  "ToInterpreter1" ->
    ceresHeaderWrapper CRSToInterpreter1 >>= addCHeader >>= moreVariablePosition
  "ToInterpreter2" ->
    ceresHeaderWrapper CRSToInterpreter2
      >>= addCHeader
      >>= moreVariablePosition
      >>= moreVariablePosition
  "ToInterpreter3" ->
    ceresHeaderWrapper CRSToInterpreter3
      >>= addCHeader
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
  "ToInterpreter4" ->
    ceresHeaderWrapper CRSToInterpreter4
      >>= addCHeader
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
  "ToInterpreter5" ->
    ceresHeaderWrapper CRSToInterpreter5
      >>= addCHeader
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
  "ToInterpreter6" ->
    ceresHeaderWrapper CRSToInterpreter6
      >>= addCHeader
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
  "ToInterpreter7" ->
    ceresHeaderWrapper CRSToInterpreter7
      >>= addCHeader
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
  "ToInterpreter8" ->
    ceresHeaderWrapper CRSToInterpreter8
      >>= addCHeader
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
  "Extend1" ->
    ceresHeaderWrapper CRSExtend1 >>= addCHeader >>= moreVariablePosition
  "Extend2" ->
    ceresHeaderWrapper CRSExtend2
      >>= addCHeader
      >>= moreVariablePosition
      >>= moreVariablePosition
  "Extend3" ->
    ceresHeaderWrapper CRSExtend3
      >>= addCHeader
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
  "Extend4" ->
    ceresHeaderWrapper CRSExtend4
      >>= addCHeader
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
  "Extend5" ->
    ceresHeaderWrapper CRSExtend5
      >>= addCHeader
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
  "Extend6" ->
    ceresHeaderWrapper CRSExtend6
      >>= addCHeader
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
  "Extend7" ->
    ceresHeaderWrapper CRSExtend7
      >>= addCHeader
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
  "Extend8" ->
    ceresHeaderWrapper CRSExtend8
      >>= addCHeader
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
      >>= moreVariablePosition
  _ -> Left ("[Fail] No such CERES Header", aText)
 where
  (pHeader, pRest) = T.breakOn " " aText
  ceresHeaderWrapper cH =
    findPattern " " "[Fail] Reading CERES Header fails" (cH, pRest)
  parseValueType       = readValueTypeWrapper
  readCERESDelimiter   = findPattern " " "[Fail] Reading CERES delimiter"
  addVariablePosition  = readAppliable parseVariablePosition
  addValueType         = readAppliable parseValueType
  addCERESOperator     = readAppliable parseCERESOperator
  addCHeader           = readAppliable parseCHeader
  moreVariablePosition = readCERESDelimiter >=> addVariablePosition
  moreCERESOperator    = readCERESDelimiter >=> addCERESOperator

parseCHeader aText = if T.null pRest
  then Left ("[Fail] Reading CERES CHeader fails", aText)
  else Right (T.toStrict pHeader, pRest)
  where (pHeader, pRest) = T.breakOn " " aText
