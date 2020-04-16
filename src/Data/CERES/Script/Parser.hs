module Data.CERES.Script.Parser where


import           Data.Maybe
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T

import           TextShow

import           Data.CERES.Data
import           Data.CERES.Operator
import           Data.CERES.Parser
import           Data.CERES.Type
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
  "ToInterpreter1" ->
    ceresHeaderWrapper CRSToInterpreter1
      >>= addVariablePosition
  "ToInterpreter2" ->
    ceresHeaderWrapper CRSToInterpreter2
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "ToInterpreter3" ->
    ceresHeaderWrapper CRSToInterpreter3
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "ToInterpreter4" ->
    ceresHeaderWrapper CRSToInterpreter4
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "ToInterpreter5" ->
    ceresHeaderWrapper CRSToInterpreter5
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "ToInterpreter6" ->
    ceresHeaderWrapper CRSToInterpreter6
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "ToInterpreter7" ->
    ceresHeaderWrapper CRSToInterpreter7
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "ToInterpreter8" ->
    ceresHeaderWrapper CRSToInterpreter8
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "Extend1" ->
    ceresHeaderWrapper CRSExtend1
      >>= addVariablePosition
  "Extend2" ->
    ceresHeaderWrapper CRSExtend2
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "Extend3" ->
    ceresHeaderWrapper CRSExtend3
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "Extend4" ->
    ceresHeaderWrapper CRSExtend4
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "Extend5" ->
    ceresHeaderWrapper CRSExtend5
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "Extend6" ->
    ceresHeaderWrapper CRSExtend6
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "Extend7" ->
    ceresHeaderWrapper CRSExtend7
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "Extend8" ->
    ceresHeaderWrapper CRSExtend8
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
      >>= readCERESDelimiter
      >>= addVariablePosition
  "Noop" ->
    ceresHeaderWrapper CRSNoop
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
readCERESOperatorWrapper =
  readWrapper "[Fail] Reading CERESOperator fails" readCERESOperator

readCERESOperator :: Text -> Maybe (CERESOperator, Text)
readCERESOperator aText | T.isPrefixOf "Add" aText = coReader "Add" COAAdd aText
                        | T.isPrefixOf "Sub" aText = coReader "Sub" COASub aText
                        | T.isPrefixOf "Mul" aText = coReader "Mul" COAMul aText
                        | T.isPrefixOf "Div" aText = coReader "Div" COADiv aText
                        | T.isPrefixOf "Mod" aText = coReader "Mod" COAMod aText
                        | T.isPrefixOf "Swp" aText = coReader "Swp" CORSwp aText
                        | T.isPrefixOf "Mov" aText = coReader "Mov" CORMov aText
                        | otherwise                = Nothing
  where coReader key tf body = Just (tf, fromJust . T.stripPrefix key $ body)

