module Data.CERES.Variable.Parser where


import           Control.Monad                  ( (>=>) )

import           Data.Bifunctor
import           Data.Either
import qualified Data.IntMap                   as IM
import           Data.Maybe
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Read           as TR

import           Data.CERES.Type
import           Data.CERES.Data

import           Data.CERES.Parser

import           Debug.Trace

parseVariablePosition :: Text -> Result VariablePosition
parseVariablePosition = readCompositor
  VP
  (readVariablePlace >=> findPattern "[" "[Fail] Reading VP Opener")
  (readVariableIndex >=> findPattern "]" "[Fail] Reading VP Closer")

readVariablePlace :: Text -> Result VariablePlace
readVariablePlace aText = case pHeader of
  "AtTricky" -> Right (AtTricky, pRest)
  "AtPtr"    -> Right (AtPtr, pRest)
  "AtWorld"  -> Right (AtWorld, pRest)
  "AtTime"   -> Right (AtTime, pRest)
  "AtNWorld" -> Right (AtNWorld, pRest)
  "AtNTime"  -> Right (AtNTime, pRest)
  "AtDict"   -> Right (AtDict, pRest)
  "AtNDict"  -> Right (AtNDict, pRest)
  "AtVars"   -> Right (AtVars, pRest)
  "AtNVars"  -> Right (AtNVars, pRest)
  "AtLVars"  -> Right (AtLVars, pRest)
  "AtLNVars" -> Right (AtLNVars, pRest)
  "AtLTemp"  -> Right (AtLTemp, pRest)
  "AtLNTemp" -> Right (AtLNTemp, pRest)
  "AtReg"    -> Right (AtReg, pRest)
  "AtHere"   -> Right (AtHere, pRest)
  "AtNull"   -> Right (AtNull, pRest)
  _          -> Left ("[Fail] No such VariablePlace", aText)
  where (pHeader, pRest) = T.breakOn "[" aText

readVariableIndex :: Text -> Result VariableIndex
readVariableIndex aText = maybeNext (maybeNext eVI VINull mVINull)
                                    VIAtom
                                    mVIAtom
 where
  mVIAtom          = T.stripPrefix "VIAtom" aText
  mVINull          = T.stripPrefix "VINull" aText
  (pHeader, pRest) = T.breakOn "=" aText
  viWrapper vi = findPattern "=" "[Fail] Reading VI Opener fails" (vi, pRest)
  eVI = case pHeader of
    "VII"  -> viWrapper VII >>= readAppliable readIdx
    "VIN"  -> viWrapper VIN >>= readAppliable readQuotedNKey
    "VIpN" -> viWrapper VIpN >>= readAppliable readQuotedNKey
    "VIIT" ->
      viWrapper VIIT
        >>= readAppliable readIdx
        >>= readVIDelimiter
        >>= readAppliable readTime
    "VINT" ->
      viWrapper VINT
        >>= readAppliable readQuotedNKey
        >>= readVIDelimiter
        >>= readAppliable readTime
    "VIpNT" ->
      viWrapper VIpNT
        >>= readAppliable readQuotedNKey
        >>= readVIDelimiter
        >>= readAppliable readTime
    "VIIRI" ->
      viWrapper VIIRI
        >>= readAppliable readIdx
        >>= readVIDelimiter
        >>= readAppliable readIndices
    "VINRI" ->
      viWrapper VINRI
        >>= readAppliable readQuotedNKey
        >>= readVIDelimiter
        >>= readAppliable readIndices
    "VIpNRI" ->
      viWrapper VIpNRI
        >>= readAppliable readQuotedNKey
        >>= readVIDelimiter
        >>= readAppliable readIndices
    "VIIRIT" ->
      viWrapper VIIRIT
        >>= readAppliable readIdx
        >>= readVIDelimiter
        >>= readAppliable readIndices
        >>= readVIDelimiter
        >>= readAppliable readTime
    "VINRIT" ->
      viWrapper VINRIT
        >>= readAppliable readQuotedNKey
        >>= readVIDelimiter
        >>= readAppliable readIndices
        >>= readVIDelimiter
        >>= readAppliable readTime
    "VIpNRIT" ->
      viWrapper VIpNRIT
        >>= readAppliable readQuotedNKey
        >>= readVIDelimiter
        >>= readAppliable readIndices
        >>= readVIDelimiter
        >>= readAppliable readTime
    "VIV"   -> viWrapper VIV >>= readAppliable parseValue
    "VIPtr" -> viWrapper VIPtr >>= readAppliable parseVariablePosition
    "PVII"  -> viWrapper PVII >>= readAppliable readIdx
    "PVIN"  -> viWrapper PVIN >>= readAppliable readQuotedNKey
    "PVIpN" -> viWrapper PVIpN >>= readAppliable readQuotedNKey
    "PVIT"  -> viWrapper PVIT >>= readAppliable readTime
    "PVIIRI" ->
      viWrapper PVIIRI
        >>= readAppliable readIdx
        >>= readVIDelimiter
        >>= readAppliable readIndices
    "PVINRI" ->
      viWrapper PVINRI
        >>= readAppliable readQuotedNKey
        >>= readVIDelimiter
        >>= readAppliable readIndices
    "PVIpNRI" ->
      viWrapper PVIpNRI
        >>= readAppliable readQuotedNKey
        >>= readVIDelimiter
        >>= readAppliable readIndices
    "PVIIRIT" ->
      viWrapper PVIIRIT
        >>= readAppliable readIdx
        >>= readVIDelimiter
        >>= readAppliable readIndices
        >>= readVIDelimiter
        >>= readAppliable readTime
    "PVINRIT" ->
      viWrapper PVINRIT
        >>= readAppliable readQuotedNKey
        >>= readVIDelimiter
        >>= readAppliable readIndices
        >>= readVIDelimiter
        >>= readAppliable readTime
    "PVIpNRIT" ->
      viWrapper PVIpNRIT
        >>= readAppliable readQuotedNKey
        >>= readVIDelimiter
        >>= readAppliable readIndices
        >>= readVIDelimiter
        >>= readAppliable readTime
    _ -> Left ("[Fail] Reading VariableIndex fails", aText)
  readIdx         = readIntWrapper "[Fail] Reading Idx fails"
  readTime        = readIntWrapper "[Fail] Reading Time fails"
  readIndices     = eIntListReader
  readVIDelimiter = findPattern ":" "[Fail] Reading VI delimiter"


parseValue :: Text -> Result Value
parseValue aText = readVType aText >>= readValue

-- TODO: Split as readVType and readOpener

readVType :: Text -> Result ValueType
readVType aText = if T.isPrefixOf "A[|| " aText
  then Right (VTArr, fromJust . T.stripPrefix "A[|| " $ aText)
  else maybe (Left ("[Fail]<getValue> Not have \"<| \"", pHeader)) rVType mPRest
 where
  (pHeader, mPRest) = second (T.stripPrefix "<| ") . T.breakOn "<| " $ aText
  rVType pRest = case pHeader of
    "IV" -> Right (VTInt, pRest)
    "DV" -> Right (VTDbl, pRest)
    "SV" -> Right (VTStr, pRest)
    "BV" -> Right (VTBool, pRest)
    "AV" -> Right (VTAtom, pRest)
    "PV" -> Right (VTPtr, pRest)
    "EV" -> Right (VTErr, pRest)
    _    -> Left ("[Fail] Reading Header fails", pHeader)

readValue :: (ValueType, Text) -> Result Value
readValue (vType, pRest) = if vType == VTArr
  then eArrValueReader pRest
  else medium >>= readValueCloser
 where
  medium = case vType of
    VTInt  -> eIntValueReader pRest
    VTDbl  -> eDblValueReader pRest
    VTStr  -> eStrValueReader pRest
    VTBool -> eBoolValueReader pRest
    VTAtom -> eAtomValueReader pRest
    VTPtr  -> ePtrValueReader pRest
    VTErr  -> eErrValueReader pRest
    _      -> error "[ERROR]<readValue> Can't be reached"


eIntValueReader :: Text -> Result Value
eIntValueReader = convertResult IntValue
  . readIntWrapper "[Fail] Reading IntValue from body fails"

eDblValueReader :: Text -> Result Value
eDblValueReader = convertResult DblValue
  . readDblWrapper "[Fail] Reading DblValue from body fails"

eBoolValueReader :: Text -> Result Value
eBoolValueReader = convertResult BoolValue
  . readBoolWrapper "[Fail] Reading BoolValue from body fails"

eAtomValueReader aText = if T.isPrefixOf "-" aText
  then Right (AtomValue, T.tail aText)
  else Left ("[Fail] Reading AtomValue from body fails", aText)

readText :: Text -> Maybe (Text, Text)
readText aText = if T.null pRest then Nothing else Just (pStr, pRest)
  where (pStr, pRest) = T.breakOn " |>" aText

readTextWrapper :: Message -> Text -> Result Text
readTextWrapper msg aText = maybe (Left (msg, aText)) Right (readText aText)

eStrValueReader = convertResult StrValue
  . readTextWrapper "[Fail] Reading StrValue from body fails"
eErrValueReader = convertResult ErrValue
  . readTextWrapper "[Fail] Reading ErrValue from body fails"

ePtrValueReader = convertResult PtrValue . parseVariablePosition


-- TODO: Make more monadic

eArrValueReader :: Text -> Result Value
eArrValueReader aText = eArrValueReaderSub IM.empty (T.append " || " aText)
eArrValueReaderSub im aText
  | isRight shownItem = eArrValueReaderSub (IM.insert idx item im) aItemRest
  | isRight shownEnd  = Right (ArrValue im, aEndRest)
  | otherwise         = Left ("[Fail] Reading ArrValue from body fails", aText)
 where
  shownItem                   = readPair aText
  Right (Just (idx, item), _) = shownItem
  aItemRest                   = either snd snd shownItem
  shownEnd                    = readArrValueCloser (undefined, aItemRest)
  Right (_, aEndRest)         = shownEnd

readPair :: Text -> Result (Maybe (Idx, Value))
readPair aText =
  readArrayDelimiter ((), aText)
    >>= readIndex
    >>= readIndexDelimiter
    >>= readItem

readIndex :: (a, Text) -> Result Idx
readIndex (_, aText) =
  readIntWrapper "[Fail] Reading Index of ArrValue from body fails" aText

readIndexDelimiter = findPattern ":" "[Fail] Reading index delimiter fails"
readArrayDelimiter = findPattern " || " "[Fail] Reading item delimiter fails"

readArrValueCloser = findPattern " ||]" "[Fail] Reading ArrValue closer fails"
readValueCloser = findPattern " |>" "[Fail] Reading Value closer fails"

readItem :: (Idx, Text) -> Result (Maybe (Idx, Value))
readItem (idx, aText) = convertResult (\v -> Just (idx, v)) (parseValue aText)


readValueTypeWrapper :: Text -> Result ValueType
readValueTypeWrapper =
  readWrapper "[Fail] Reading ValueType fails" readValueType

readValueType :: Text -> Maybe (ValueType, Text)
readValueType aText | T.isPrefixOf "C-Int" aText = vtReader "C-Int" VTInt aText
                    | T.isPrefixOf "C-Dbl" aText = vtReader "C-Dbl" VTDbl aText
                    | T.isPrefixOf "CBool" aText = vtReader "CBool" VTBool aText
                    | T.isPrefixOf "CAtom" aText = vtReader "CAtom" VTAtom aText
                    | T.isPrefixOf "C-Arr" aText = vtReader "C-Arr" VTArr aText
                    | T.isPrefixOf "C-Str" aText = vtReader "C-Str" VTStr aText
                    | T.isPrefixOf "C-Err" aText = vtReader "C-Err" VTErr aText
                    | otherwise                  = Nothing
  where vtReader key vt body = Just (vt, fromJust . T.stripPrefix key $ body)
