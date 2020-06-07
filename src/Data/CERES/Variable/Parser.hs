module Data.CERES.Variable.Parser where


import           Control.Monad                  ( (>=>) )

import           Data.Bifunctor
import           Data.Either
import qualified Data.IntMap.Strict            as IM
import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as TL
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
  where (pHeader, pRest) = TL.breakOn "[" aText

readVariableIndex :: Text -> Result VariableIndex
readVariableIndex aText = maybeNext (maybeNext eVI VINull mVINull)
                                    VIAtom
                                    mVIAtom
 where
  mVIAtom          = TL.stripPrefix "VIAtom" aText
  mVINull          = TL.stripPrefix "VINull" aText
  (pHeader, pRest) = TL.breakOn "=" aText
  viWrapper vi = findPattern "=" "[Fail] Reading VI Opener fails" (vi, pRest)
  eVI = case pHeader of
    "VII"     -> viWrapper VII >>= addIdx
    "VIN"     -> viWrapper VIN >>= addQuotedNKey
    "VIpN"    -> viWrapper VIpN >>= addQuotedNKey
    "VIIT"    -> viWrapper VIIT >>= addIdx >>= andTime
    "VINT"    -> viWrapper VINT >>= addQuotedNKey >>= andTime
    "VIpNT"   -> viWrapper VIpNT >>= addQuotedNKey >>= andTime
    "VIIRI"   -> viWrapper VIIRI >>= addIdx >>= andIndices
    "VINRI"   -> viWrapper VINRI >>= addQuotedNKey >>= andIndices
    "VIpNRI"  -> viWrapper VIpNRI >>= addQuotedNKey >>= andIndices
    "VIIRIT"  -> viWrapper VIIRIT >>= addIdx >>= andIndices >>= andTime
    "VINRIT"  -> viWrapper VINRIT >>= addQuotedNKey >>= andIndices >>= andTime
    "VIpNRIT" -> viWrapper VIpNRIT >>= addQuotedNKey >>= andIndices >>= andTime
    "VIV"     -> viWrapper VIV >>= readAppliable parseValue
    "VIPtr"   -> viWrapper VIPtr >>= readAppliable parseVariablePosition
    "PVII"    -> viWrapper PVII >>= addIdx
    "PVIN"    -> viWrapper PVIN >>= addQuotedNKey
    "PVIpN"   -> viWrapper PVIpN >>= addQuotedNKey
    "PVIT"    -> viWrapper PVIT >>= addTime
    "PVIIRI"  -> viWrapper PVIIRI >>= addIdx >>= andIndices
    "PVINRI"  -> viWrapper PVINRI >>= addQuotedNKey >>= andIndices
    "PVIpNRI" -> viWrapper PVIpNRI >>= addQuotedNKey >>= andIndices
    "PVIIRIT" -> viWrapper PVIIRIT >>= addIdx >>= andIndices >>= andTime
    "PVINRIT" -> viWrapper PVINRIT >>= addQuotedNKey >>= andIndices >>= andTime
    "PVIpNRIT" ->
      viWrapper PVIpNRIT >>= addQuotedNKey >>= andIndices >>= andTime
    _ -> Left ("[Fail] Reading VariableIndex fails", aText)
  readIdx         = readIntWrapper "[Fail] Reading Idx fails"
  readTime        = readIntWrapper "[Fail] Reading Time fails"
  readIndices     = eIntListReader
  readVIDelimiter = findPattern ":" "[Fail] Reading VI delimiter"
  addIdx          = readAppliable readIdx
  addTime         = readAppliable readTime
  addIndices      = readAppliable readIndices
  addValue        = readAppliable parseValue
  addQuotedNKey   = readAppliable readQuotedNKey
  andIdx          = readVIDelimiter >=> addIdx
  andTime         = readVIDelimiter >=> addTime
  andIndices      = readVIDelimiter >=> addIndices


parseValue :: Text -> Result Value
parseValue aText = readVType aText >>= readValue

-- TODO: Split as readVType and readOpener

readVType :: Text -> Result ValueType
readVType aText = if TL.isPrefixOf "A[|| " aText
  then Right (VTArr, fromJust . TL.stripPrefix "A[|| " $ aText)
  else maybe (Left ("[Fail]<getValue> Not have \"<| \"", pHeader)) rVType mPRest
 where
  (pHeader, mPRest) = second (TL.stripPrefix "<| ") . TL.breakOn "<| " $ aText
  rVType pRest = case pHeader of
    "IV" -> Right (VTInt, pRest)
    "DV" -> Right (VTDbl, pRest)
    "SV" -> Right (VTStr, pRest)
    "BV" -> Right (VTBool, pRest)
    "AV" -> Right (VTAtom, pRest)
    "PV" -> Right (VTPtr, pRest)
    "CV" -> Right (VTScr, pRest)
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
    VTScr  -> eScrValueReader pRest
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

eAtomValueReader aText = if TL.isPrefixOf "-" aText
  then Right (AtomValue, TL.tail aText)
  else Left ("[Fail] Reading AtomValue from body fails", aText)

readText :: Text -> Maybe (T.Text, Text)
readText aText = if TL.null pRest
  then Nothing
  else Just (TL.toStrict pStr, pRest)
  where (pStr, pRest) = TL.breakOn " |>" aText

readTextWrapper :: Message -> Text -> Result T.Text
readTextWrapper msg aText = maybe (Left (msg, aText)) Right (readText aText)

eStrValueReader = convertResult StrValue
  . readTextWrapper "[Fail] Reading StrValue from body fails"
eErrValueReader = convertResult ErrValue
  . readTextWrapper "[Fail] Reading ErrValue from body fails"

ePtrValueReader = convertResult PtrValue . parseVariablePosition

eScrValueReader =
  error
    "[Fail] Not yet implemented before integrate Variable.Parser and Script.Parser modules"


-- TODO: Make more monadic

eArrValueReader :: Text -> Result Value
eArrValueReader aText = eArrValueReaderSub IM.empty (" || " <> aText)
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
readValueType aText
  | TL.isPrefixOf "C-Int" aText = vtReader "C-Int" VTInt aText
  | TL.isPrefixOf "C-Dbl" aText = vtReader "C-Dbl" VTDbl aText
  | TL.isPrefixOf "CBool" aText = vtReader "CBool" VTBool aText
  | TL.isPrefixOf "CAtom" aText = vtReader "CAtom" VTAtom aText
  | TL.isPrefixOf "C-Arr" aText = vtReader "C-Arr" VTArr aText
  | TL.isPrefixOf "C-Str" aText = vtReader "C-Str" VTStr aText
  | TL.isPrefixOf "C-Scr" aText = vtReader "C-Scr" VTScr aText
  | TL.isPrefixOf "C-Err" aText = vtReader "C-Err" VTErr aText
  | otherwise                   = Nothing
  where vtReader key vt body = Just (vt, fromJust . TL.stripPrefix key $ body)
