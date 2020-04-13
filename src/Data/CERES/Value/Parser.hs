module Data.CERES.Value.Parser where


import           Data.Bifunctor
import           Data.Either
import qualified Data.IntMap                   as IM
import           Data.Maybe
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Read           as TR

import           Data.CERES.Type
import           Data.CERES.Value

import           Data.CERES.Parser

import           Debug.Trace


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
    "BV" -> Right (VTBool, pRest)
    "AV" -> Right (VTAtom, pRest)
    "EV" -> Right (VTErr, pRest)
    "SV" -> Right (VTStr, pRest)
    _    -> Left ("[Fail] Reading Header fails", pHeader)

readValue :: (ValueType, Text) -> Result Value
readValue (vType, pRest) = if vType == VTArr
  then eArrValueReader pRest
  else medium >>= readValueCloser
 where
  medium = case vType of
    VTInt  -> eIntValueReader pRest
    VTDbl  -> eDblValueReader pRest
    VTBool -> eBoolValueReader pRest
    VTAtom -> eAtomValueReader pRest
    VTErr  -> eErrValueReader pRest
    VTStr  -> eStrValueReader pRest
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
readValueType aText | T.isPrefixOf "C-Int" aText = coReader "C-Int" VTInt aText
                    | T.isPrefixOf "C-Dbl" aText = coReader "C-Dbl" VTDbl aText
                    | T.isPrefixOf "CBool" aText = coReader "CBool" VTBool aText
                    | T.isPrefixOf "CAtom" aText = coReader "CAtom" VTAtom aText
                    | T.isPrefixOf "C-Arr" aText = coReader "C-Arr" VTArr aText
                    | T.isPrefixOf "C-Str" aText = coReader "C-Str" VTStr aText
                    | T.isPrefixOf "C-Err" aText = coReader "C-Err" VTErr aText
                    | otherwise                  = Nothing
  where coReader key tf body = Just (tf, fromJust . T.stripPrefix key $ body)
