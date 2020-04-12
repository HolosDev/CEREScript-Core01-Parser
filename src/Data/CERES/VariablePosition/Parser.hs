module Data.CERES.VariablePosition.Parser where


import           Data.Bifunctor
import           Data.Either
import           Data.Maybe
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Read           as TR

import           Data.CERES.Type
import           Data.CERES.Value
import           Data.CERES.VariablePosition

import           Data.CERES.Parser
import           Data.CERES.Value.Parser

import           Debug.Trace

parseVariablePosition :: Text -> Result VariablePosition
parseVariablePosition = readCompositor
  (\t -> readVariablePlace t >>= findPattern "[" "[Fail] Reading VP Opener")
  (\t -> readVariableIndex t >>= findPattern "]" "[Fail] Reading VP Closer")
  VP

readVariablePlace :: Text -> Result VariablePlace
readVariablePlace aText = case pHeader of
  "AtTricky" -> Right (AtTricky, pRest)
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
    "VII" -> viWrapper VII >>= readAppliable readIdx
    "VIN" -> viWrapper VIN >>= readAppliable readQuotedNKey
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
    "VIV"  -> viWrapper VIV >>= readAppliable parseValue
    "PVII" -> viWrapper PVII >>= readAppliable readIdx
    "PVIN" -> viWrapper PVIN >>= readAppliable readQuotedNKey
    "PVIT" -> viWrapper PVIT >>= readAppliable readTime
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
    _ -> Left ("[Fail] Reading VariableIndex fails", pRest)
  readIdx         = readIntWrapper "[Fail] Reading Idx fails"
  readTime        = readIntWrapper "[Fail] Reading Time fails"
  readIndices     = eIntListReader
  readVIDelimiter = findPattern ":" "[Fail] Reading VI delimiter"
