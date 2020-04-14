module Data.CERES.VariablePosition.Parser where


import           Control.Monad                  ( (>=>) )

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
  VP
  (readVariablePlace >=> findPattern "[" "[Fail] Reading VP Opener")
  (readVariableIndex >=> findPattern "]" "[Fail] Reading VP Closer")

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
    _ -> Left ("[Fail] Reading VariableIndex fails", pRest)
  readIdx         = readIntWrapper "[Fail] Reading Idx fails"
  readTime        = readIntWrapper "[Fail] Reading Time fails"
  readIndices     = eIntListReader
  readVIDelimiter = findPattern ":" "[Fail] Reading VI delimiter"
