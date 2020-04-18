module Data.CERES.Operator.Parser where


import           Control.Monad                  ( (>=>) )

import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as TL

import           TextShow

import           Data.CERES.Data
import           Data.CERES.Operator
import           Data.CERES.Parser
import           Data.CERES.Type


parseCERESOperator :: Text -> Result CERESOperator
parseCERESOperator aText = maybe
  (Left ("[Fail] Reading CERESOperator Opener", aText))
  (parseCERESOperatorSub >=> readCOCloser)
  mCO
 where
  mCO          = TL.stripPrefix "<" aText
  readCOCloser = findPattern ">" "[Fail] Reading CERESOperator Closer"

parseCERESOperatorSub :: Text -> Result CERESOperator
parseCERESOperatorSub aText = if TL.null pRest
  then Left ("[Fail] Reading CERESOperator body@Sub", aText)
  else case pBody of
    "Add"      -> Right (COAAdd, pRest)
    "Sub"      -> Right (COASub, pRest)
    "Mul"      -> Right (COAMul, pRest)
    "Div"      -> Right (COADiv, pRest)
    "Mod"      -> Right (COAMod, pRest)
    "Eql"      -> Right (COAEql, pRest)
    "Cmp"      -> Right (COACmp, pRest)
    "Swp"      -> Right (CORSwp, pRest)
    "Mov"      -> Right (CORMov, pRest)
    "Take"     -> Right (COTTake, pRest)
    "Drop"     -> Right (COTDrop, pRest)
    "Split"    -> Right (COTSplit, pRest)
    "Trim"     -> Right (COTTrim, pRest)
    "Append"   -> Right (COTAppend, pRest)
    "Concat"   -> Right (COTConcat, pRest)
    "Inter"    -> Right (COTInter, pRest)
    "Replace"  -> Right (COTReplace, pRest)
    "Reverse"  -> Right (COTReverse, pRest)
    "Justify"  -> Right (COTJustify, pRest)
    "Length"   -> Right (COTLength, pRest)
    "IsNull"   -> Right (COTIsNull, pRest)
    "IsPrefix" -> Right (COTIsPrefix, pRest)
    "IsInfix"  -> Right (COTIsInfix, pRest)
    "IsSuffix" -> Right (COTIsSuffix, pRest)
    _          -> readCustomCO
 where
  (pBody, pRest) = TL.breakOn ">" aText
  coReader key op body = Just (op, fromJust . TL.stripPrefix key $ body)
  readCustomCO
    | TL.isPrefixOf "A:" pBody  = Right (COA (customCOReader "A:"), pRest)
    | TL.isPrefixOf "R:" pBody  = Right (COR (customCOReader "R:"), pRest)
    | TL.isPrefixOf "T:" pBody  = Right (COT (customCOReader "T:"), pRest)
    | TL.isPrefixOf "E1:" pBody = Right (COE1 (customCOReader "E1:"), pRest)
    | TL.isPrefixOf "E2:" pBody = Right (COE2 (customCOReader "E2:"), pRest)
    | TL.isPrefixOf "E3:" pBody = Right (COE3 (customCOReader "E3:"), pRest)
    | TL.isPrefixOf "E4:" pBody = Right (COE4 (customCOReader "E4:"), pRest)
    | otherwise = Left ("[Fail] Reading Custom CERESOperator body@CCO", aText)
  customCOReader header =
    TL.toStrict . fromJust . TL.stripPrefix header $ pBody
