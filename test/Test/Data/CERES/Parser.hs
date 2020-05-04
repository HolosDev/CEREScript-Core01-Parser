module Test.Data.CERES.Parser where


import           Test.HUnit.Base

import           Data.Either
import qualified Data.Text                     as T
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as TL

import           TextShow


import           Data.CERES.Type
import           Data.CERES.Parser


showtl :: TextShow a => a -> Text
showtl = TextShow.showtl

eqInverseParsingTester
  :: (Eq a, Show a)
  => String
  -> Result a
  -> a
  -> (Text -> Result a)
  -> (a -> Text)
  -> Assertion
eqInverseParsingTester msg rA vA' parseF showF = case rA of
  Right (vA, rest) -> if vA == vA'
    then assertEqual msg (Right (vA, rest)) (parseF (TL.append (showF vA) rest))
    else error "[ERROR] Input is not equal"
  Left (lMsg, rest) ->
    assertEqual msg (Left (lMsg, rest)) (parseF (TL.append (showF vA') rest))

eqParsingTester
  :: (Eq a, Show a)
  => String
  -> Result a
  -> (Text -> Result a)
  -> Text
  -> Assertion
eqParsingTester msg rA parseF coreText = case rA of
  Right (vA, rest) ->
    assertEqual msg (Right (vA, rest)) (parseF (TL.append coreText rest))
  Left (lMsg, rest) ->
    assertEqual msg (Left (lMsg, rest)) (parseF (TL.append coreText rest))
