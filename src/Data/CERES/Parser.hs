-- {-# LANGUAGE ExplicitForAll #-}
module Data.CERES.Parser where


import           Data.Bifunctor
import           Data.Either
import           Data.List                      ( sort )
import           Data.Maybe
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Read           as TR

import           Data.CERES.Type
import           Data.CERES.Data

import           Debug.Trace


type Result a = Either (Message, Text) (a, Text)

getResult :: Result a -> Maybe a
getResult (Left  _     ) = Nothing
getResult (Right (a, _)) = Just a
getMessage (Left  (msg, _)) = Just msg
getMessage (Right _       ) = Nothing

convertResult :: (a -> b) -> Result a -> Result b
convertResult f = either (\(e, t) -> Left (e, t)) (\(v, t) -> Right (f v, t))

convertInternalResult :: (a -> b) -> (a, Text) -> Result b
convertInternalResult f (a, t) = Right (f a, t)

readAppliable :: (Text -> Result a) -> (a -> b, Text) -> Result b
readAppliable reader (f, aText) = convertResult f (reader aText)

compositeResult :: (a -> b -> c) -> (Text -> Result a) -> (b, Text) -> Result c
compositeResult f readerA (b, aText) =
  either (\(e, t) -> Left (e, t)) (\(a, t) -> Right (f a b, t)) (readerA aText)

--readAnyway :: forall a b. (Text -> Result b) -> (a,Text) -> Result b
readAnyway :: (Text -> Result b) -> (a, Text) -> Result b
readAnyway reader (_, aText) = reader aText

readLifter :: ((a, Text) -> Result b) -> Text -> Result b
readLifter reader aText = reader (undefined, aText)

readCompositor
  :: (a -> b -> c)
  -> (Text -> Result a)
  -> (Text -> Result b)
  -> Text
  -> Result c
readCompositor f readerA readerB aText = convertResult (\_ -> f rA rB) eB
 where
  eA                 = readerA aText
  Right (rA, _)      = eA
  eB                 = eA >>= readAnyway readerB
  Right (rB, aRestB) = eB


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

maybeNext :: Result a -> a -> Maybe Text -> Result a
maybeNext base a = maybe base (\t -> Right (a, t))


findPattern :: Text -> Message -> (a, Text) -> Result a
findPattern aPattern msg (a, aText) =
  maybe (Left (msg, aText)) (\t -> Right (a, t)) (T.stripPrefix aPattern aText)

-- passingReader :: (Text -> Result a) -> Text -> (b,Text)

readWrapper :: Message -> (Text -> Maybe (a, Text)) -> Text -> Result a
readWrapper msg mReader aText = maybe (Left (msg, aText)) Right (mReader aText)

readIntWrapper :: Message -> Text -> Result Int
readIntWrapper msg = readWrapper msg readInt

readInt :: Text -> Maybe (Int, Text)
readInt aText = eitherToMaybe (TR.signed TR.decimal aText)


readDblWrapper :: Message -> Text -> Result Double
readDblWrapper msg = readWrapper msg readDbl

readDbl :: Text -> Maybe (Double, Text)
readDbl aText = eitherToMaybe (TR.rational aText)


readBoolWrapper :: Message -> Text -> Result Bool
readBoolWrapper msg = readWrapper msg readBool

readBool :: Text -> Maybe (Bool, Text)
readBool aText | T.isPrefixOf "True" aText  = boolReader "True" True aText
               | T.isPrefixOf "False" aText = boolReader "False" False aText
               | T.isPrefixOf "true" aText  = boolReader "true" True aText
               | T.isPrefixOf "false" aText = boolReader "false" False aText
               | T.isPrefixOf "T" aText     = boolReader "T" True aText
               | T.isPrefixOf "F" aText     = boolReader "F" False aText
               | T.isPrefixOf "t" aText     = boolReader "t" True aText
               | T.isPrefixOf "f" aText     = boolReader "f" False aText
               | T.isPrefixOf "1" aText     = boolReader "1" True aText
               | T.isPrefixOf "0" aText     = boolReader "0" False aText
               | otherwise                  = Nothing
  where boolReader key tf body = Just (tf, fromJust . T.stripPrefix key $ body)


readQuoted aText =
  findPattern "\"" "[Fail] Reading Opening of Name fails" ((), aText)
    >>= readName
    >>= findPattern "\"" "[Fail] Reading Closing of Name fails"
readName :: (a, Text) -> Result Text
readName (a, aText) = if T.null pRest
  then Left ("[Fail] Reading Name Body fails", aText)
  else Right (pBody, pRest)
  where (pBody, pRest) = T.breakOn "\"" aText

readQuotedNKey aText =
  findPattern "\"" "[Fail] Reading Opening of Name fails" ((), aText)
    >>= readNKey
    >>= findPattern "\"" "[Fail] Reading Closing of Name fails"
readNKey :: (a, Text) -> Result NKey
readNKey (a, aText) = if T.null pRest
  then Left ("[Fail] Reading Name Body fails", aText)
  else Right (T.toStrict pBody, pRest)
  where (pBody, pRest) = T.breakOn "\"" aText


readIntListWrapper :: Message -> Text -> Result [Int]
readIntListWrapper msg = readWrapper msg readIntList

readIntList :: Text -> Maybe ([Int], Text)
readIntList aText = maybe (mRest >>= readIntListSub [] . T.append ",")
                          (\t -> Just ([], t))
                          mNull
 where
  mNull = T.stripPrefix "[]" aText
  mRest = T.stripPrefix "[" aText

readIntListSub :: [Int] -> Text -> Maybe ([Int], Text)
readIntListSub acc aText | isJust shownEnd = Just (sort acc, aEndRest)
                         | isJust shownInt = readIntListSub (int : acc) aIntRest
                         | otherwise       = Nothing
 where
  shownInt             = T.stripPrefix "," aText >>= readInt
  Just (int, aIntRest) = shownInt
  shownEnd             = T.stripPrefix "]" aText
  Just aEndRest        = shownEnd

eIntListReader :: Text -> Result [Int]
eIntListReader aText = maybe
  (maybe (Left ("[Fail] Reading IL Opener", aText))
         (eIntListReaderSub [] . T.append ",")
         mRest
  )
  (\t -> Right ([], t))
  mNull
 where
  mNull = T.stripPrefix "[]" aText
  mRest = T.stripPrefix "[" aText
eIntListReaderSub :: [Int] -> Text -> Result [Int]
eIntListReaderSub acc aText = maybe
  (   readILDelimiter (undefined, aText)
  >>= readInt'
  >>= (\(i, aRest) -> eIntListReaderSub (i : acc) aRest)
  )
  (\t -> Right (sort acc, t))
  shownEnd
 where
  readILDelimiter = findPattern "," "[Fail] Reading IL delimiter fails"
  readInt' = readAnyway (readIntWrapper "[Fail] Reading Int in List fails")
  shownEnd = T.stripPrefix "]" aText
