{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module UnitTests.SomeType
    (
      someTypeTests
    ) where

import Prelude.Compat

import Data.Aeson (decode, eitherDecode, fromEncoding, Value)
import Data.Aeson.Types (Parser, IResult (..), iparse)
import Data.ByteString.Builder (toLazyByteString)
import Data.Maybe (fromJust)
import Encoders
import Test.Tasty.HUnit ((@=?), Assertion)
import Types
import qualified Data.ByteString.Lazy.Char8 as L

-- data SomeType a = Nullary
--                 | Unary Int
--                 | Product String (Maybe Char) a
--                 | Record { testOne   :: Double
--                          , testTwo   :: Maybe Bool
--                          , testThree :: Maybe a
--                          }
--                 | List [a]
--   deriving (Eq, Show)

someTypeTests :: [Assertion]
someTypeTests =
  [ dec "{\"nullary\":[]}"                                                                 @=? thSomeTypeToJSONObjectWithSingleField Nullary
  , dec "{\"nullary\":[]}"                                                                 @=? gSomeTypeToJSONObjectWithSingleField Nullary
  , dec "{\"unary\":1}"                                                                    @=? thSomeTypeToJSONObjectWithSingleField (Unary 1)
  , dec "{\"unary\":1}"                                                                    @=? gSomeTypeToJSONObjectWithSingleField (Unary 1)
  , dec "{\"product\":[\"a\",\"b\",1]}"                                      @=? thSomeTypeToJSONObjectWithSingleField (Product "a" (Just 'b') 1)
  , dec "{\"product\":[\"a\",\"b\",1]}"                                      @=? gSomeTypeToJSONObjectWithSingleField (Product "a" (Just 'b') 1)
  , dec "{\"record\":{\"testone\":1,\"testtwo\":null,\"testthree\":null}}"                 @=? thSomeTypeToJSONObjectWithSingleField (Record 1 Nothing Nothing)
  , dec "{\"record\":{\"testone\":1,\"testtwo\":null,\"testthree\":null}}"                 @=? gSomeTypeToJSONObjectWithSingleField (Record 1 Nothing Nothing)
  , dec "{\"nullary\":[],\"_tag\":true}"                                                   @=? thSomeTypeToJSONObjectWithSingleFieldTagged Nullary
  , dec "{\"nullary\":[],\"_tag\":true}"                                                   @=? gSomeTypeToJSONObjectWithSingleFieldTagged Nullary
  , dec "{\"unary\":1,\"_tag\":true}"                                                      @=? thSomeTypeToJSONObjectWithSingleFieldTagged (Unary 1)
  , dec "{\"unary\":1,\"_tag\":true}"                                                      @=? gSomeTypeToJSONObjectWithSingleFieldTagged (Unary 1)
  , dec "{\"product\":[\"a\",\"b\",1],\"_tag\":true}"                                      @=? thSomeTypeToJSONObjectWithSingleFieldTagged (Product "a" (Just 'b') 1)
  , dec "{\"product\":[\"a\",\"b\",1],\"_tag\":true}"                                      @=? gSomeTypeToJSONObjectWithSingleFieldTagged (Product "a" (Just 'b') 1)
  , dec "{\"record\":{\"testone\":1,\"testtwo\":null,\"testthree\":null},\"_tag\":true}"   @=? thSomeTypeToJSONObjectWithSingleFieldTagged (Record 1 Nothing Nothing)
  , dec "{\"record\":{\"testone\":1,\"testtwo\":null,\"testthree\":null},\"_tag\":true}"   @=? gSomeTypeToJSONObjectWithSingleFieldTagged (Record 1 Nothing Nothing)

  , decE "{\"unary\":1}"                                                                   @=? enc (thSomeTypeToEncodingObjectWithSingleField (Unary 1))
  , decE "{\"unary\":1}"                                                                   @=? enc (gSomeTypeToEncodingObjectWithSingleField (Unary 1))
  , decE "{\"nullary\":[]}"                                                                @=? enc (thSomeTypeToEncodingObjectWithSingleField Nullary)
  , decE "{\"nullary\":[]}"                                                                @=? enc (gSomeTypeToEncodingObjectWithSingleField Nullary)
  , decE "{\"product\":[\"a\",\"b\",1]}"                                     @=? enc (thSomeTypeToEncodingObjectWithSingleField (Product "a" (Just 'b') 1))
  , decE "{\"product\":[\"a\",\"b\",1]}"                                     @=? enc (gSomeTypeToEncodingObjectWithSingleField (Product "a" (Just 'b') 1))
  , decE "{\"record\":{\"testone\":1,\"testtwo\":null,\"testthree\":null}}"                @=? enc (thSomeTypeToEncodingObjectWithSingleField (Record 1 Nothing Nothing))
  , decE "{\"record\":{\"testone\":1,\"testtwo\":null,\"testthree\":null}}"                @=? enc (gSomeTypeToEncodingObjectWithSingleField (Record 1 Nothing Nothing))
  , decE "{\"nullary\":[],\"_tag\":true}"                                                  @=? enc (thSomeTypeToEncodingObjectWithSingleFieldTagged Nullary)
  , decE "{\"nullary\":[],\"_tag\":true}"                                                  @=? enc (gSomeTypeToEncodingObjectWithSingleFieldTagged Nullary)
  , decE "{\"unary\":1,\"_tag\":true}"                                                     @=? enc (thSomeTypeToEncodingObjectWithSingleFieldTagged (Unary 1))
  , decE "{\"unary\":1,\"_tag\":true}"                                                     @=? enc (gSomeTypeToEncodingObjectWithSingleFieldTagged (Unary 1))
  , decE "{\"product\":[\"a\",\"b\",1],\"_tag\":true}"                                     @=? enc (thSomeTypeToEncodingObjectWithSingleFieldTagged (Product "a" (Just 'b') 1))
  , decE "{\"product\":[\"a\",\"b\",1],\"_tag\":true}"                                     @=? enc (gSomeTypeToEncodingObjectWithSingleFieldTagged (Product "a" (Just 'b') 1))
  , decE "{\"record\":{\"testone\":1,\"testtwo\":null,\"testthree\":null},\"_tag\":true}"  @=? enc (thSomeTypeToEncodingObjectWithSingleFieldTagged (Record 1 Nothing Nothing))
  , decE "{\"record\":{\"testone\":1,\"testtwo\":null,\"testthree\":null},\"_tag\":true}"  @=? enc (gSomeTypeToEncodingObjectWithSingleFieldTagged (Record 1 Nothing Nothing))

  , ISuccess Nullary @=? parse thSomeTypeParseJSONObjectWithSingleField                    (dec "{\"nullary\":[]}")
  , ISuccess Nullary @=? parse gSomeTypeParseJSONObjectWithSingleField                     (dec "{\"nullary\":[]}")
  , ISuccess (Unary 1) @=? parse thSomeTypeParseJSONObjectWithSingleField                  (dec "{\"unary\":1}")
  , ISuccess (Unary 1) @=? parse gSomeTypeParseJSONObjectWithSingleField                   (dec "{\"unary\":1}")
  , ISuccess (Product "a" (Just 'b') 1) @=? parse thSomeTypeParseJSONObjectWithSingleField (dec "{\"product\":[\"a\",\"b\",1]}")
  , ISuccess (Product "a" (Just 'b') 1) @=? parse gSomeTypeParseJSONObjectWithSingleField  (dec "{\"product\":[\"a\",\"b\",1]}")
  , ISuccess (Record 1 Nothing Nothing) @=? parse thSomeTypeParseJSONObjectWithSingleField (dec "{\"record\":{\"testone\":1,\"testtwo\":null,\"testthree\":null}}")
  , ISuccess (Record 1 Nothing Nothing) @=? parse gSomeTypeParseJSONObjectWithSingleField  (dec "{\"record\":{\"testone\":1,\"testtwo\":null,\"testthree\":null}}")

  , ISuccess Nullary @=? parse thSomeTypeParseJSONObjectWithSingleFieldTagged                    (dec "{\"nullary\":[],\"_tag\":true}")
  , ISuccess Nullary @=? parse gSomeTypeParseJSONObjectWithSingleFieldTagged                     (dec "{\"nullary\":[],\"_tag\":true}")
  , ISuccess Nullary @=? parse thSomeTypeParseJSONObjectWithSingleFieldTagged                    (dec "{\"nullary\":[]}")
  , ISuccess Nullary @=? parse gSomeTypeParseJSONObjectWithSingleFieldTagged                     (dec "{\"nullary\":[]}")
  , ISuccess (Unary 1) @=? parse thSomeTypeParseJSONObjectWithSingleFieldTagged                  (dec "{\"unary\":1,\"_tag\":true}")
  , ISuccess (Unary 1) @=? parse gSomeTypeParseJSONObjectWithSingleFieldTagged                   (dec "{\"unary\":1,\"_tag\":true}")
  , ISuccess (Unary 1) @=? parse thSomeTypeParseJSONObjectWithSingleFieldTagged                  (dec "{\"unary\":1}")
  , ISuccess (Unary 1) @=? parse gSomeTypeParseJSONObjectWithSingleFieldTagged                   (dec "{\"unary\":1}")
  , ISuccess (Product "a" (Just 'b') 1) @=? parse thSomeTypeParseJSONObjectWithSingleFieldTagged (dec "{\"product\":[\"a\",\"b\",1],\"_tag\":true}")
  , ISuccess (Product "a" (Just 'b') 1) @=? parse gSomeTypeParseJSONObjectWithSingleFieldTagged  (dec "{\"product\":[\"a\",\"b\",1],\"_tag\":true}")
  , ISuccess (Product "a" (Just 'b') 1) @=? parse thSomeTypeParseJSONObjectWithSingleFieldTagged (dec "{\"product\":[\"a\",\"b\",1]}")
  , ISuccess (Product "a" (Just 'b') 1) @=? parse gSomeTypeParseJSONObjectWithSingleFieldTagged  (dec "{\"product\":[\"a\",\"b\",1]}")
  , ISuccess (Record 1 Nothing Nothing) @=? parse thSomeTypeParseJSONObjectWithSingleFieldTagged (dec "{\"record\":{\"testone\":1,\"testtwo\":null,\"testthree\":null},\"_tag\":true}")
  , ISuccess (Record 1 Nothing Nothing) @=? parse gSomeTypeParseJSONObjectWithSingleFieldTagged  (dec "{\"record\":{\"testone\":1,\"testtwo\":null,\"testthree\":null},\"_tag\":true}")
  , ISuccess (Record 1 Nothing Nothing) @=? parse thSomeTypeParseJSONObjectWithSingleFieldTagged (dec "{\"record\":{\"testone\":1,\"testtwo\":null,\"testthree\":null}}")
  , ISuccess (Record 1 Nothing Nothing) @=? parse gSomeTypeParseJSONObjectWithSingleFieldTagged  (dec "{\"record\":{\"testone\":1,\"testtwo\":null,\"testthree\":null}}")
  ]
  where
    enc = eitherDecode . toLazyByteString . fromEncoding
    dec :: L.ByteString -> Value
    dec = fromJust . decode
    decE :: L.ByteString -> Either String Value
    decE = eitherDecode
    parse :: (a -> Parser b) -> a -> IResult b
    parse parsejson v = iparse parsejson v
