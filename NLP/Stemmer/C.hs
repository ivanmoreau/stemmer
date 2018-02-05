{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Haskell bindings for the Snowball stemming library.
-- This module contains all the low-level functions and are more or less direct
-- translations of the foreign function calls.  The 'stem' function expects
-- strings to use UTF-8 encoding.
module NLP.Stemmer.C (
    -- * Types
      Stemmer(..)
    -- * Low level functions
    , stem
    , stemText
    , stemByteString
    ) where

import Control.Exception (bracket)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Char8 as BC

import Data.Text (Text)
import qualified Data.Text.Encoding as T

import Foreign.C        (CString(..), CStringLen(..), CInt(..), peekCStringLen, withCStringLen)
import Foreign.Ptr      (Ptr)

data StemmerStruct

-- | Pointer to a stemmer instance
type StemmerP  = Ptr StemmerStruct

-- | Available algorithms.  For English, 'English' is recommended over 'Porter'.
data Stemmer = Danish
             | Dutch
             | English
             | Finnish
             | French
             | German
             | Hungarian
             | Italian
             | Norwegian
             | Portuguese
             | Romanian
             | Russian
             | Spanish
             | Swedish
             | Turkish
             | Porter
             deriving Show

foreign import ccall "libstemmer.h sb_stemmer_new"     sb_stemmer_new    :: CString -> CString -> IO StemmerP
foreign import ccall "libstemmer.h sb_stemmer_delete"  sb_stemmer_delete :: StemmerP -> IO ()
foreign import ccall "libstemmer.h sb_stemmer_stem"    sb_stemmer_stem   :: StemmerP -> CString -> CInt -> IO (CString)
foreign import ccall "libstemmer.h sb_stemmer_length"  sb_stemmer_length :: StemmerP -> IO CInt

-- | Stem a word
stemPtr :: Stemmer -> CStringLen -> (CStringLen -> IO a) -> IO a
stemPtr algorithm (word, len) io = BS.unsafeUseAsCString (stemmerString algorithm) 
    $ \ algorithm' -> BS.unsafeUseAsCString utf8 
    $ \ encoding -> bracket (sb_stemmer_new algorithm' encoding) sb_stemmer_delete
    $ \ stemmer -> do
        strPtr <- sb_stemmer_stem stemmer word (fromIntegral len)
        strLen <- sb_stemmer_length stemmer
        io (strPtr, fromIntegral strLen)

stem :: Stemmer -> String -> IO String
stem algorithm word = withCStringLen word
    $ \ cWord -> stemPtr algorithm cWord peekCStringLen

stemText :: Stemmer -> Text -> IO Text
stemText algorithm word = do
    bs <- stemByteString algorithm $ T.encodeUtf8 word
    pure $ T.decodeUtf8 bs

stemByteString :: Stemmer -> ByteString -> IO ByteString
stemByteString algorithm word = BS.unsafeUseAsCString word
    $ \ cWord -> stemPtr algorithm (cWord, BS.length word) BS.packCStringLen

utf8 :: ByteString
utf8 = BC.pack "UTF_8"

stemmerString :: Stemmer -> ByteString
stemmerString s = case s of
    Danish     -> BC.pack "danish"
    Dutch      -> BC.pack "dutch"
    English    -> BC.pack "english"
    Finnish    -> BC.pack "finnish"
    French     -> BC.pack "french"
    German     -> BC.pack "german"
    Hungarian  -> BC.pack "hungarian"
    Italian    -> BC.pack "italian"
    Norwegian  -> BC.pack "norwegian"
    Portuguese -> BC.pack "portuguese"
    Romanian   -> BC.pack "romanian"
    Russian    -> BC.pack "russian"
    Spanish    -> BC.pack "spanish"
    Swedish    -> BC.pack "swedish"
    Turkish    -> BC.pack "turkish"
    Porter     -> BC.pack "porter"
