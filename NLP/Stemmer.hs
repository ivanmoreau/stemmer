-- | Haskell bindings for the Snowball stemming library
-- A 'pure' stemming interface. Strings should use UTF-8 encoding. 
module NLP.Stemmer ( 
    -- * Stemming algorithms
      Stemmer(..)
    -- * Stemming functions
    , stem
    , stemText
    , stemByteString
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import           NLP.Stemmer.C (Stemmer)
import qualified NLP.Stemmer.C as C
import           System.IO.Unsafe (unsafePerformIO)

-- | Stem a word
{-# NOINLINE stem #-}
stem :: Stemmer -> String -> String
stem algorithm input = unsafePerformIO $ C.stem algorithm input

-- | Stem a word
{-# NOINLINE stemText #-}
stemText :: Stemmer -> Text -> Text
stemText algorithm input = unsafePerformIO $ C.stemText algorithm input

-- | Stem a word
{-# NOINLINE stemByteString #-}
stemByteString :: Stemmer -> ByteString -> ByteString
stemByteString algorithm input = unsafePerformIO $ C.stemByteString algorithm input
