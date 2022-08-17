{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : CmdLine.Util
   Description : Personal utility functions for working with CLIs
   Copyright   : (c) 2022  Tony Zorman
   License     : GPL3
   Maintainer  : Tony Zorman <soliditsallgood@mailbox.org>
   Stability   : experimental
   Portability : non-portable
-}
module CmdLine.Util
    ( wrapText
    ) where

import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as T

{- | Simple (and probably hilariously inefficient) function to wrap text
at @N@ columns.

NOTE: "Data.Text"s 'Data.Text.length' function is @O(n)@, which may or
      may not matter here.
-}
wrapText
  :: Text   -- ^ How to concatenate chunks, i.e. the separator
  -> Int    -- ^ Left alignment
  -> Int    -- ^ Max line length (wrap)
  -> [Text] -- ^ Text as chunks that have to stay together
  -> Text   -- ^ Text with line breaks
wrapText separator al wrapAt chunks
  | wrapAt == 0   = mconcat (intersperse separator chunks)
  | [c] <- chunks = processed $ go "" ""        al (T.chunksOf 1 c)
  | otherwise     = processed $ go "" separator al chunks
 where
  go :: Text    -- Already processed part of the text
     -> Text    -- Separator to put between chunks
     -> Int     -- Counter of the current line length
     -> [Text]  -- Text as chunks that have to stay together
     -> GoState
  go !done _   !acc []        = GoState acc done
  go !line sep !acc xs@(c:cs)
    | cLen      >= wrapAt = go goLine             sep (accum goAgain) cs
    | al + cLen >= wrapAt = go goLine             sep (accum goAgain) cs
    | combLen   >= wrapAt = go (align line)       sep al              xs
    | otherwise           = go (line <> c <> end) sep newLen          cs
   where
    goAgain :: GoState
      | length (T.words c) > 1 = go line " " acc (T.words c)
      | otherwise              = let w = (wrapAt - acc - 2)
                                  in go line "" acc [T.take w c <> "-", T.drop w c]
    goLine  :: Text = if sep `T.isSuffixOf` goProc then goProc else goProc <> sep
    goProc  :: Text = T.stripEnd $ processed goAgain
    cLen    :: Int  = T.length c
    combLen :: Int  = acc + cLen                  -- Length including the next word
    newLen  :: Int  = combLen + T.length end + 1  -- Take separator length into account

    -- Nicely left-align the text after a line-break.  We like pretty
    -- things.
    align :: Text -> Text
    align = (<> "\n" <> T.replicate al " ") . T.stripEnd

    end :: Text
    end = if null cs then "" else sep

data GoState = GoState { accum :: !Int, processed :: !Text }
