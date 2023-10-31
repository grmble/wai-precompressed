{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Precompressed Brotli and Gzip resources with @wai-app-static@
-- Copyright   : Juergen Gmeiner
-- License     : MIT
-- Maintainer  : spamless.juergen@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Middleware (and convenience helper) that can serve precompressed
-- brotli and gzip files.  It works when serving files from disk
-- and when serving files embedded in the executable.
--
-- It can co-exist with the existing @gzip@ middleware,
-- and it can come before or after @gzip@.  But 'wai-precompressed'
-- must wrap 'wai-app-static'
module Network.Wai.Middleware.Precompressed where

import Control.Applicative (liftA2)
import Data.ByteString.Char8 qualified as B8
import Data.Data ()
import Data.Function ((&))
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text qualified as T
import Network.HTTP.Types (Header, HeaderName)
import Network.Wai
  ( Middleware,
    Request (..),
    mapResponseHeaders,
  )
import Network.Wai.Application.Static (StaticSettings)
import Network.Wai.Header (parseQValueList, replaceHeader)
import Text.ParserCombinators.ReadP qualified as RP
import Text.Read (Read (..), lift)
import WaiAppStatic.Types (LookupResult (..), StaticSettings (..), unsafeToPiece)

-- | Precompress middleware
--
-- It uses the @ssLookupFile@ and @ssGetMimeType@ actions from 'StaticSettings'
-- are used to determine if a precompressed @brotli@ or @gzip@ file exists.
--
-- Brotli is given a little (1/1000) increase in q value, so it is preferred
-- if the client gives equal (or default) weights.
precompress :: StaticSettings -> Middleware
precompress StaticSettings {ssLookupFile, ssGetMimeType} app req send =
  chooseEncoding encodings
  where
    chooseEncoding [] = app req send
    chooseEncoding (ext : rest) =
      haveProcompressed (pathInfo req) ext >>= \case
        Nothing -> chooseEncoding rest
        Just (pathinfo, origMimetype) ->
          app req {pathInfo = pathinfo} (fixHeaders ext origMimetype)

    encodings =
      lookup "Accept-Encoding" (requestHeaders req)
        & fromMaybe ""
        & parseQValueList
        >>= ( \(enc, q) ->
                let q2 = fromMaybe 1000 q
                 in case readEncoding (B8.unpack enc) of
                      Just ext -> [(ext, if enc == "br" then q2 + 1 else q2)]
                      Nothing -> []
            )
        & sortOn (Down . snd)
        & map fst

    fixHeaders ext origMimeetype resp =
      send $ flip mapResponseHeaders resp $ \headers ->
        replaceHeader "Content-Type" origMimeetype headers
          & replaceHeader "Content-Encoding" (B8.pack $ show ext)
          & alterHeader
            "Vary"
            ( \case
                Nothing -> Just "Accept-Encoding"
                Just x -> Just (x <> ", Accept-Encoding")
            )

    haveProcompressed pathinfo enc =
      let pie = appendExtension pathinfo (extension enc)
       in (liftA2 . liftA2)
            (\mt _ -> (pie, mt))
            (pieceExists (unsafeToPiece <$> pathInfo req))
            (pieceExists (unsafeToPiece <$> pie))

    pieceExists pieces =
      ssLookupFile pieces >>= \case
        LRFile f ->
          Just <$> ssGetMimeType f
        _ -> pure Nothing

    appendExtension [txt] ext = [T.pack (T.unpack txt ++ ext)]
    appendExtension (h : r) ext = h : appendExtension r ext
    appendExtension [] _ = []

-- | Alter a header
--
-- Analogue to misc alter functions - it takes and returns a maybe.
-- When Nothing is returned by 'f', an existing header is removed.
alterHeader :: HeaderName -> (Maybe B8.ByteString -> Maybe B8.ByteString) -> [Header] -> [Header]
alterHeader name f headers =
  let old = lookup name headers
      new = f old
      filtered = filter ((/= name) . fst) headers
   in case new of
        Nothing -> filtered
        Just value -> (name, value) : filtered

-- | Append a value to a comma separated header
appendToCommaSeparatedHeader :: HeaderName -> B8.ByteString -> [Header] -> [Header]
appendToCommaSeparatedHeader name value =
  alterHeader
    name
    ( \case
        Nothing -> Just value
        Just old -> Just (old <> ", " <> value)
    )

data Encoding = Brotli | Gzip deriving (Eq, Ord)

extension :: Encoding -> String
extension Brotli = ".br"
extension Gzip = ".gz"

instance Show Encoding where
  show Brotli = "br"
  show Gzip = "gzip"

instance Read Encoding where
  readPrec = lift encodingParser

encodingParser :: RP.ReadP Encoding
encodingParser = (Gzip <$ RP.string "gzip") RP.+++ (Brotli <$ RP.string "br")

readEncoding :: String -> Maybe Encoding
readEncoding s =
  case RP.readP_to_S encodingParser s of
    [(e, "")] -> Just e
    _ -> Nothing
