{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Wai.Middleware.PrecompressedSpec where

import Data.Function ((&))
import Network.HTTP.Types
import Network.Wai.Middleware.Precompressed
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "alterHeader" $
    let headers = [("Content-Length", "17")] :: [Header]
     in do
          it "can insert" $
            ( alterHeader "Content-Type" (const $ Just "application/json") headers
                -- look ma!  lower case!
                & lookup "content-type"
            )
              `shouldBe` Just "application/json"
          it "can update" $
            alterHeader "Content-Length" (const $ Just "666") headers
              `shouldBe` [("Content-Length", "666")]
          it "can delete" $
            alterHeader "Content-Length" (const Nothing) headers
              `shouldBe` []
          it "can modify existing headers (via appendToCommaSeparatedHeader)" $ do
            appendToCommaSeparatedHeader "Vary" "Accept-Encoding" []
              `shouldBe` [("Vary", "Accept-Encoding")]
            appendToCommaSeparatedHeader "Vary" "Accept-Encoding" [("Vary", "Accept-Language")]
              `shouldBe` [("Vary", "Accept-Language, Accept-Encoding")]
  describe "Encoding" $ do
    prop "roundtrips via show/readEncoding" $ do
      \x -> (readEncoding . show) x `shouldBe` (Just x :: Maybe Encoding)
    prop "roundtrips via show/read" $ do
      \x -> (read . show) x `shouldBe` (x :: Encoding)

instance Arbitrary Encoding where
  arbitrary = oneof [return Brotli, return Gzip]

main :: IO ()
main = hspec spec
