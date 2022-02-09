{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.QRCode.JuicyPixels
  ( -- * Image
    toImage
    -- * URL
  , toBmpDataUrlBS
  , toBmpDataUrlS
  , toBmpDataUrlT
  ) where

import           Data.Bool                   (bool)
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString.Lazy.Char8  as BLC8
import qualified Data.Text.Lazy              as TL
import qualified Data.Vector.Storable        as SV
import qualified Data.Vector.Unboxed         as UV
import           Data.Word                   (Word8)

import           Codec.QRCode                (QRImage (..))
import           Codec.QRCode.Types

-- | Convert the QR code into an image.
--
--   If this is not the required image format use `Codec.Picture.Types.promoteImage` and/or `Codec.Picture.Types.convertImage`.
toImage
  :: Int -- ^ Border to add around the QR code, recommended is 4 (<0 is treated as 0)
  -> Int -- ^ Factor to scale the image (<1 is treated as 1)
  -> QRImage -- ^ The QRImage
  -> Image Pixel8
toImage border scale QRImage{..}
  | border <= 0 && scale <= 1 =
    Image qrImageSize qrImageSize (SV.fromList $ map (bool 0xff 0x00) (UV.toList qrImageData))
toImage border' scale' QRImage{..} =
  let
    border = border' `max` 0
    scale = scale' `max` 1
    size = (qrImageSize + 2 * border) * scale
  in
    Image size size (SV.fromList $ concat $ doScale scale $ addBorder border $ toMatrix qrImageData)
  where
    toMatrix :: UV.Vector Bool -> [[Word8]]
    toMatrix img
      | UV.null img = []
      | otherwise =
        let
          (h, t) = UV.splitAt qrImageSize img
        in
          map (bool 0xff 0x00) (UV.toList h) : toMatrix t
    addBorder :: Int -> [[Word8]] -> [[Word8]]
    addBorder 0 img = img
    addBorder n img = topBottom ++ addLeftRight img ++ topBottom
      where
        topBottom = [replicate ((qrImageSize + 2 * n) * n) 0xff]
        leftRight = replicate n 0xff
        addLeftRight = map (\ x -> leftRight ++ x ++ leftRight)
    doScale :: Int -> [[Word8]] -> [[Word8]]
    doScale 1 img = img
    doScale n img = scaleV img
      where
        scaleV :: [[Word8]] -> [[Word8]]
        scaleV = concatMap (replicate n . scaleH)
        scaleH :: [Word8] -> [Word8]
        scaleH = concatMap (replicate n)

-- | Convert an QR code into a Uri.
--   Has the same arguments as `toImage`.
--
--   This can be used to display a image in HTML without creating a temporary file.
toBmpDataUrlBS :: Int -> Int -> QRImage -> BL.ByteString
toBmpDataUrlBS border scale img = "data:image/bmp;base64," `BL.append` B64L.encode (encodeBitmap $ toImage border scale img)

-- | Convert an QR code into a Uri.
--   Has the same arguments as `toImage`.
--
--   Like `toBmpDataUrlBS` but with a to String conversion afterwards.
toBmpDataUrlS :: Int -> Int -> QRImage -> String
{-# INLINE toBmpDataUrlS #-}
toBmpDataUrlS border scale = BLC8.unpack . toBmpDataUrlBS border scale

-- | Convert an QR code into a Uri.
--   Has the same arguments as `toImage`.
--
--   Like `toBmpDataUrlS` but with a to Text conversion afterwards.
toBmpDataUrlT :: Int -> Int -> QRImage -> TL.Text
{-# INLINE toBmpDataUrlT #-}
toBmpDataUrlT border scale = TL.pack . toBmpDataUrlS border scale
