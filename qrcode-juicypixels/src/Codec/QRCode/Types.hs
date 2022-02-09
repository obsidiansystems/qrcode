{-# LANGUAGE ScopedTypeVariables #-}

module Codec.QRCode.Types where

import Control.Monad
import Control.Monad.ST ( ST, runST )
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Maybe
import Data.Typeable ( Typeable )
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Data.Word

type Pixel8 = Word8

data Image a = Image
    { -- | Width of the image in pixels

      imageWidth  :: {-# UNPACK #-} !Int
      -- | Height of the image in pixels.

    , imageHeight :: {-# UNPACK #-} !Int

      -- | Image pixel data. To extract pixels at a given position

      -- you should use the helper functions.

      --

      -- Internally pixel data is stored as consecutively packed

      -- lines from top to bottom, scanned from left to right

      -- within individual lines, from first to last color

      -- component within each pixel.

    , imageData   :: V.Vector Pixel8
    }
    deriving (Typeable)

data BmpHeader = BmpHeader
    { magicIdentifier :: !Word16
    , fileSize        :: !Word32 -- ^ in bytes

    , reserved1       :: !Word16
    , reserved2       :: !Word16
    , dataOffset      :: !Word32
    }

bitmapMagicIdentifier :: Word16
bitmapMagicIdentifier = 0x4D42

instance Binary BmpHeader where
    put hdr = do
        putWord16le $ magicIdentifier hdr
        putWord32le $ fileSize hdr
        putWord16le $ reserved1 hdr
        putWord16le $ reserved2 hdr
        putWord32le $ dataOffset hdr

    get = do
        ident <- getWord16le
        when (ident /= bitmapMagicIdentifier)
             (fail "Invalid Bitmap magic identifier")
        fsize <- getWord32le
        r1 <- getWord16le
        r2 <- getWord16le
        offset <- getWord32le
        return BmpHeader
            { magicIdentifier = ident
            , fileSize = fsize
            , reserved1 = r1
            , reserved2 = r2
            , dataOffset = offset
            }

-- | The type of color space declared in a Windows BMP file.

data ColorSpaceType = CalibratedRGB
                    | DeviceDependentRGB
                    | DeviceDependentCMYK
                    | SRGB
                    | WindowsColorSpace
                    | ProfileEmbedded
                    | ProfileLinked
                    | UnknownColorSpace Word32
                    deriving (Eq, Show)

-- | BITMAPxHEADER with compatibility up to V5. This header was first introduced

-- with Windows 2.0 as the BITMAPCOREHEADER, and was later extended in Windows

-- 3.1, Windows 95 and Windows 98. The original BITMAPCOREHEADER includes all

-- fields up to 'bitPerPixel'. The Windows 3.1 BITMAPINFOHEADER adds all the

-- fields up to 'importantColors'.

--

-- Some Windows 3.1 bitmaps with 16 or 32 bits per pixel might also have three

-- bitmasks following the BITMAPINFOHEADER. These bitmasks were later

-- incorporated into the bitmap header structure in the unreleased

-- BITMAPV2INFOHEADER. The (also unreleased) BITMAPV3INFOHEADER added another

-- bitmask for an alpha channel.

--

-- The later Windows 95 and Windows 98 extensions to the BITMAPINFOHEADER extend

-- the BITMAPV3INFOHEADER, adding support for color correction.

--

--  * BITMAPV4HEADER (Windows 95) may include a simple color profile in a

--      proprietary format. The fields in this color profile (which includes gamma

--      values) are not to be used unless the 'colorSpaceType' field is

--      'CalibratedRGB'.

--

--  * BITMAPV5HEADER (Windows 98) adds support for an ICC color profile. The

--      presence of an ICC color profile is indicated by setting the 'colorSpaceType'

--      field to 'ProfileEmbedded' or 'ProfileLinked'. If it is 'ProfileLinked' then

--      the profile data is actually a Windows-1252 encoded string containing the

--      fully qualified path to an ICC color profile.

data BmpV5Header = BmpV5Header
    { size              :: !Word32 -- Header size in bytes

    , width             :: !Int32
    , height            :: !Int32
    , planes            :: !Word16 -- Number of colour planes

    , bitPerPixel       :: !Word16
    , bitmapCompression :: !Word32
    , byteImageSize     :: !Word32
    , xResolution       :: !Int32  -- ^ Pixels per meter

    , yResolution       :: !Int32  -- ^ Pixels per meter

    , colorCount        :: !Word32 -- ^ Number of colors in the palette

    , importantColours  :: !Word32
    -- Fields added to the header in V2

    , redMask           :: !Word32 -- ^ Red bitfield mask, set to 0 if not used

    , greenMask         :: !Word32 -- ^ Green bitfield mask, set to 0 if not used

    , blueMask          :: !Word32 -- ^ Blue bitfield mask, set to 0 if not used

    -- Fields added to the header in V3

    , alphaMask         :: !Word32 -- ^ Alpha bitfield mask, set to 0 if not used

    -- Fields added to the header in V4

    , colorSpaceType    :: !ColorSpaceType
    , colorSpace        :: !B.ByteString -- ^ Windows color space, not decoded

    -- Fields added to the header in V5

    , iccIntent         :: !Word32
    , iccProfileData    :: !Word32
    , iccProfileSize    :: !Word32
    }
    deriving Show

-- | Size of the Windows BITMAPV4INFOHEADER color space information.

sizeofColorProfile :: Int
sizeofColorProfile = 48

-- | Sizes of basic BMP headers.

sizeofBmpHeader, sizeofBmpCoreHeader, sizeofBmpInfoHeader :: Word32
sizeofBmpHeader = 2 + 4 + 2 + 2 + 4
sizeofBmpCoreHeader = 12
sizeofBmpInfoHeader = 40

-- | Sizes of extended BMP headers.

sizeofBmpV2Header, sizeofBmpV3Header, sizeofBmpV4Header, sizeofBmpV5Header :: Word32
sizeofBmpV2Header = 52
sizeofBmpV3Header = 56
sizeofBmpV4Header = 108
sizeofBmpV5Header = 124

instance Binary ColorSpaceType where
    put CalibratedRGB         = putWord32le 0
    put DeviceDependentRGB    = putWord32le 1
    put DeviceDependentCMYK   = putWord32le 2
    put ProfileEmbedded       = putWord32le 0x4D424544
    put ProfileLinked         = putWord32le 0x4C494E4B
    put SRGB                  = putWord32le 0x73524742
    put WindowsColorSpace     = putWord32le 0x57696E20
    put (UnknownColorSpace x) = putWord32le x
    get = do
      w <- getWord32le
      return $ case w of
        0          -> CalibratedRGB
        1          -> DeviceDependentRGB
        2          -> DeviceDependentCMYK
        0x4D424544 -> ProfileEmbedded
        0x4C494E4B -> ProfileLinked
        0x73524742 -> SRGB
        0x57696E20 -> WindowsColorSpace
        _          -> UnknownColorSpace w

instance Binary BmpV5Header where
    put hdr = do
        putWord32le $ size hdr

        if (size hdr == sizeofBmpCoreHeader) then do
          putWord16le . fromIntegral $ width hdr
          putWord16le . fromIntegral $ height hdr
          putWord16le $ planes hdr
          putWord16le $ bitPerPixel hdr
        else do
          putInt32le $ width hdr
          putInt32le $ height hdr
          putWord16le $ planes hdr
          putWord16le $ bitPerPixel hdr

        when (size hdr > sizeofBmpCoreHeader) $ do
          putWord32le $ bitmapCompression hdr
          putWord32le $ byteImageSize hdr
          putInt32le $ xResolution hdr
          putInt32le $ yResolution hdr
          putWord32le $ colorCount hdr
          putWord32le $ importantColours hdr

        when (size hdr > sizeofBmpInfoHeader || bitmapCompression hdr == 3) $ do
          putWord32le $ redMask hdr
          putWord32le $ greenMask hdr
          putWord32le $ blueMask hdr

        when (size hdr > sizeofBmpV2Header) $
          putWord32le $ alphaMask hdr

        when (size hdr > sizeofBmpV3Header) $ do
          put $ colorSpaceType hdr
          putByteString $ colorSpace hdr

        when (size hdr > sizeofBmpV4Header) $ do
          put $ iccIntent hdr
          putWord32le $ iccProfileData hdr
          putWord32le $ iccProfileSize hdr
          putWord32le 0 -- reserved field


    get = do
      readSize <- getWord32le
      if readSize == sizeofBmpCoreHeader
        then getBitmapCoreHeader readSize
        else getBitmapInfoHeader readSize

      where
        getBitmapCoreHeader readSize = do
          readWidth <- getWord16le
          readHeight <- getWord16le
          readPlanes <- getWord16le
          readBitPerPixel <- getWord16le
          return BmpV5Header {
              size = readSize,
              width = fromIntegral readWidth,
              height = fromIntegral readHeight,
              planes = readPlanes,
              bitPerPixel = readBitPerPixel,
              bitmapCompression = 0,
              byteImageSize = 0,
              xResolution = 2835,
              yResolution = 2835,
              colorCount = 2 ^ readBitPerPixel,
              importantColours = 0,
              redMask = 0,
              greenMask = 0,
              blueMask = 0,
              alphaMask = 0,
              colorSpaceType = DeviceDependentRGB,
              colorSpace = B.empty,
              iccIntent = 0,
              iccProfileData = 0,
              iccProfileSize = 0
          }

        getBitmapInfoHeader readSize = do
          readWidth <- getInt32le
          readHeight <- getInt32le
          readPlanes <- getWord16le
          readBitPerPixel <- getWord16le
          readBitmapCompression <- getWord32le
          readByteImageSize <- getWord32le
          readXResolution <- getInt32le
          readYResolution <- getInt32le
          readColorCount <- getWord32le
          readImportantColours <- getWord32le

          (readRedMask, readGreenMask, readBlueMask) <-
            if readSize == sizeofBmpInfoHeader && readBitmapCompression /= 3
              then return (0, 0, 0)
              else do
                -- fields added to the header in V2, but sometimes present

                -- immediately after a plain BITMAPINFOHEADER

                innerReadRedMask <- getWord32le
                innerReadGreenMask <- getWord32le
                innerReadBlueMask <- getWord32le
                return (innerReadRedMask, innerReadGreenMask, innerReadBlueMask)

          -- field added in V3 (undocumented)

          readAlphaMask <- if readSize < sizeofBmpV3Header then return 0 else getWord32le

          (readColorSpaceType, readColorSpace) <-
            if readSize < sizeofBmpV4Header
              then return (DeviceDependentRGB, B.empty)
              else do
                -- fields added in V4 (Windows 95)

                csType <- get
                cs <- getByteString sizeofColorProfile
                return (csType, cs)

          (readIccIntent, readIccProfileData, readIccProfileSize) <-
            if readSize < sizeofBmpV5Header
              then return (0, 0, 0)
              else do
                -- fields added in V5 (Windows 98)

                innerIccIntent <- getWord32le
                innerIccProfileData <- getWord32le
                innerIccProfileSize <- getWord32le
                void getWord32le -- reserved field

                return (innerIccIntent, innerIccProfileData, innerIccProfileSize)

          return BmpV5Header {
              size = readSize,
              width = readWidth,
              height = readHeight,
              planes = readPlanes,
              bitPerPixel = readBitPerPixel,
              bitmapCompression = readBitmapCompression,
              byteImageSize = readByteImageSize,
              xResolution = readXResolution,
              yResolution = readYResolution,
              colorCount = readColorCount,
              importantColours = readImportantColours,
              redMask = readRedMask,
              greenMask = readGreenMask,
              blueMask = readBlueMask,
              alphaMask = readAlphaMask,
              colorSpaceType = readColorSpaceType,
              colorSpace = readColorSpace,
              iccIntent = readIccIntent,
              iccProfileData = readIccProfileData,
              iccProfileSize = readIccProfileSize
          }

encodeBitmap :: Image Pixel8 -> L.ByteString
encodeBitmap = encodeBitmapWithPaletteAndMetadata (defaultPalette (undefined :: pixel))

-- | Equivalent to 'encodeBitmap' but also store

-- the following metadatas:

--

--  * 'Codec.Picture.Metadata.DpiX'

--  * 'Codec.Picture.Metadata.DpiY'

--

-- encodeBitmapWithMetadata :: Metadatas -> Image Pixel8 -> L.ByteString
-- encodeBitmapWithMetadata metas =
--   encodeBitmapWithPaletteAndMetadata metas (defaultPalette (undefined :: Pixel8))

-- | Write a dynamic image in a .bmp image file if possible.

-- The same restriction as 'encodeDynamicBitmap' apply.

-- writeDynamicBitmap :: FilePath -> DynamicImage -> IO (Either String Bool)
-- writeDynamicBitmap path img = case encodeDynamicBitmap img of
--         Left err -> return $ Left err
--         Right b  -> L.writeFile path b >> return (Right True)

-- | Encode a dynamic image in BMP if possible, supported images are:

--

--   - 'ImageY8'

--

--   - 'ImageRGB8'

--

--   - 'ImageRGBA8'

--

-- encodeDynamicBitmap :: DynamicImage -> Either String L.ByteString
-- encodeDynamicBitmap (ImageRGB8 img) = Right $ encodeBitmap img
-- encodeDynamicBitmap (ImageRGBA8 img) = Right $ encodeBitmap img
-- encodeDynamicBitmap (ImageY8 img) = Right $ encodeBitmap img
-- encodeDynamicBitmap _ = Left "Unsupported image format for bitmap export"

-- extractDpiOfMetadata :: Metadatas -> (Word32, Word32)
-- extractDpiOfMetadata metas = (fetch Met.DpiX, fetch Met.DpiY) where
--   fetch k = maybe 0 (fromIntegral . Met.dotPerInchToDotsPerMeter) $ Met.lookup k metas

-- | Convert an image to a bytestring ready to be serialized.

-- encodeBitmapWithPalette :: BmpPalette -> Image Pixel8 -> L.ByteString
-- encodeBitmapWithPalette = encodeBitmapWithPaletteAndMetadata mempty

-- | Equivalent to 'encodeBitmapWithPalette' but also store

-- the following metadatas:

--

--  * 'Codec.Picture.Metadata.DpiX'

--  * 'Codec.Picture.Metadata.DpiY'

--

linePadding :: Int -> Int -> Int
linePadding bpp imgWidth = (4 - (bytesPerLine `mod` 4)) `mod` 4
  where bytesPerLine = (bpp * imgWidth + 7) `div` 8

-- mkBS :: ForeignPtr Word8 -> Int -> Int -> S.ByteString
mkBS = S.PS

blitVector :: V.Vector Word8 -> Int -> Int -> B.ByteString
blitVector vec atIndex = mkBS ptr (offset + atIndex)
  where (ptr, offset, _length) = V.unsafeToForeignPtr vec

stridePut :: M.STVector s Word8 -> Int -> Int -> ST s ()
{-# INLINE stridePut #-}
stridePut vec = inner
 where inner  _ 0 = return ()
       inner ix n = do
           (vec `M.unsafeWrite` ix) 0
           inner (ix + 1) (n - 1)

newtype BmpPalette = BmpPalette [(Word8, Word8, Word8, Word8)]

putPalette :: BmpPalette -> Put
putPalette (BmpPalette p) = mapM_ (\(r, g, b, a) -> put r >> put g >> put b >> put a) p

putICCProfile :: Maybe B.ByteString -> Put
putICCProfile Nothing = return ()
putICCProfile (Just bytes) = put bytes


bmpEncode (Image {imageWidth = w, imageHeight = h, imageData = arr}) =
  forM_ [h - 1, h - 2 .. 0] $ \l -> putVector $ runST $ encodeLine l
    where stride = fromIntegral $ linePadding 8 w
          putVector vec = putByteString $ blitVector vec 0 lineWidth
          lineWidth = w + stride

          encodeLine :: forall s. Int -> ST s (V.Vector Word8)
          encodeLine line = do
              buff <- M.new lineWidth

              let lineIdx = line * w
                  inner col | col >= w = return ()
                  inner col = do
                      let v = arr `V.unsafeIndex` (lineIdx + col)
                      (buff `M.unsafeWrite` col) v
                      inner (col + 1)

              inner 0

              stridePut buff w stride
              V.unsafeFreeze buff

defaultPalette _ = BmpPalette [(x,x,x, 255) | x <- [0 .. 255]]
hasAlpha _ = False
bitsPerPixel _ = 8

-- | Compute the size of the pixel data

sizeofPixelData :: Int -> Int -> Int -> Int
sizeofPixelData bpp lineWidth nLines = ((bpp * (abs lineWidth) + 31) `div` 32) * 4 * abs nLines

encodeBitmapWithPaletteAndMetadata :: 
  -- Metadatas -> 
    BmpPalette -> 
      Image Pixel8
                                   -> L.ByteString
encodeBitmapWithPaletteAndMetadata {-metas-} pal@(BmpPalette palette) img =
  runPut $ put hdr >> put info >> putPalette pal >> bmpEncode img
                   >> putICCProfile colorProfileData

    where imgWidth = fromIntegral $ imageWidth img
          imgHeight = fromIntegral $ imageHeight img
          (dpiX, dpiY) = (0, 0)-- extractDpiOfMetadata metas
          -- cs = Met.lookup Met.ColorSpace metas
          colorType = 
            -- case cs of
            --             Just Met.SRGB -> SRGB
            --             Just (Met.WindowsBitmapColorSpace _) -> CalibratedRGB
            --             Just (Met.ICCProfile _) -> ProfileEmbedded
                        -- Nothing -> 
                          DeviceDependentRGB

          colorSpaceInfo = 
            -- case cs of
            --                 Just (Met.WindowsBitmapColorSpace bytes) -> bytes
                            -- _ -> 
                              B.pack $ replicate sizeofColorProfile 0

          colorProfileData = 
            -- case cs of
            --                   Just (Met.ICCProfile bytes) -> Just bytes
                              -- _ -> 
                                Nothing

          headerSize | colorType == ProfileEmbedded                = sizeofBmpV5Header
                     | colorType == CalibratedRGB || hasAlpha img  = sizeofBmpV4Header
                     | otherwise                                   = sizeofBmpInfoHeader

          paletteSize = fromIntegral $ length palette
          bpp = bitsPerPixel (undefined :: pixel)

          profileSize = fromIntegral $ maybe 0 B.length colorProfileData
          imagePixelSize = fromIntegral $ sizeofPixelData bpp imgWidth imgHeight
          offsetToData = sizeofBmpHeader + headerSize + 4 * paletteSize
          offsetToICCProfile = offsetToData + imagePixelSize <$ colorProfileData
          sizeOfFile = sizeofBmpHeader + headerSize + 4 * paletteSize
                        + imagePixelSize + profileSize

          hdr = BmpHeader {
              magicIdentifier = bitmapMagicIdentifier,
              fileSize = sizeOfFile,
              reserved1 = 0,
              reserved2 = 0,
              dataOffset = offsetToData
          }

          info = BmpV5Header {
              size = headerSize,
              width = fromIntegral imgWidth,
              height = fromIntegral imgHeight,
              planes = 1,
              bitPerPixel = fromIntegral bpp,
              bitmapCompression = if hasAlpha img then 3 else 0,
              byteImageSize = imagePixelSize,
              xResolution = fromIntegral dpiX,
              yResolution = fromIntegral dpiY,
              colorCount = paletteSize,
              importantColours = 0,
              redMask   = if hasAlpha img then 0x00FF0000 else 0,
              greenMask = if hasAlpha img then 0x0000FF00 else 0,
              blueMask  = if hasAlpha img then 0x000000FF else 0,
              alphaMask = if hasAlpha img then 0xFF000000 else 0,
              colorSpaceType = colorType,
              colorSpace = colorSpaceInfo,
              iccIntent = 0,
              iccProfileData = fromMaybe 0 offsetToICCProfile,
              iccProfileSize = profileSize
          }