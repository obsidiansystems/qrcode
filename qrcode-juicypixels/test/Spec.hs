import qualified Codec.Picture as CP
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector.Storable as V
import Test.QuickCheck

import qualified Codec.QRCode.Types as T

-- This data type represents the image type
data TestImage = TestImage Int Int (V.Vector T.Pixel8)
  deriving Show

instance Arbitrary TestImage where
  arbitrary = do
    width <- getPositive <$> arbitrary
    height <- getPositive <$> arbitrary
    list <- vector $ height * width
    pure $ TestImage width height $ V.fromList list

prop_encodeIsCorrect :: TestImage -> Bool
prop_encodeIsCorrect (TestImage w h v) = jpEncoding == myEncoding
  where
    jpImage :: CP.Image CP.Pixel8
    jpImage = CP.Image w h v

    jpEncoding = CP.encodeBitmap jpImage

    myImage :: T.Image T.Pixel8
    myImage = T.Image w h v

    myEncoding = T.encodeBitmap myImage

main :: IO ()
main = do
  quickCheck prop_encodeIsCorrect
  putStrLn "Okay"