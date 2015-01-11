module Practice_12 where

import qualified Data.ByteString as BS
import Data.Bits
import Data.List
import Data.List.Split
import Data.Word (Word8)
import qualified Huffman as H
import Morse
import SkewHeap


{-G12.1-}

serializeHuff :: [H.Bit] -> BS.ByteString
serializeHuff = BS.pack . ser
  where
    ser bits = if len == 8
      then ch8 : ser bits2
      else [ch8 `shiftL` (8 - len), fromInteger $ toInteger len]
       where
        (bits1, bits2) = splitAt 8 bits
        len = length bits1 
        ch8 = toWord8 0 bits1

    toWord8 n [] = n
    toWord8 n (H.L:xs) = toWord8 (n `shiftL` 1) xs
    toWord8 n (H.R:xs) = toWord8 ((n `shiftL` 1) .|. 1) xs

deserializeHuff :: BS.ByteString -> Maybe [H.Bit]
deserializeHuff = deser . BS.unpack
  where
    deser :: [Word8] -> Maybe [H.Bit]
    deser [x,l] = Just $ take (fromInteger $ toInteger l) (fromWord8 x)
    deser (x:xs) = case deser xs of
                    Nothing -> Nothing
                    Just res -> Just $ fromWord8 x ++ res
    deser _ = Nothing

    fromWord8 c = reverse [ if testBit c n then H.R else H.L | n <- [0..7] ]

compress :: String -> FilePath -> IO ()
compress xs file = do
    BS.writeFile (file ++ ".code") (H.serializeTree tree)
    BS.writeFile (file ++ ".huff") (serializeHuff huff)
  where
    tree = H.mkTree $ H.mkFTable xs
    huff = H.encode (H.mkCTable tree) xs

decompress :: FilePath -> IO (Maybe String)
decompress file = do
    rawTree <- BS.readFile (file ++ ".code")
    rawHuff <- BS.readFile (file ++ ".huff")
    let tree = H.deserializeTree rawTree
    return $ case deserializeHuff rawHuff of
      Nothing -> Nothing
      Just huff -> Just $ H.decode tree huff

{-G12.2-}

newtype Reversed a = Reversed a deriving (Eq, Show)

instance Ord a => Ord (Reversed a) where
  compare (Reversed x) (Reversed y) = compare y x

type RevHeap a = Heap (Reversed a)
