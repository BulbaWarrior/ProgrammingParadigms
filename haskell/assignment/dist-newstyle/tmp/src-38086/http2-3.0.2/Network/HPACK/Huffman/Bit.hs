module Network.HPACK.Huffman.Bit (
  -- * Bits
    B(..)
  , Bits
  , fromBits
  ) where

import Imports

-- | Data type for Bit.
data B = F -- ^ Zero
       | T -- ^ One
       deriving (Eq,Ord,Show)

-- | Bit stream.
type Bits = [B]

fromBit :: B -> Word8
fromBit F = 0
fromBit T = 1

-- | From 'Bits' of length 8 to 'Word8'.
--
-- >>> fromBits [T,F,T,F,T,F,T,F]
-- 170
-- >>> fromBits [F,T,F,T,F,T,F,T]
-- 85
fromBits :: Bits -> Word8
fromBits = foldl' (\x y -> x * 2 + y) 0 . map fromBit
