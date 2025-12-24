module Cat.Types (
    CatImage (..),
    CatHash,
    mkCatImage,
    renderCatHashHex,
) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.ByteString as BS

type CatHash = Digest SHA256

data CatImage = CatImage
    { catBytes :: BS.ByteString
    , catHash :: CatHash
    }
    deriving (Eq, Ord, Show)

mkCatImage :: BS.ByteString -> CatImage
mkCatImage bs = CatImage{catBytes = bs, catHash = hash bs}

renderCatHashHex :: CatHash -> BS.ByteString
renderCatHashHex = convertToBase Base16
