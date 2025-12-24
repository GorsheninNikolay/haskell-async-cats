module Cat.Zip (
    catsToZip,
    defaultCatEntryName,
) where

import Codec.Archive.Zip (
    Archive,
    addEntryToArchive,
    emptyArchive,
    fromArchive,
    toEntry,
 )
import qualified Data.ByteString.Lazy as LBS

import Cat.Types (CatImage (catBytes))

defaultCatEntryName :: Int -> FilePath
defaultCatEntryName i =
    "cat-" <> pad2 i <> ".jpg"
  where
    pad2 n
        | n < 10 = '0' : show n
        | otherwise = show n

catsToZip :: [CatImage] -> LBS.ByteString
catsToZip cats = fromArchive archive
  where
    archive :: Archive
    archive =
        foldl
            (\a (i, cat) -> addEntryToArchive (toEntry (defaultCatEntryName i) 0 (LBS.fromStrict (catBytes cat))) a)
            emptyArchive
            (zip [1 ..] cats)
