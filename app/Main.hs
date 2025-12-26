module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath ((</>))

import Cat.Api (fetchCatImage, mkCatService, newCatManager, postCatFileMultipart)
import Cat.Factory (collectUniqueCats, defaultCollectOptions)
import Cat.Types (CatImage (catBytes))
import Cat.Zip (catsToZip, defaultCatEntryName)

main :: IO ()
main = do
    let outDir = "static" </> "cats"
    resetDir outDir

    mgr <- newCatManager
    let svc = mkCatService "http://algisothal.ru:8889"
    cats <-
        collectUniqueCats
            defaultCollectOptions
            12
            (fetchCatImage mgr svc)

    putStrLn "All cats collected"

    putStrLn ("Saving cats to: " ++ outDir)
    mapM_ (\(i, c) -> BS.writeFile (outDir </> defaultCatEntryName i) (catBytes c)) (zip [1 ..] cats)

    let zipLazy = catsToZip cats
    let zipPath = outDir </> "cats.zip"
    LBS.writeFile zipPath zipLazy

    let zipBytes = LBS.toStrict zipLazy
    putStrLn ("Uploading zip, size=" ++ show (BS.length zipBytes) ++ " bytes")
    postCatFileMultipart mgr svc "cats.zip" zipBytes
    putStrLn "Upload OK"

resetDir :: FilePath -> IO ()
resetDir dir = do
    exists <- doesDirectoryExist dir
    if exists
        then removeDirectoryRecursive dir
        else pure ()
    createDirectoryIfMissing True dir
