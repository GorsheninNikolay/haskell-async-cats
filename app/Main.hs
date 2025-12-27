module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath ((</>))

import Cat.Api (fetchCatImage, mkCatService, newCatManager, postCatFileMultipart)
import Cat.Factory (collectUniqueCats, defaultCollectOptions)
import Cat.Types (CatImage (catBytes))
import Cat.Zip (catsToZip, defaultCatEntryName)

import Control.Concurrent.MSem (MSem, new, with)
import Control.Concurrent.Async (async)

processCollage :: Int -> Manager -> CatService -> FilePath -> IO()
processCollage collageId mgr svc baseDir = do
    let outDir = baseDir </> ("collage-" ++ showcollageId)
    resetDir outDir

    cats <- collectUniqueCats
                defaultCollectOptions
                12
                (fetchCatImage mgr svc)
    mapM_ (\(i, c) -> BS.writeFile (outDir </> defaultCatEntryName i) (catBytes c)) (zip [1 ..] cats)
    putStrLn "All cats collected"

    putStrLn ("Saving cats to: " ++ outDir)
    mapM_ (\(i, c) -> BS.writeFile (outDir </> defaultCatEntryName i) (catBytes c)) (zip [1 ..] cats)

    let zipLazy = catsToZip cats
    let zipPath = outDir </> "cats.zip"
    LBS.writeFile zipPath zipLazy

    let zipBytes = LBS.toStrict zipLazy
    putStrLn ("Uploading zip, size=" ++ show (BS.length zipBytes) ++ " bytes")
    postCatFileMultipart mgr svc ("collage-" ++ show collageId ++ ".zip") zipBytes
    putStrLn "Upload OK"

main :: IO ()
main = do
    let baseDir = "static" </> "cats"
    let maxCollageWorkers = 3
    resetDir baseDir

    mgr <- newCatManager
    let svc = mkCatService "http://algisothal.ru:8889"
    sem <- maxCollageWorkers
    let loop collageId = do
        putStrLn $ "Starting collage generation #" ++ show collageId
        _ <- async $ with sem $ do
            processCollage collageId mgr svc baseDir


resetDir :: FilePath -> IO ()
resetDir dir = do
    exists <- doesDirectoryExist dir
    if exists
        then removeDirectoryRecursive dir
        else pure ()
    createDirectoryIfMissing True dir
