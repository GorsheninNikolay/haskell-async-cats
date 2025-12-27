module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import Network.HTTP.Client (Manager)

import Cat.Api (fetchCatImage, mkCatService, newCatManager, postCatFileMultipart)
import Cat.Factory (collectUniqueCats, defaultCollectOptions)
import Cat.Types (CatImage (catBytes))
import Cat.Zip (catsToZip, defaultCatEntryName)

import Control.Concurrent.MSem (MSem)
import qualified Control.Concurrent.MSem as MSem
import Control.Concurrent.Async (async)

processCollage :: Int -> Manager -> Cat.Api.CatService -> FilePath -> IO ()
processCollage collageId mgr svc baseDir = do
    let outDir = baseDir </> ("collage-" ++ show collageId)
    resetDir outDir

    cats <- collectUniqueCats
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
    postCatFileMultipart mgr svc ("collage-" ++ show collageId ++ ".zip") zipBytes
    putStrLn "Upload OK"

main :: IO ()
main = do
    let baseDir = "static" </> "cats"
    let maxCollageWorkers = 3
    resetDir baseDir

    mgr <- newCatManager
    let svc = mkCatService "http://algisothal.ru:8889"
    sem <- MSem.new maxCollageWorkers
    
    let loop collageId = do
        MSem.with sem $ do
            putStrLn $ "Starting worker #" ++ show collageId
            _ <- async $ do
                processCollage collageId mgr svc baseDir
                putStrLn $ "Worker #" ++ show collageId ++ " finished."
            pure () 
        loop (collageId + 1)
    
    loop 1

resetDir :: FilePath -> IO ()
resetDir dir = do
    exists <- doesDirectoryExist dir
    if exists
        then removeDirectoryRecursive dir
        else pure ()
    createDirectoryIfMissing True dir
