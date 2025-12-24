module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Cat.Api (fetchCatImage, mkCatService, newCatManager, postCatFileMultipart)
import Cat.Factory (collectUniqueCats, defaultCollectOptions)
import Cat.Zip (catsToZip)

main :: IO ()
main = do
    mgr <- newCatManager
    let svc = mkCatService "http://algisothal.ru:8889"
    cats <-
        collectUniqueCats
            defaultCollectOptions
            12
            (fetchCatImage mgr svc)

    putStrLn "All cats collected"

    let zipBytes = LBS.toStrict (catsToZip cats)
    putStrLn ("Uploading zip, size=" ++ show (BS.length zipBytes) ++ " bytes")
    postCatFileMultipart mgr svc "cats.zip" zipBytes
    putStrLn "Upload OK"
