{-# LANGUAGE OverloadedStrings #-}

module Cat.Api (
    CatService (..),
    mkCatService,
    newCatManager,
    fetchCatImage,
    saveCatJpeg,
    postCatFileMultipart,
    getCatJpegBytes,
    postCatBytes,
    CatApiError (..),
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client (
    Manager,
    RequestBody (RequestBodyBS),
    httpLbs,
    method,
    newManager,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
    responseStatus,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hAccept, hContentType, statusCode)

import Cat.Types (CatImage (catBytes), mkCatImage)

data CatService = CatService
    { catBaseUrl :: String
    }
    deriving (Eq, Show)

mkCatService :: String -> CatService
mkCatService base = CatService{catBaseUrl = rstripSlash base}

newCatManager :: IO Manager
newCatManager = newManager tlsManagerSettings

data CatApiError
    = CatApiUnexpectedStatus
    { expected :: Int
    , actual :: Int
    }
    deriving (Eq, Show)

instance Exception CatApiError

catUrl :: CatService -> String
catUrl svc = catBaseUrl svc <> "/cat"

getCatJpegBytes :: Manager -> CatService -> IO BS.ByteString
getCatJpegBytes mgr svc = do
    req0 <- parseRequest (catUrl svc)
    let req =
            req0
                { method = "GET"
                , requestHeaders = [(hAccept, "image/jpeg")]
                }
    res <- httpLbs req mgr
    let code = statusCode (responseStatus res)
    unless (code == 200) $
        throwIO CatApiUnexpectedStatus{expected = 200, actual = code}
    pure (LBS.toStrict (responseBody res))

fetchCatImage :: Manager -> CatService -> IO CatImage
fetchCatImage mgr svc = mkCatImage <$> getCatJpegBytes mgr svc

saveCatJpeg :: Manager -> CatService -> CatImage -> IO ()
saveCatJpeg mgr svc img = postCatBytes mgr svc "image/jpeg" (catBytes img)

postCatBytes :: Manager -> CatService -> BS.ByteString -> BS.ByteString -> IO ()
postCatBytes mgr svc contentType body = do
    req0 <- parseRequest (catUrl svc)
    let req =
            req0
                { method = "POST"
                , requestHeaders = [(hContentType, contentType)]
                , requestBody = RequestBodyBS body
                }
    res <- httpLbs req mgr
    let code = statusCode (responseStatus res)
    unless (code == 200) $
        throwIO CatApiUnexpectedStatus{expected = 200, actual = code}

postCatFileMultipart :: Manager -> CatService -> FilePath -> BS.ByteString -> IO ()
postCatFileMultipart mgr svc filename body = do
    req0 <- parseRequest (catUrl svc)
    let boundary = "---------------------------haskell-async-cats-boundary-7b2c0f5c"
    let ct = "multipart/form-data; boundary=" <> boundary
    let req =
            req0
                { method = "POST"
                , requestHeaders = [(hContentType, ct)]
                , requestBody = RequestBodyBS (renderMultipart boundary filename body)
                }
    res <- httpLbs req mgr
    let code = statusCode (responseStatus res)
    unless (code == 200) $
        throwIO CatApiUnexpectedStatus{expected = 200, actual = code}

renderMultipart :: BS.ByteString -> FilePath -> BS.ByteString -> BS.ByteString
renderMultipart boundary filename fileBytes =
    BS.concat
        [ "--"
        , boundary
        , crlf
        , "Content-Disposition: form-data; name=\"file\"; filename=\""
        , filenameBs
        , "\""
        , crlf
        , "Content-Type: application/zip"
        , crlf
        , crlf
        , fileBytes
        , crlf
        , "--"
        , boundary
        , "--"
        , crlf
        ]
  where
    crlf = "\r\n"
    filenameBs = BS.pack (map (fromIntegral . fromEnum) filename)

rstripSlash :: String -> String
rstripSlash s =
    case reverse s of
        ('/' : rest) -> reverse rest
        _ -> s
