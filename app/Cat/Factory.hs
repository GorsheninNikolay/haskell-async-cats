{-# LANGUAGE ScopedTypeVariables #-}

module Cat.Factory (
    collectUniqueCats,
    CollectOptions (..),
    defaultCollectOptions,
) where

import Control.Concurrent.Async (Async, async, cancel, waitAnyCatch)
import Control.Exception (SomeException, displayException)
import Data.List (delete)
import Data.Set (Set)
import qualified Data.Set as Set

import Cat.Types (CatHash, CatImage (catHash))

data CollectOptions = CollectOptions
    { concurrency :: Int
    , logMsg :: String -> IO ()
    , scaleWorkers :: Bool
    }

defaultCollectOptions :: CollectOptions
defaultCollectOptions = CollectOptions{concurrency = 12, logMsg = putStrLn, scaleWorkers = True}

collectUniqueCats :: CollectOptions -> Int -> IO CatImage -> IO [CatImage]
collectUniqueCats opts target fetchOne
    | target <= 0 = pure []
    | concurrency opts <= 0 = error "CollectOptions.concurrency must be > 0"
    | otherwise = do
        let logFn = logMsg opts
        logFn ("[factory] start target=" ++ show target ++ " concurrency=" ++ show (concurrency opts))
        go logFn 1 Set.empty [] []
  where
    go ::
        (String -> IO ()) ->
        Int ->
        Set CatHash ->
        [Async (Int, CatImage)] ->
        [CatImage] ->
        IO [CatImage]
    go logFn nextId seen inFlight uniques = do
        let uniqueCount = length uniques
        if uniqueCount >= target
            then do
                logFn ("[factory] successfully collected all cats: " ++ show target ++ "/" ++ show target)
                mapM_ cancel inFlight
                pure (reverse uniques)
            else do
                let leftToCollect = target - uniqueCount
                let desiredInFlight =
                        if scaleWorkers opts
                            then concurrency opts
                            else min (concurrency opts) leftToCollect
                let missing = desiredInFlight - length inFlight
                (nextId', inFlight') <- spawnMany logFn missing nextId inFlight

                (doneA, outcome) <- waitAnyCatch inFlight'
                let remaining = delete doneA inFlight'

                case outcome of
                    Left (e :: SomeException) -> do
                        logFn ("[factory] task failed: " ++ displayException e)
                        go logFn nextId' seen remaining uniques
                    Right (taskId, cat) -> do
                        let h = catHash cat
                        if Set.member h seen
                            then do
                                logFn ("[factory] taskId=" ++ show taskId ++ " completed -> duplicate hash=" ++ show h)
                                go logFn nextId' seen remaining uniques
                            else do
                                let seen' = Set.insert h seen
                                let ix = Set.size seen'
                                logFn ("[factory] taskId=" ++ show taskId ++ " completed -> unique (" ++ show ix ++ "/" ++ show target ++ ") hash=" ++ show h)
                                go logFn nextId' seen' remaining (cat : uniques)

    spawnMany ::
        (String -> IO ()) ->
        Int ->
        Int ->
        [Async (Int, CatImage)] ->
        IO (Int, [Async (Int, CatImage)])
    spawnMany logFn n startId acc
        | n <= 0 = pure (startId, acc)
        | otherwise = do
            let tid = startId
            logFn ("[factory] start fetch taskId=" ++ show tid)
            a <- async $ do
                cat <- fetchOne
                pure (tid, cat)
            spawnMany logFn (n - 1) (startId + 1) (a : acc)
