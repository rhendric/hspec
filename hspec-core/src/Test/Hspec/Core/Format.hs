{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Core.Format (
  Format(..)
, Progress
, Path
, Location(..)
, Seconds(..)
, Item(..)
, Result(..)
, FailureReason(..)

, printSlowSpecItems
) where

import           Test.Hspec.Core.Compat
import           Test.Hspec.Core.Spec (Progress, Location(..))
import           Test.Hspec.Core.Example (FailureReason(..))
import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Clock

import           Control.Monad.IO.Class
import           Text.Printf
import           Data.List

data Item = Item {
  itemLocation :: Maybe Location
, itemDuration :: Seconds
, itemInfo :: String
, itemResult :: Result
} deriving Show

data Result =
    Success
  | Pending (Maybe Location) (Maybe String)
  | Failure (Maybe Location) FailureReason
  deriving Show

data Format m = Format {
  formatRun :: forall a. m a -> IO a
, formatGroupStarted :: Path -> m ()
, formatGroupDone :: Path -> m ()
, formatProgress :: Path -> Progress -> m ()
, formatItemStarted :: Path -> m ()
, formatItemDone :: Path -> Item -> m ()
}

printSlowSpecItems :: MonadIO m => Int -> Format m -> IO (Format m)
printSlowSpecItems n format@Format{..} = do
  slow <- newIORef []
  return format {
      formatRun = \ action -> do
        r <- formatRun action

        putStrLn "\nSlow spec items:"

        readIORef slow >>= mapM_ printItem . take n . reverse . sortOn (itemDuration . snd)
        return r
    , formatItemDone = \ path item -> do
        liftIO $ modifyIORef slow ((path, item) :)
        formatItemDone path item
    }
  where
    printItem :: (Path, Item) -> IO ()
    printItem (path, Item{..}) = putStrLn foo
      where
        -- FIXME: Need item location, not assertion location!
        foo = (printf "  %1.3fs " itemDuration) ++ maybe "" formatLoc itemLocation ++ joinPath path
    formatLoc (Location file line column) = file ++ ":" ++ show line ++ ":" ++ show column ++ ": "
