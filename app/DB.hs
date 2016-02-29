{-# LANGUAGE NoImplicitPrelude #-}

module DB (
  DB
  , init
  , create
  , update
  , delete
  , read
  , ls
  ) where

import BasePrelude hiding ((&), delete, read, init)
import Control.Lens
import Control.Monad.IO.Class

newtype DB = DB (MVar [String])

init :: MonadIO m => m DB
init = liftIO (DB <$> newMVar [])

create :: MonadIO m => String -> DB -> m Int
create s (DB mvar) =
  (liftIO . modifyMVar mvar) $ \ss ->
    return (s:ss, length ss)

update :: MonadIO m => Int -> DB -> String -> m ()
update sid (DB mvar) s =
  (liftIO . modifyMVar_ mvar) $ \ss ->
    return (ss & element sid .~ s)

delete :: MonadIO m => Int -> DB -> m ()
delete sid (DB mvar) =
  (liftIO . modifyMVar_ mvar) $ \ss ->
    return (ss ^.. folded . ifiltered (\i _ -> i /= sid))

read :: MonadIO m => Int -> DB -> m (Maybe String)
read sid (DB mvar) = liftIO $ do
  ss <- readMVar mvar
  return (ss ^? element sid)

ls :: MonadIO m => DB -> m [(Int, String)]
ls (DB mvar) = liftIO $ do
  ss <- readMVar mvar
  return (zip [0..] ss)
