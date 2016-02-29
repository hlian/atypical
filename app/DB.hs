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

newtype DB = DB (MVar [String])

init :: IO DB
init = DB <$> newMVar []

create :: String -> DB -> IO ()
create s (DB mvar) =
  modifyMVar_ mvar $ \ss -> return (s:ss)

update :: Int -> String -> DB -> IO ()
update sid s (DB mvar) =
  modifyMVar_ mvar $ \ss -> return (ss & element sid .~ s)

delete :: Int -> DB -> IO ()
delete sid (DB mvar) =
  modifyMVar_ mvar $ \ss -> return (ss ^.. folded . ifiltered (\i _ -> i /= sid))

read :: Int -> DB -> IO String
read = undefined

ls :: DB -> IO [(Int, String)]
ls = undefined
