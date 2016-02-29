{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified DB
import           DB (DB)
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Swagger (info, title, version, Swagger)
import qualified Data.Swagger as Swagger
import           Network.Wai.Handler.Warp (run)

import           Control.Lens
import           OurPrelude
import           Servant.Swagger

type Interface =
  GET [Keyed Hello]
  :<|> POST Hello (Keyed Hello)
  :<|> Capture "id" Int :> (
    GET (Maybe Hello)
    :<|> PUT Hello OK
    :<|> DELETE OK
  )

ls :: DB -> Server (GET [Keyed Hello])
ls db = do
  ss <- DB.ls db
  return [Keyed i (Hello s) | (i, s) <- ss]

post :: DB -> Server (POST Hello (Keyed Hello))
post db (Hello s) = do
  i <- DB.create s db
  return (Keyed i (Hello s))

get :: Int -> DB -> Server (GET (Maybe Hello))
get hid db = do
  sMaybe <- DB.read hid db
  return (Hello <$> sMaybe)

put :: Int -> DB -> Server (PUT Hello OK)
put hid db (Hello s) = do
  DB.update hid db s
  return OK

delete :: Int -> DB -> Server (DELETE OK)
delete hid db = do
  DB.delete hid db
  return OK

proxy :: Proxy Interface
proxy = Proxy

swag :: Swagger
swag =
  toSwagger proxy
    & info . title .~ "A Collection of Hellos, Friendly and Unfriendly"
    & info . version .~ "v1"

main :: IO ()
main = do
  db <- DB.init
  BL8.writeFile "swagger.json" (encode swag)
  run 8000 (serve proxy
            (ls db :<|> post db :<|> (\hid -> get hid db :<|> put hid db :<|> delete hid db)))
