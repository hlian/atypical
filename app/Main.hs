{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified DB
import OurPrelude
import Network.Wai.Handler.Warp (run)

type Interface =
  GET Hello
  :<|> POST Hello OK

get :: DB.DB -> Server (GET Hello)
get db = pure (Hello "world")

post :: DB.DB -> Server (POST Hello OK)
post db (Hello _) = pure OK

main :: IO ()
main = do
  db <- DB.init
  run 8000 (serve (Proxy :: Proxy Interface) (get db :<|> post db))
