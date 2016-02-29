{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module OurPrelude (
  module BasePrelude
  , HelloWorld(..)
  , Hello(..)
  , OK(..)
  , GET
  , POST
  , PUT
  , DELETE
  , (:<|>)(..)
  , serve
  , Server
  ) where

import           BasePrelude hiding (insert, (&), delete)
import           Data.Aeson
import           Data.Aeson.Types
import           Servant.API
import           Servant.Server

type GET = Get '[JSON]
type POST body output = ReqBody '[JSON] body :> Post '[JSON] output
type PUT body output = ReqBody '[JSON] body :> Put '[JSON] output
type DELETE = Delete '[JSON]

data HelloWorld = HelloWorld

data OK = OK

data Hello = Hello String

instance ToJSON HelloWorld where
  toJSON HelloWorld =
    object ["hello" .= ("world" :: String)]

instance ToJSON Hello where
  toJSON (Hello s) =
    object ["hello" .= s]

instance ToJSON OK where
  toJSON OK =
    object ["ok" .= True]

instance FromJSON HelloWorld where
  parseJSON (Object o) = do
    (x :: String) <- o .: "hello"
    if x == "world" then
      return HelloWorld
    else
      fail "found a hello, but no world"
  parseJSON invalid =
    typeMismatch "HelloWorld" invalid

instance FromJSON Hello where
  parseJSON (Object o) = do
    x <- o .: "hello"
    return (Hello x)
  parseJSON invalid =
    typeMismatch "Hello" invalid
