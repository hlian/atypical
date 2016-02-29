{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module OurPrelude (
  module BasePrelude
  , HelloWorld(..)
  , Hello(..)
  , OK(..)
  , Capture
  , GET
  , POST
  , PUT
  , DELETE
  , Keyed(..)
  , (:<|>)(..)
  , (:>)
  , serve
  , Server
  , io
  ) where

import           BasePrelude hiding (insert, (&), delete)
import           Control.Lens hiding ((.=))
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Aeson.Types (camelTo)
import           Data.Swagger
import qualified Data.Swagger as Swagger
import           Data.Swagger.Internal.Schema
import           GHC.Exts (fromList)
import           Servant.API
import           Servant.Server

type GET = Get '[JSON]
type POST body output = ReqBody '[JSON] body :> Post '[JSON] output
type PUT body output = ReqBody '[JSON] body :> Put '[JSON] output
type DELETE = Delete '[JSON]

data HelloWorld =
  HelloWorld deriving (Generic)

data OK =
  OK deriving (Generic)

data Hello =
  Hello {helloThing :: String} deriving (Generic)

data Keyed a =
  Keyed Int a deriving (Generic)

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

io :: MonadIO m => IO a -> m a
io = liftIO

-- | see https://haskell-servant.github.io/posts/2016-02-06-servant-swagger.html
modifier :: String -> String
modifier = drop 1 . dropWhile (/= '_') . camelTo '_'

instance ToSchema OK

instance ToSchema Hello where
  declareNamedSchema = genericDeclareNamedSchema
    $ defaultSchemaOptions { Swagger.fieldLabelModifier = modifier }

instance ToSchema a => ToSchema (Keyed a) where
  declareNamedSchema _ = do
    (return . addKey . toNamedSchema) (Proxy :: Proxy a)
    where
      addKey =
        schema . properties <>~ fromList [("id", Inline (mempty & type_ .~ SwaggerNumber & description ?~ "DB id"))]

instance ToJSON a => ToJSON (Keyed a) where
  toJSON (Keyed i a) =
    toJSON a & _Object . at "id" ?~ toJSON i
