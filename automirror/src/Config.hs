{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Config where
import Network.Curl.Opts
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.Char

import Control.Monad
import Control.Applicative

data Config = Config
  { cfgRootPath :: String
  , cfgAvailableMirrors :: [Mirror]
  , cfgCurlOpts :: [CurlOption]
  }
  deriving (Show,Generic)

data Mirror = Mirror
  { mirrorName :: String
  , mirrorSource :: String
  }
  deriving (Eq,Show,Generic)


instance FromJSON Mirror where
  parseJSON = genericParseJSON opts 
    where
      opts = defaultOptions { fieldLabelModifier = (map toLower . drop 6) }

loadMirrorFile :: String -> IO (Either String [Mirror])
loadMirrorFile cfile = 
  eitherDecode <$> (B.readFile cfile)
