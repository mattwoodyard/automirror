module Main where

import Lib
import Config
import Network.Wai.Handler.Warp (run)
import Options.Applicative

testConfig = Config
  { cfgRootPath = "/tmp"
  , cfgAvailableMirrors =
    [ Mirror { mirrorName = "debian", mirrorSource = "http://mirror.cc.columbia.edu/debian/"}
    ]
  , cfgCurlOpts = []
  }




data Cli = Cli
  { port :: Int
  , rootDirectory :: String
  -- , curlOpts :: [(String, Maybe String)]
  -- , port :: Int
  , configFile :: String
  }

cli :: Parser Cli
cli = Cli 3000
  <$> strOption
      (long "root"
      <> metavar "ROOT_DIRECTORY"
      <> help "Root path for cached local copies of data")
  <*> strOption
      (long "config"
      <> metavar "CONFIG_FILE"
      <> help "Configuration file defining mirrors"
      )

serverMain :: Cli -> IO ()
serverMain options = do
  mlist <- loadMirrorFile (configFile options)
  case mlist of
    Left l ->
      putStrLn $ "Error Loading Config File: " ++ (configFile options) ++ " " ++ l
    Right mirrors -> do
      putStrLn $ "Listening on port " ++ show (port options)
      rt <- mkRuntime
      run (port options) $ handle rt $ testConfig {cfgAvailableMirrors = mirrors}


main :: IO ()
main = execParser opts >>= serverMain
  where
    opts = info (helper <*> cli)
      ( fullDesc
     <> progDesc "mirror some external code repository"
     <> header "mirror stuff" )
     
