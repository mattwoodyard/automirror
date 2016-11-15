module Main where

import Lib
import Network.Wai.Handler.Warp (run)


testConfig = Config
  { cfgRootPath = "/tmp"
  , cfgAvailableMirrors =
    [ Mirror { mirrorName = "debian", mirrorSource = "http://mirror.cc.columbia.edu/debian/"}
    ]
  , cfgCurlOpts = []
  }


main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port $ handle testConfig
