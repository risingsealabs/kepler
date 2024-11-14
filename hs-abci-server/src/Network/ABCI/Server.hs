module Network.ABCI.Server where

import           Data.Conduit            (runConduit, (.|))
import           Data.Conduit.Network    (AppData, ServerSettings, appSink,
                                          appSource, runTCPServer,
                                          serverSettings)
import           Data.String             (fromString)
import           Network.ABCI.Server.App (App (..), appConduit)


-- | Default ABCI app network settings for serving on localhost at the
-- standard port.
defaultLocalSettings :: ServerSettings
defaultLocalSettings = serverSettings 26658 $ fromString "0.0.0.0"

-- | Serve an ABCI application with custom 'ServerSettings' and a custom
-- action to perform on acquiring the socket resource.
serveAppWith
  :: ServerSettings
  -> (AppData -> IO ())
  -> App IO
  -> IO ()
serveAppWith cfg onAquire app = runTCPServer cfg $ \appData -> do
  onAquire appData
  runConduit $ appSource appData
    .| appConduit app
    .| appSink appData

-- | Serve an ABCI application with default local 'ServerSettings'
-- and a no-op on acquiring the socket resource.
serveApp :: App IO -> IO ()
serveApp = serveAppWith defaultLocalSettings mempty
