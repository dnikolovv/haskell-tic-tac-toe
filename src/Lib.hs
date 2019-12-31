module Lib where

import Api
import App
import ClassyPrelude hiding (Handler, try)
import Control.Monad.Catch (try)
import Control.Monad.Except (ExceptT(..))
import Crypto.JOSE (JWK)
import qualified Data.Map as M
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (cors, corsMethods, corsRequestHeaders, simpleCorsResourcePolicy, simpleMethods)
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings)

toHandler :: State -> AppM a -> Handler a
toHandler state app = Handler $ ExceptT $ try $ runReaderT (runAppM app) state

corsMiddleware :: Middleware
corsMiddleware =
  cors
    (const $
     Just
       simpleCorsResourcePolicy
         {corsRequestHeaders = ["authorization", "content-type"], corsMethods = "PUT" : simpleMethods})

mkApp :: State -> IO Application
mkApp state@(userStore, pendingInvitations, games, userSubscriptions, jwtCfg) = do
  let cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy Api
      context = Proxy :: Proxy '[ CookieSettings, JWTSettings]
  pure $ corsMiddleware $ serveWithContext api cfg $ hoistServerWithContext api context (toHandler state) server

runApp :: Int -> State -> IO ()
runApp port state =
  withStdoutLogger $ \logger -> do
    let settings =
          setPort port $
          setBeforeMainLoop (print ("Listening on port " <> tshow port <> "...")) $ setLogger logger defaultSettings
    app <- mkApp state
    runSettings settings app

withState :: JWK -> (State -> IO ()) -> IO ()
withState jwtSigningKey action = do
  userStore <- newTVarIO []
  userConnections <- newTVarIO M.empty
  pendingInvitations <- newTVarIO []
  games <- newTVarIO []
  let jwtSettings = defaultJWTSettings jwtSigningKey
  action (userStore, pendingInvitations, games, userConnections, jwtSettings)

defaultMain :: JWK -> Int -> IO ()
defaultMain jwtSigningKey port = withState jwtSigningKey (runApp port)