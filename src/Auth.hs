module Auth where

import ClassyPrelude
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Crypto.JWT (ClaimsSet, JWK, JWTError, decodeCompact, defaultJWTValidationSettings, verifyClaims)
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.ByteString.Lazy as L
import Data.Either (fromRight)
import Data.Has (Has, getter)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, addUTCTime)
import Data.UUID (UUID)
import Error (AppError(..))
import Json (genericParseWithPrefix, genericToJSONNoPrefix)
import Network.HTTP.Types
import Network.WebSockets
  ( Connection
  , PendingConnection
  , acceptHeaders
  , acceptRequest
  , defaultRejectRequest
  , pendingRequest
  , rejectCode
  , rejectMessage
  , rejectRequestWith
  , requestHeaders
  , requestPath
  )
import Servant.Auth.Server (FromJWT(..), JWTSettings, ToJWT(..), makeJWT, signingKey)
import Views (TokenView(..))
import Common (checkEither)

data AuthenticatedUser =
  AuthenticatedUser
    { aUserId :: UUID
    , aUserName :: Text
    }
  deriving (Show, Generic)

instance ToJSON AuthenticatedUser where
  toJSON = genericToJSONNoPrefix "aUser"

instance FromJSON AuthenticatedUser where
  parseJSON = genericParseWithPrefix "aUser"

instance ToJWT AuthenticatedUser

instance FromJWT AuthenticatedUser

withAuthenticatedWsConnection ::
     (Has JWTSettings r, MonadReader r m, MonadIO m, MonadThrow m)
  => PendingConnection
  -> (AuthenticatedUser -> Connection -> m ())
  -> m Connection
withAuthenticatedWsConnection pendingConnection action = do
  let tokenParamName = "access_token"
      tokenQueryParam =
        find (\(pName, _) -> pName == tokenParamName) . snd . decodePath $
        (requestPath . pendingRequest $ pendingConnection)
      tokenValue = tokenQueryParam >>= \(_, token) -> fromStrict <$> token
  case tokenValue of
    Just token -> do
      jwtSecret <- signingKey <$> asks getter
      authResult <- liftIO $ verifyJwt jwtSecret token
      case authResult of
        Right claims -> do
          let decoded = decodeJWT claims :: Either Text AuthenticatedUser
          case decoded of
            Right authenticatedUser -> do
              acceptedConnection <- liftIO $ acceptRequest pendingConnection
              action authenticatedUser acceptedConnection
              return acceptedConnection
            Left _ -> reject
        Left _ -> reject
    Nothing -> reject -- TODO: Do something about the nesting?
  where
    reject = do
      liftIO $
        rejectRequestWith pendingConnection (defaultRejectRequest {rejectCode = 401, rejectMessage = "Unauthorized"})
      throwM $ Unauthorized mempty

verifyJwt :: JWK -> L.ByteString -> IO (Either JWTError ClaimsSet)
verifyJwt key jwtString =
  runExceptT $ do
    let audCheck = const True -- We don't care about the audience at this point
    jwtString' <- decodeCompact jwtString
    -- There is probably some way to integrate with Servant's auth combinators
    verifyClaims (defaultJWTValidationSettings audCheck) key jwtString'

createJwt :: (Has JWTSettings r, MonadReader r m, MonadIO m, MonadThrow m) => AuthenticatedUser -> m TokenView
createJwt user = do
  jwtConfiguration <- asks getter
  expiration <- liftIO getNextExpiration
  jwtResultE <- liftIO $ makeJWT user jwtConfiguration (Just expiration)
  jwtResult <- decodeUtf8 . toStrict <$> checkEither jwtResultE (ValidationError . tshow)
  return $ TokenView jwtResult

expirationInMinutes :: NominalDiffTime
expirationInMinutes = 120

getNextExpiration :: IO UTCTime
getNextExpiration = addUTCTime (expirationInMinutes * 60) <$> getCurrentTime