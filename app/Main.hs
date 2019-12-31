module Main where

import ClassyPrelude
import Lib
import Crypto.JOSE (JWK)
import Servant.Auth.Server (fromSecret)

getPort :: IO Int
getPort = return 8080

getJwtSecret :: IO JWK
getJwtSecret = return $ fromSecret . fromString $ "SOME-AMAZING-SECRET-THAT-IS-KEPT-VERY-SECURELY-TRUST-ME"

main :: IO ()
main = do
  port <- getPort
  jwtSecret <- getJwtSecret
  defaultMain jwtSecret port
