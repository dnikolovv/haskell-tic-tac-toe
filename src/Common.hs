module Common where

import ClassyPrelude
import Control.Monad.Catch (MonadThrow, throwM)

checkEither :: (MonadThrow m, Exception e) => Either a b -> (a -> e) -> m b
checkEither (Right a) _ = return a
checkEither (Left e) f = throwM $ f e

checkMaybe :: (MonadThrow m, Exception e) => Maybe a -> e -> m a
checkMaybe (Just a) _ = return a
checkMaybe _ err = throwM err

checkBool :: (MonadThrow m, Exception e) => Bool -> e -> m ()
checkBool True _ = return ()
checkBool _ err = throwM err