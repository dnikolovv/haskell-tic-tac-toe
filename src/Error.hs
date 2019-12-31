module Error where

import ClassyPrelude

data AppError
  = ValidationError Text
  | NotFound Text
  | Conflict Text
  | Unauthorized Text
  deriving Show

instance Exception AppError