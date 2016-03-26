{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans where
import           Data.Thyme.Time
import           Data.Yaml

#define deriveShowJSON(X) \
  instance ToJSON X where { toJSON = toJSON . show }; \
  instance FromJSON X where { parseJSON = fmap read . parseJSON };

#define deriveGenericJSON(X) \
  instance ToJSON X; \
  instance FromJSON X;

deriveShowJSON(LocalTime)
deriveShowJSON(DiffTime)
deriveShowJSON(UTCTime)
deriveShowJSON(NominalDiffTime)
deriveGenericJSON(TimeZone)
