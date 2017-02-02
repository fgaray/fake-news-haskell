{-# LANGUAGE OverlappingInstances #-}
module Database.Fields where


import Database.Persist.Sql
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.TH
import Prelude


data JsonB a = JsonB { unJsonB :: !a }
    deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''JsonB)


instance (ToJSON a, FromJSON a) => PersistField (JsonB a) where
    toPersistValue (JsonB x) = PersistDbSpecific . BSL.toStrict $ encode x

    fromPersistValue (PersistByteString x) = case eitherDecode (BSL.fromStrict x) of
                                               Left txt -> Left $ T.pack txt
                                               Right x' -> Right (JsonB x')
    fromPersistValue _                     = Left "Wrong PersistValue type"

instance (ToJSON a, FromJSON a) => PersistFieldSql (JsonB a) where
    sqlType _ = SqlOther "jsonb"
