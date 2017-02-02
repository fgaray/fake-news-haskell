module Database.Types where

import Prelude
import Data.Text
import Database.Fields
import Data.Aeson.TH

-- Para los JsonB de la base de datos

data Reporte = Reporte
    { ip        :: Text
    , mensaje   :: Text
    } deriving (Eq, Show)


$(deriveJSON defaultOptions ''Reporte)
