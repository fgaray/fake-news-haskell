
module Database.Operators 
    ( (|+>|)
    , (|<+|)
    , (|||)
    , jsonArrayLength
    , deleteElementAt
    )
    where

import Prelude
import Database.Esqueleto.Internal.Sql
import Database.Esqueleto
import Data.Text
import Data.Aeson hiding (Value)
import qualified Data.ByteString.Lazy.Char8 as BS8
import Database.Fields
import qualified Data.Text as T


--------------------------------------------------------------------------------

concatJson :: SqlExpr (Value a) -> SqlExpr (Value Text) -> SqlExpr (Value a)
concatJson = unsafeSqlBinOp " || "

concatJsonFirst ::  SqlExpr (Value Text) -> SqlExpr (Value a) -> SqlExpr (Value a)
concatJsonFirst = unsafeSqlBinOp " || "


-- | Convierte un valor a json pero en un Text en vez de un bytestring para
-- poder pasarlo de forma correcta la DB
jsonBToJSON :: (ToJSON a) => a -> SqlExpr (Value Text)
jsonBToJSON = val . T.pack . BS8.unpack . encode



-- | Concatena un elemento al final de una lista
infix 5 |+>|
(|+>|) :: (ToJSON a)
       => SqlExpr (Value (JsonB [a]))    -- ^ El campo a agregar el elemento
       -> a                              -- ^ El elemento a agregar
       -> SqlExpr (Value (JsonB [a]))
xs |+>| x = concatJson xs (jsonBToJSON x)

-- | Concatena al principio de una lista
infix 5 |<+|
(|<+|) :: (ToJSON a)
       => a
       -> SqlExpr (Value (JsonB [a]))    -- ^ El campo a agregar el elemento
       -> SqlExpr (Value (JsonB [a]))
x |<+| xs = concatJsonFirst (jsonBToJSON x) xs




-- | Concatena un json con otro donde no necesariamente deben ser iguales, pero
-- el resultado debe ser del mismo tipo que el que se está modificando
infix 5 |||
(|||) :: (ToJSON a, ToJSON b)
      => SqlExpr (Value (JsonB a))      -- ^ El campo en la base de datos a concatenar
      -> b                              -- ^ El dato a concatenar
      -> SqlExpr (Value (JsonB a))
x ||| y = concatJson x (jsonBToJSON y)




-- | Obtiene el largo de un arreglo json
-- >>>  return $ jsonArrayLength (e ^. EstacionamientoComentarios)
jsonArrayLength :: (ToJSON a)
                => SqlExpr (Value (JsonB [a]))
                -> SqlExpr (Value Int)
jsonArrayLength = unsafeSqlFunction "jsonb_array_length"


deleteElementAt :: (ToJSON a)
                => SqlExpr (Value (JsonB [a]))
                -> SqlExpr (Value Int)
                -> SqlExpr (Value (JsonB [a]))
deleteElementAt = unsafeSqlBinOp "-"
