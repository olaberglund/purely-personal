module DBing where

import Data.Int (Int64)
import Data.Profunctor.Product (p3)
import Database.PostgreSQL.Simple (ConnectInfo (connectDatabase, connectUser), defaultConnectInfo)
import qualified Database.PostgreSQL.Simple as PG
import Opaleye

connInfo :: PG.ConnectInfo
connInfo =
  defaultConnectInfo
    { connectDatabase = "blog",
      connectUser = "ola"
    }

recipeTable :: Table (Maybe (Field SqlInt4), Field SqlText, Field SqlText) (Field SqlInt4, Field SqlText, Field SqlText)
recipeTable = table "recipes" (p3 (tableField "id", tableField "name", tableField "description"))

testInsert :: Insert Int64
testInsert =
  Insert
    { iTable = recipeTable,
      iRows = [(Nothing, sqlString "potatismos", sqlString "En smaskig familjefavorit!")],
      iReturning = rCount,
      iOnConflict = Nothing
    }

test :: IO ()
test = do
  conn <- PG.connect connInfo
  colsAffected <- runInsert conn testInsert
  print colsAffected
