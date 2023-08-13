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

recipeSelect :: Select (Field SqlInt4, Field SqlText, Field SqlText)
recipeSelect = selectTable recipeTable

test :: IO ()
test = do
  conn <- PG.connect connInfo
  rows <- runSelectI conn recipeSelect
  print rows
