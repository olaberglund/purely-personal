{-# LANGUAGE OverloadedStrings #-}

module DBing where

import Data.Int (Int64)
import Data.Profunctor.Product (p3)
import Data.Text
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

insertRecipe :: Text -> Text -> Insert Int64
insertRecipe n d =
  Insert
    { iTable = recipeTable,
      iRows = [(Nothing, sqlStrictText n, sqlStrictText d)],
      iReturning = rCount,
      iOnConflict = Nothing
    }

recipeSelect :: Select (Field SqlInt4, Field SqlText, Field SqlText)
recipeSelect = selectTable recipeTable

test :: IO ()
test = do
  conn <- PG.connect connInfo
  rows <- runInsert conn (insertRecipe "1" "d")
  print rows
