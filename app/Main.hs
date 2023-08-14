{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import DBing (connInfo, insertRecipe, recipeSelect)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Database.PostgreSQL.Simple as PG
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Lucid
import Network.HTTP.Types (hLocation)
import Network.Wai.Handler.Warp (run)
import Opaleye (runInsert, runInsert_, runSelectI)
import Servant
import Servant.HTML.Lucid (HTML)
import Web.FormUrlEncoded (FromForm)

-- Server

type RecipesHref = "recipes"

type StylesHref = "styles"

type RecipesInsertionHref = "insert"

data RecipeForm = RecipeForm
  { name :: Text,
    description :: Text
  }
  deriving (Generic)

instance FromForm RecipeForm

type BlogAPI =
  Get '[HTML] HomePage
    :<|> RecipesHref :> Get '[HTML] RecipesPage
    :<|> RecipesInsertionHref :> ReqBody '[FormUrlEncoded] RecipeForm :> Post '[HTML] RecipesPage
    :<|> StylesHref :> Raw

mkServer :: PG.Connection -> Server BlogAPI
mkServer conn =
  return HomePage
    :<|> recipes
    :<|> newRecipe
    :<|> serveDirectoryWebApp "static"
  where
    recipes :: Handler RecipesPage
    recipes = do
      rs <- liftIO (runSelectI conn recipeSelect)
      return (RecipesPage rs)

    newRecipe :: RecipeForm -> Handler RecipesPage
    newRecipe (RecipeForm n d) = do
      liftIO (runInsert conn (insertRecipe n d))
      throwError $ err303 {errHeaders = [(hLocation, "http://localhost:8080/" <> encodeUtf8 (urlpath @RecipesHref))]}

app :: PG.Connection -> Application
app = serve (Proxy :: Proxy BlogAPI) . mkServer

main :: IO ()
main = PG.connect connInfo >>= run 8080 . app

-- HTML
doc_ :: (Monad m) => HtmlT m () -> HtmlT m ()
doc_ b =
  doctypehtml_ $
    html_ $ do
      head_ $ do
        title_ "My Blog"
        link_ [rel_ "stylesheet", href_ "styles/styles.css"]
      body_ $ do
        navbar_
        b

navbar_ :: (Monad m) => HtmlT m ()
navbar_ = nav_ $ do
  a_ [href_ "/"] "Home"
  a_ [href_ $ urlpath @RecipesHref] "Recipes"

newtype RecipesPage = RecipesPage [(Int, Text, Text)]

instance ToHtml RecipesPage where
  toHtml (RecipesPage rs) =
    doc_ $ do
      h1_ "Recipes"
      form_ [action_ (urlpath @RecipesInsertionHref), method_ "post"] $
        fieldset_ $ do
          legend_ "Add recipe"
          label_ [for_ "name"] "Name:"
          input_ [type_ "text", name_ "name"]
          label_ [for_ "description"] "Description:"
          input_ [type_ "text", name_ "description"]
          br_ []
          input_ [type_ "submit", value_ "Add"]
      ul_ $ do
        mapM_
          ( \(_, n, d) -> li_ $ do
              h2_ (toHtml n)
              ul_ (li_ (toHtml d))
          )
          rs

  toHtmlRaw = toHtml

data HomePage = HomePage

instance ToHtml HomePage where
  toHtml HomePage =
    doc_ $ do
      h1_ "Hello, there!"
      p_ "En blogg!"

  toHtmlRaw = toHtml

urlpath :: forall s. (KnownSymbol s) => Text
urlpath = pack $ "/" <> symbolVal (Proxy :: Proxy s)
