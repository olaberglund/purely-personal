{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import DBing (connInfo, recipeSelect)
import Data.Function ((&))
import Data.Text (Text, pack)
import qualified Database.PostgreSQL.Simple as PG
import GHC.TypeLits (KnownSymbol, symbolVal)
import Lucid
import Network.Wai.Handler.Warp (run)
import Opaleye (runSelectI)
import Servant
import Servant.HTML.Lucid (HTML)

-- Server

type RecipesHref = "about"

type StylesHref = "styles"

type BlogAPI =
  Get '[HTML] HomePage
    :<|> RecipesHref :> Get '[HTML] RecipesPage
    :<|> StylesHref :> Raw

mkServer :: PG.Connection -> Server BlogAPI
mkServer conn =
  return HomePage
    :<|> serveRecipes
    :<|> serveDirectoryWebApp "static"
  where
    serveRecipes :: Handler RecipesPage
    serveRecipes = do
      rs <- liftIO (runSelectI conn recipeSelect)
      return (RecipesPage rs)

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
      hr_ []
      ul_ $ do
        mapM_
          ( \(i, n, d) -> li_ $ do
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
urlpath = pack $ symbolVal (Proxy :: Proxy s)
