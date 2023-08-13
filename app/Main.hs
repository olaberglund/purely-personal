{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Function ((&))
import Data.Text (Text, pack)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Lucid
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Lucid (HTML)

type RecipesHref = "about"

type StylesHref = "styles"

type BlogAPI =
  Get '[HTML] HomePage
    :<|> RecipesHref :> Get '[HTML] RecipesPage
    :<|> StylesHref :> Raw

data RecipesPage = RecipesPage

instance ToHtml RecipesPage where
  toHtml RecipesPage =
    doc_ $ do
      h1_ "Recipes"
      p_ "This is my about page!"

  toHtmlRaw = toHtml

data HomePage = HomePage

instance ToHtml HomePage where
  toHtml HomePage =
    doc_ $ do
      h1_ "Hello, there!"
      p_ "This is my personal blog!"
      button_ "YO!"

  toHtmlRaw = toHtml

mkServer :: Server BlogAPI
mkServer =
  return HomePage
    :<|> return RecipesPage
    :<|> serveDirectoryWebApp "static"

app :: Application
app = mkServer & serve (Proxy :: Proxy BlogAPI)

main :: IO ()
main = run 8080 app

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

urlpath :: forall s. (KnownSymbol s) => Text
urlpath = pack $ symbolVal (Proxy :: Proxy s)
