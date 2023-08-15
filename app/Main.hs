{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Amazement (generateMaze, showMaze)
import Control.Monad.IO.Class
import DBing (connInfo, insertRecipe, recipeSelect)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Database.PostgreSQL.Simple as PG
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Lucid
import Network.HTTP.Types (hLocation)
import Network.Wai.Handler.Warp (run)
import Opaleye (runInsert, runInsert_, runSelectI)
import Poker (Card, card, cards, go)
import Servant
  ( Application,
    FormUrlEncoded,
    FromHttpApiData,
    Get,
    Handler,
    NoContent,
    PostNoContent,
    Proxy (..),
    QueryParam,
    Raw,
    ReqBody,
    Server,
    ServerError (errHeaders),
    err303,
    serve,
    serveDirectoryWebApp,
    throwError,
    type (:<|>) (..),
    type (:>),
  )
import Servant.HTML.Lucid (HTML)
import Web.FormUrlEncoded (FromForm)
import Web.Internal.HttpApiData (parseQueryParam)

-- Server

type RecipesHref = "recipes"

type StylesHref = "styles"

type RecipesInsertionHref = "insert"

type MazesHref = "mazes"

type PokerHref = "poker"

data RecipeForm = RecipeForm
  { name :: Text,
    description :: Text
  }
  deriving (Generic)

instance FromForm RecipeForm

newtype MazeForm = MazeForm {seed :: Int}
  deriving (Generic)

instance FromForm MazeForm

type BlogAPI =
  Get '[HTML] HomePage
    :<|> RecipesHref :> Get '[HTML] RecipesPage
    :<|> RecipesInsertionHref :> ReqBody '[FormUrlEncoded] RecipeForm :> PostNoContent
    :<|> MazesHref :> QueryParam "seed" Int :> Get '[HTML] MazePage
    :<|> PokerHref :> QueryParam "first" Card :> QueryParam "second" Card :> QueryParam "third" Card :> QueryParam "fourth" Card :> QueryParam "board" Board :> Get '[HTML] PokerPage
    :<|> StylesHref :> Raw

newtype PokerPage = PokerPage (Maybe (Double, Double, Double))

instance ToHtml PokerPage where
  toHtml (PokerPage Nothing) =
    doc_ $ do
      h1_ "Pokerkalkylator"
      form_
        [action_ (urlpath @PokerHref), method_ "get"]
        ( fieldset_ $ do
            cardsForm "first" "second" "Seat 1"
            br_ []
            cardsForm "third" "fourth" "Seat 2"
            br_ []
            inp_ "board" "Bordet:"
            br_ []
            button_ "Skicka"
        )
  toHtml (PokerPage (Just (p1w, p2w, chops))) = do
    toHtml (PokerPage Nothing)
    p_ $ do
      toHtml ("Player 1 wins " <> take 4 (show $ p1w * 100) <> "% of the time")
      br_ []
      toHtml ("Player 2 wins " <> take 4 (show $ p2w * 100) <> "% of the time")
      br_ []
      toHtml ("They chop " <> take 4 (show (100 * chops)) <> "% of the time")

  toHtmlRaw = toHtml

cardsForm :: (Monad m) => Text -> Text -> Text -> HtmlT m ()
cardsForm id1 id2 title =
  fieldset_
    ( legend_ (toHtml title)
        <> inp_ id1 "First Card:"
        <> inp_ id2 "Second Card:"
    )

inp_ :: (Monad m) => Text -> Text -> HtmlT m ()
inp_ id label = do
  label_ [for_ id] (toHtml label)
  br_ []
  input_ [id_ id, name_ id]
  br_ []

newtype MazePage = MazePage (Maybe Int)

instance ToHtml MazePage where
  toHtml (MazePage Nothing) = doc_ $ do
    h1_ "Labyrinter"
    form_ [] $ do
      fieldset_ [class_ "gapped"] $ do
        legend_ "Generera en labyrint"
        label_ [for_ "seed"] "Seed:"
        input_ [type_ "number", name_ "seed", value_ "0"]
        button_ "Generera"
  toHtml (MazePage (Just n)) = do
    toHtml (MazePage Nothing)
    pre_ [class_ "maze"] (toHtml $ showMaze $ generateMaze n)
    br_ []

  toHtmlRaw = toHtml

newtype Board = Board [Card]

data CardForm = CardForm
  { first :: Card,
    second :: Card,
    third :: Card,
    fourth :: Card,
    board :: Board
  }
  deriving (Generic)

instance FromForm CardForm

instance FromHttpApiData Card where
  parseQueryParam s = maybe (Left "Invalid card") Right (card (unpack s))

instance FromHttpApiData Board where
  parseQueryParam t
    | t == "" = Right (Board [])
    | null (cards (unpack t)) = Left "Invalid cards"
    | otherwise = Right (Board $ cards (unpack t))

mkServer :: PG.Connection -> Server BlogAPI
mkServer conn =
  return HomePage
    :<|> recipes
    :<|> newRecipe
    :<|> mazes
    :<|> poker
    :<|> serveDirectoryWebApp "static"
  where
    recipes :: Handler RecipesPage
    recipes = do
      rs <- liftIO (runSelectI conn recipeSelect)
      return (RecipesPage rs)

    mazes :: Maybe Int -> Handler MazePage
    mazes = return . MazePage

    poker :: Maybe Card -> Maybe Card -> Maybe Card -> Maybe Card -> Maybe Board -> Handler PokerPage
    poker c1 c2 c3 c4 b = return $ PokerPage $ go' <$> c1 <*> c2 <*> c3 <*> c4 <*> b

    newRecipe :: RecipeForm -> Handler NoContent
    newRecipe (RecipeForm n d) = do
      liftIO (runInsert conn (insertRecipe n d))
      throwError $ err303 {errHeaders = [(hLocation, "http://localhost:8080" <> encodeUtf8 (urlpath @RecipesHref))]}

    go' :: Card -> Card -> Card -> Card -> Board -> (Double, Double, Double)
    go' a b c d (Board bo) = go [a, b] [c, d] bo

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
        title_ "Amalgam"
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        link_ [rel_ "stylesheet", href_ "styles/styles.css"]
      body_ $ do
        navbar_
        b

navbar_ :: (Monad m) => HtmlT m ()
navbar_ = nav_ $ do
  a_ [href_ "/"] "Hem"
  a_ [href_ $ urlpath @RecipesHref] "Maträtter"
  a_ [href_ $ urlpath @MazesHref] "Labyrinter"
  a_ [href_ $ urlpath @PokerHref] "Pokerkalkylator"

newtype RecipesPage = RecipesPage [(Int, Text, Text)]

instance ToHtml RecipesPage where
  toHtml (RecipesPage rs) =
    doc_ $ do
      h1_ "Maträtt"
      form_ [action_ (urlpath @RecipesInsertionHref), method_ "post"] $
        fieldset_ $ do
          legend_ "Lägg till en maträtt"
          inp_ "name" "Namn: "
          label_ [for_ "description"] "Beskrivning:"
          br_ []
          textarea_ [type_ "text", rows_ "4", cols_ "50", name_ "description"] ""
          br_ []
          input_ [type_ "submit", value_ "Lägg till"]
      mapM_
        ( \(_, n, d) -> do
            h2_ (toHtml n)
            p_ (toHtml d)
        )
        rs

  toHtmlRaw :: (Monad m) => RecipesPage -> HtmlT m ()
  toHtmlRaw = toHtml

data HomePage = HomePage

instance ToHtml HomePage where
  toHtml HomePage =
    doc_ $ do
      h1_ "Hejsan!"
      p_ "En blandning."

  toHtmlRaw = toHtml

urlpath :: forall s. (KnownSymbol s) => Text
urlpath = pack $ "/" <> symbolVal (Proxy :: Proxy s)
