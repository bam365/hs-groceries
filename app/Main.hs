module Main where

import Data.Int (Int32)
import Data.Monoid (mconcat)
import Data.Text (Text)
import Data.Text.Lazy (pack)
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Lucid
import Network.HTTP.Types.Status (status500)
import Web.Scotty

import GroceriesLib 
import qualified Views as V

main :: IO ()
main = do
    conn <- open "db/groceries.db"
    let db = defaultDbSettings
    let runDb = (liftIO . runBeamSqliteDebug putStrLn conn) :: SqliteM a -> IO a
    let repo = BeamSqliteGroceriesRepoR runDb db
    runServer repo

runServer repo = scotty 8080 $ staticContent <> pages <> forms <> partials
  where
    lucidHtml = html . renderText
    getFile url mime fileName = get url $ do
        setHeader "Content-Type" mime
        file fileName

    staticContent = do
        getFile "/js/htmx.min.js" "application/javascript" "static/htmx.min.js"
        getFile "/css/site.css" "text/css" "static/site.css"

    pages = do
        get "/" $ lucidHtml . V.page $ V.mainPage

    forms = do
        post "/grocery" $ do
            name <- param "name"
            liftIO $ insertGrocery repo name 
            setHeader "HX-Trigger" "newGrocery"
            lucidHtml V.addGroceryForm

        patch "/grocery" $ do
            allParams <- params
            liftIO $ print allParams
            liftIO $ putStrLn "^ PARAMS"
            id <- (param "id" :: ActionM Int32)
            active <- (== "on") <$> ((param "active" :: ActionM String) `rescue` (\_ -> return "false"))
            result <- liftIO $ setGot repo (mkGroceryId id) active
            case result of
                Nothing -> status status500
                Just grocery' -> lucidHtml $ V.renderGroceryRow grocery'

        post "/clear-gotten" $ do
            liftIO $ clearGotten repo
            setHeader "HX-Trigger" "clearedGroceries"
            lucidHtml V.clearGottenForm
        

    partials = do
        let partialRoute r =  literal ("/partial/" <> r)
        get (partialRoute "grocery-list") $ do
            groceries <- liftIO $ getAllGroceries repo
            lucidHtml $ V.groceryList groceries
