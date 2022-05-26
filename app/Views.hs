module Views 
    ( groceryList
    , page
    , mainPage
    , addGroceryForm
    , renderGroceryRow
    , clearGottenForm
    ) where

import Control.Monad (forM_)
import Data.Text (Text, pack)
import Lucid
import Lucid.Base (makeAttribute)

import GroceriesLib

                           
hxget_ = makeAttribute "hx-get"
hxpost_ = makeAttribute "hx-post"
hxpatch_ = makeAttribute "hx-patch"
hxtarget_ = makeAttribute "hx-target"
hxtrigger_ = makeAttribute "hx-trigger"
hxswap_ = makeAttribute "hx-swap"

page :: Html () -> Html ()
page body = doctypehtml_ (head_ headT <> body_ body)
  where 
    headT = do 
        title_ "GroceryList" 
        link_ [ rel_ "stylesheet", href_ "/css/site.css" ] 
        script_ [ src_ "/js/htmx.min.js" , defer_ "" ] ("" :: Text)

mainPage :: Html ()
mainPage = div_ [ class_ "main" ] (title <> addGroceryForm <> lst <> clearGottenForm)
  where 
    title = h1_ "Groceries!"
    lst = div_ [ id_ "#grocery-list"
               , hxget_ "/partial/grocery-list"
               , hxtrigger_ "load,newGrocery from:body,clearedGroceries from:body" 
               ] mempty 

addGroceryForm :: Html ()
addGroceryForm = form_ [ hxpost_ "/grocery" ] (groceryInput <> button)
  where
    groceryInput = label_ (
        do toHtml ("Grocery" :: Text) 
           input_ 
                [ class_ "add-grocery-input"
                , name_ "name"
                , type_ "text"
                ])
    button = button_ "Add"

groceryList :: [Grocery] -> Html ()
groceryList groceries = 
    table_ [class_ "grocery-list"] (
        tbody_ [ hxtarget_ "closest tr", hxswap_ "outerHTML" ] (
            forM_ groceries renderGroceryRow))

renderGroceryRow :: Grocery -> Html ()
renderGroceryRow (Grocery groceryId name active) = tr_ (checkCell <> labelCell)
  where
    labelCell :: Html ()
    labelCell = td_ (toHtml name) 
    formInputs = do
        input_ ([ class_ "active-chk"
                , type_ "checkbox"
                , name_ "active" 
                ] <> [ checked_ | active ])
        let idText = pack . show $ groceryId
        input_ [ type_ "hidden", name_ "id", value_ idText ]
    checkCell = td_ (form_ [hxpatch_ "/grocery", hxtrigger_ "change"] formInputs)

clearGottenForm :: Html ()
clearGottenForm = 
    form_
        [ class_ "clear-gotten"
        , hxpost_ "/clear-gotten"
        , hxswap_ "none" 
        ] (button_ "Clear")