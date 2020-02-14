module Examples.Router where

import Data.Either (either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.String (drop)
import Data.String.CodeUnits (drop, dropWhile) as Str
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Hedwig ((:>))
import Hedwig as H
import Prelude (Unit, identity, bind, show, ($), (<$>), (<>), (>>>), (>>=), (==), (/=))
import Web.Event.EventTarget (eventListener, addEventListener) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Event.HashChangeEvent as HCE
import Web.HTML.Event.HashChangeEvent.EventTypes as HCET
import Web.HTML.Window as Window
import Windrose.Router (type (:<|>), type (:>), C, P, Q, RouteProxy(..), V, allLinks, mkRoutable, route)

newtype Renderer = Renderer (H.View Model Msg)

type Model = { url :: String, renderer :: Renderer }

-- The only action taken by this application is to change the route.
data Msg = Navigate String | NoOp

-- Our routes are defined through a Windrose-style typelevel API.
-- Nested routes are permitted where they make sense, as in this example
-- which branches after "posts".
type ExampleApi =
        V "index"
  :<|>  P "posts" :> (  Q ( sortBy :: Maybe String ) :> V "postIndex"
                  :<|>  C "id" Int :> P "edit" :> V "postEdit")

-- Subscribe to hash changes
subscribeHashChange :: H.Sub Msg
subscribeHashChange = \sink -> liftEffect do
  listener <- DOM.eventListener $ \ev -> do 
    for_ (HCE.fromEvent ev) $ \hce -> do
      let hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ HCE.newURL hce
      sink (Navigate hash)
  liftEffect $
    DOM.window
      >>= Window.toEventTarget
      >>> DOM.addEventListener HCET.hashchange listener false

-- The dashboard is the basic layout common to all routes in this example.
dashboard :: Array String -> String -> Renderer
dashboard routes description = Renderer $ \model ->
  let
    f url = H.h3 [H.class' (if drop 1 url == model.url then "active" else "")] [
      H.text $ "Sample URL: {" <> url <> "}",
      H.a [H.href $ "#" <> url ] [H.text "Push"]
    ]
  in 
    H.main [] [
      H.div [] $ [
        H.h2 [] [H.text $ "Url: {" <> model.url <> "}"],
        H.h2 [] [H.text $ "Description: " <> description],
        H.div [] $ f <$> routes
      ]
    ]    

main :: Effect Unit
main = do 
  H.mount "main"
    { init: Tuple { renderer: handlers.index, url: urls.index } [ subscribeHashChange ]
    , update: \model -> case _ of 
      Navigate url -> model
        { url = url
        , renderer = either (\err -> handle ("Error 404: " <> show err)) identity $ 
            route api handlers url
        } :> []
      NoOp -> model :> []
    , view: \model -> (case model.renderer of Renderer view -> view model)
    } 
  where
    -- Canonicalize the typelevel API, which undoes nesting.
    api = mkRoutable (RouteProxy :: RouteProxy ExampleApi) 

    -- Define links to the endpoints in the API.
    urls = allLinks api

    -- Define handlers for the endpoints in the API.
    handle description = dashboard routes description

    -- Define some sample routes to play with, including an error case.
    routes = [
      urls.index,
      urls.postIndex { sortBy: Nothing },
      urls.postIndex { sortBy: Just "id" },
      urls.postIndex { sortBy: Just "title" },
      urls.postEdit 1,
      urls.postEdit 2,
      "nonsense, but try me anyway"
    ]

    handlers = 
      { index: 
          handle "index"
      , postEdit: \n -> 
          handle ("edit post #" <> show n)
      , postIndex: \{ sortBy: m } -> 
          handle ("all posts (" <> maybe "unsorted" ("sorting on " <> _) m <> ")")
      }

