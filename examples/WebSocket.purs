module Examples.WebSocket where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (snoc)
import Data.Either (either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (Foreign, F, readString, unsafeToForeign)
import Hedwig as H
import Hedwig ((:>))
import Web.Event.EventTarget as EET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS

type Model = 
  { socket :: WS.WebSocket
  , log :: Array String 
  }

data Msg = Send String | Recv String | NoOp

socketSub :: WS.WebSocket -> H.Sub Msg
socketSub socket = \sink -> liftEffect do
  listener <- EET.eventListener \ev -> do
    for_ (ME.fromEvent ev) \msgEvent ->
      for_ (readHelper readString (ME.data_ msgEvent)) (Recv >>> sink)
  EET.addEventListener
    WSET.onMessage
    listener
    false
    (WS.toEventTarget socket)
  where
    readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
    readHelper read =
      either (const Nothing) Just <<< runExcept <<< read <<< unsafeToForeign

update :: H.Update Model Msg
update model = case _ of
  Recv msg ->
    model { log = model.log `snoc` ("Echo: " <> msg <> "\n") } :> []
  Send msg ->
    model { log = model.log `snoc` ("Sent: " <> msg <> "\n") } :> [
      liftEffect (WS.sendString model.socket msg *> pure NoOp)
    ]
  NoOp -> model :> []

view :: H.View Model Msg
view model = H.main [] [
  H.input [H.onInput Send] [H.text "Hello"],
  H.div [] (map H.text model.log)
]

main :: Effect Unit
main = do 
  connection <- WS.create "ws://echo.websocket.org" []
  let init = Tuple { socket: connection, log: [] } [socketSub connection]
  H.mount "main" { init, update, view } 
