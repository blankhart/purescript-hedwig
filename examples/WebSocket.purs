module Examples.WebSocket where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (snoc)
import Data.Either (hush)
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (Foreign, F, readString, unsafeToForeign)
import Hedwig ((:>), (<!#))
import Hedwig as H
import Web.Event.EventTarget as EET
import Web.Socket.Event.CloseEvent as CE
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.ReadyState (ReadyState)
import Web.Socket.WebSocket (WebSocket, readyState)
import Web.Socket.WebSocket as WS

type Model = 
  { socket :: WebSocket
  , status :: ReadyState
  , chat :: String
  , log :: Array String 
  }

data ChatMsg 
  = UpdatedChat String 
  | SubmittedChat 
  | ReceivedChat String

data Msg 
  = ConnectionMsg ReadyState
  | ChatMsg ChatMsg 
  | NoOp

socketSub :: WebSocket -> H.Sub Msg
socketSub socket = \sink -> liftEffect do
  listenerMessageEvent <- EET.eventListener \ev -> do
    for_ (ME.fromEvent ev) \msgEvent ->
      for_ (maybeParse readString (ME.data_ msgEvent)) (sink <<< ChatMsg <<< ReceivedChat)
  EET.addEventListener
    WSET.onMessage
    listenerMessageEvent
    false
    (WS.toEventTarget socket)
  listenerOpenEvent <- EET.eventListener \_ -> do
    status <- readyState socket 
    sink $ ConnectionMsg status
  EET.addEventListener
    WSET.onOpen
    listenerOpenEvent
    false
    (WS.toEventTarget socket)
  listenerErrorEvent <- EET.eventListener \_ -> do
    status <- readyState socket 
    -- Report that an error occurred, etc.
    sink $ ConnectionMsg status
  EET.addEventListener
    WSET.onError
    listenerErrorEvent
    false
    (WS.toEventTarget socket)
  listenerCloseEvent <- EET.eventListener \ev -> 
      for_ (CE.fromEvent ev) \_ -> do
    -- Determine whether connection was clean, reason, etc.
    status <- readyState socket 
    sink $ ConnectionMsg status
  EET.addEventListener
    WSET.onClose
    listenerCloseEvent
    false
    (WS.toEventTarget socket)
  where
    maybeParse :: forall a b. (Foreign -> F a) -> b -> Maybe a
    maybeParse read = hush <<< runExcept <<< read <<< unsafeToForeign

update :: H.Update Model Msg
update model = case _ of
  ChatMsg msg -> case msg of 
    ReceivedChat chat ->
      model { log = model.log `snoc` ("Echo: " <> chat <> "\n") } :> []
    UpdatedChat chat -> 
      model { chat = chat } :> [] 
    SubmittedChat ->
      model { 
        log = model.log `snoc` ("Sent: " <> model.chat <> "\n"),
        chat = "" 
      } <!# do 
        readyState <- readyState model.socket
        WS.sendString model.socket model.chat
        pure NoOp
  ConnectionMsg status ->
    model { status = status } :> []
  NoOp -> model :> []

view :: H.View Model Msg
view model = H.main [] [
  H.div [] [
    H.text ("Connection status: " <> show model.status),
    H.br [] [],
    H.input [ H.value model.chat, H.onInput (ChatMsg <<< UpdatedChat) ] [],
    H.button [ H.onClick (ChatMsg SubmittedChat) ] [ H.text "Send" ]
  ],
  H.ul [] (map (H.text >>> \e -> H.li [] [e]) model.log)
]

main :: Effect Unit
main = do 
  socket <- WS.create "wss://echo.websocket.org" []
  status <- readyState socket
  let model = { 
    socket, 
    status, 
    log: [], 
    chat: "Echo Me" 
  }
  let init = Tuple model [socketSub socket]
  H.mount "main" { init, update, view } 
