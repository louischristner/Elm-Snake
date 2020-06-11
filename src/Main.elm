module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Color
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable)
import Keyboard
import Keyboard.Arrows
import Html exposing (..)
import Browser.Navigation exposing (Key)


main : Program () Model Msg
main =
  Browser.element
  { view = view
  , update = update
  , init = init
  , subscriptions = subs
  }


type alias Input =
  { x : Int
  , y : Int
  }


type alias Model =
  { position : (Float, Float)
  , velocity : (Float, Float)
  , keys : List Keyboard.Key
  , screen : (Int, Int)
  , camera : Camera
  }


initModel : Model
initModel =
  { position = (0, 3)
  , velocity = (0, 0)
  , keys = []
  , screen = (800, 600)
  , camera = Camera.fixedWidth 8 (0, 0)
  }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel, Cmd.none)


subs : Model -> Sub Msg
subs _ =
  Sub.batch
    [ Sub.map Keys Keyboard.subscriptions
    , onAnimationFrameDelta Tick
    ]


type Msg
  = Tick Float
  | Keys Keyboard.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick dt ->
      (tick (dt / 1000) model, Cmd.none)

    Keys keyMsg ->
      ({ model | keys = Keyboard.update keyMsg model.keys }, Cmd.none)


tick : Float -> Model -> Model
tick dt model =
  let
    ((x, y), (vx, vy)) =
      (model.position, model.velocity)

    arrows =
      Keyboard.Arrows.arrows model.keys

    vy_ = if arrows.y > 0 then 4 else vy - 9.81 * dt

    (newP, newV) =
      if y <= 0 then
        ((x, 0.00001), (0, -vy_ * 0.9))
      else
        ((x, y + vy_ * dt), (0, vy_))
  in
    { model
    | position = newP
    , velocity = newV
    }


view : Model -> Html Msg
view model =
  let
    ((_, y), (_, vy)) =
      (model.position, model.velocity)
  in
    Html.div []
      [ text (String.fromFloat y ++ " " ++ String.fromFloat vy)
      , Game.renderCentered { time = 0, camera = model.camera, size = model.screen }
        [ Render.shape Render.rectangle { color = Color.green, position = (-10, -10), size = (20, 10) }
        , Render.shape Render.circle { color = Color.blue, position = model.position, size = (0.5, 0.5) }
        ]
      ]
