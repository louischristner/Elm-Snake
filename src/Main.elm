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
import Html.Attributes exposing (..)
import Random


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


type alias Vector2D a =
  { x : a
  , y : a
  }

type alias Vector2DF =
  Vector2D Float

type alias Object a =
  { a | position : Vector2DF
  }

type alias Node =
  Object { index : Int }

type alias Apple =
  Object {}

type Direction
  = UP
  | DOWN
  | LEFT
  | RIGHT

type alias Model =
  { snake : List Node
  , timeKeeper : Float
  , direction : Direction
  , apples : List Apple
  , score : Int
  , keys : List Keyboard.Key
  , screen : (Int, Int)
  , camera : Camera
  }


initNode : (Float, Float) -> Int -> Node
initNode (x, y) index =
  { position =
    { x = x
    , y = y
    }
  , index = index
  }

initDefaultNode : Node
initDefaultNode =
  initNode (0, 0) 0

initSnake : List Node
initSnake =
  [ initNode (0, 0) 0
  , initNode (1, 0) 1
  , initNode (2, 0) 2
  ]

initModel : Model
initModel =
  { snake = initSnake
  , timeKeeper = 0
  , direction = LEFT
  , apples = []
  , score = 0
  , keys = []
  , screen = (600, 600)
  , camera = Camera.fixedArea 100 (0, 0)
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
  | NewApple (Int, Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick dt ->
      (tick (dt / 1000) model
      , if not <| List.isEmpty model.apples then
          Cmd.none
        else
          Random.generate NewApple <| Random.pair (Random.int -10 10) (Random.int -10 10)
      )

    Keys keyMsg ->
      ({ model | keys = Keyboard.update keyMsg model.keys }, Cmd.none)

    NewApple (x, y) ->
      ({ model | apples = { position = { x = toFloat x, y = toFloat y } } :: model.apples }, Cmd.none)


updateSnake : Direction -> List Node -> List Node
updateSnake direction snake =
  let
    snakeHead = Maybe.withDefault initDefaultNode <| List.head snake
  in
    case direction of
      UP ->
        { snakeHead
        | position = { x = snakeHead.position.x, y = snakeHead.position.y - 1 }
        } :: List.map (\node -> { node | index = node.index + 1 }) (List.take (List.length snake - 1) snake)

      DOWN ->
        { snakeHead
        | position = { x = snakeHead.position.x, y = snakeHead.position.y + 1 }
        } :: List.map (\node -> { node | index = node.index + 1 }) (List.take (List.length snake - 1) snake)

      LEFT ->
        { snakeHead
        | position = { x = snakeHead.position.x - 1, y = snakeHead.position.y }
        } :: List.map (\node -> { node | index = node.index + 1 }) (List.take (List.length snake - 1) snake)

      RIGHT ->
        { snakeHead
        | position = { x = snakeHead.position.x + 1, y = snakeHead.position.y }
        } :: List.map (\node -> { node | index = node.index + 1 }) (List.take (List.length snake - 1) snake)

updateDirection : Direction -> Input -> Direction
updateDirection direction arrows =
  case direction of
    UP ->
      if arrows.x == 0 then
        direction
      else
        if arrows.x > 0 then
          RIGHT
        else
          LEFT

    DOWN ->
      if arrows.x == 0 then
        direction
      else
        if arrows.x > 0 then
          RIGHT
        else
          LEFT

    LEFT ->
      if arrows.y == 0 then
        direction
      else
        if arrows.y > 0 then
          DOWN
        else
          UP

    RIGHT ->
      if arrows.y == 0 then
        direction
      else
        if arrows.y > 0 then
          DOWN
        else
          UP

updateApples : List Apple -> List Node -> List Apple
updateApples apples snake =
  List.filter (\apple -> List.length (List.filter (\node -> node.position == apple.position) snake) == 0) apples

tick : Float -> Model -> Model
tick dt model =
  let
    direction =
      updateDirection model.direction
        <| Keyboard.Arrows.arrows model.keys

    apples =
      updateApples model.apples model.snake

    newSnakeNode =
      Maybe.withDefault initDefaultNode (List.head (List.drop (List.length model.snake - 1) model.snake))
  in
    if model.timeKeeper < (0.2 * (1 - 0.002 * toFloat model.score)) then
      { model
      | snake =
        if List.length apples == List.length model.apples then
          model.snake
        else
          model.snake ++ [ { newSnakeNode | index = newSnakeNode.index + 1 } ]
      , timeKeeper = model.timeKeeper + dt
      , apples = apples
      , score = model.score + (List.length model.apples - List.length apples)
      , direction = direction
      }
    else
      { model
      | snake = updateSnake model.direction model.snake
      , timeKeeper = 0
      , direction = direction
      }


viewRectangle : Color.Color -> (Float, Float) -> Object a -> Renderable
viewRectangle color size object =
  let
    (sizeX, sizeY) = size
  in
    Render.shape Render.rectangle
      { color = color
      , position = (-(sizeX / 2) + object.position.x * sizeX, -(sizeY / 2) + object.position.y * sizeY)
      , size = (sizeX, sizeY)
      }

viewNode : Int -> (Float, Float) -> Node -> Renderable
viewNode snakeSize size node =
  let
    color = Color.rgb 0 (toFloat node.index / toFloat snakeSize) 0
  in
    viewRectangle color size node

view : Model -> Html Msg
view model =
  Html.div [ style "background" "rgba(51,51,51,0.7)", style "height" "100%", style "width" "100%", style "left" "0", style "top" "0", style "position" "fixed" ]
    [ text (String.fromInt model.score)
    -- , ul []
    --   <| List.map (\node -> li [] [ text <| String.fromInt node.index ]) model.snake
    , Game.renderCentered { time = 0, camera = model.camera, size = model.screen }
      <| List.map (viewNode (List.length model.snake) (0.4, 0.4)) model.snake ++ List.map (viewRectangle Color.red (0.4, 0.4)) model.apples ++ [ viewRectangle Color.white (14, 14) initDefaultNode ]
    ]
