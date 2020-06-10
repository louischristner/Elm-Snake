module Main exposing (main)

import String exposing (fromInt)
import Playground exposing (..)



type Direction
  = UP
  | DOWN
  | LEFT
  | RIGHT

type alias Vector2D a =
  { x : a
  , y : a
  }

type alias Object a b =
  { a
  | position : Vector2D b
  }

type alias Apple =
  Object {} Number

type alias Node =
  Object { index : Int } Number

type alias Model =
  { score : Int
  , time : Int
  , direction : Direction
  , lastPosition : Vector2D Number
  , snake : List Node
  , rands : List Int
  , apple : Apple
  }

createNode : Int -> Vector2D Number -> Node
createNode index position =
  { position = position
  , index = index
  }

toVector2D : (a,a) -> Vector2D a
toVector2D (x,y) =
  { x = x
  , y = y
  }

initModel : Model
initModel =
  { score = 0
  , time = 0
  , direction = LEFT
  , lastPosition = toVector2D (-1,-1)
  , rands = []
  , apple = { position = toVector2D (-3, 0) }
  , snake =
    [ createNode 0 (toVector2D (0,0))
    , createNode 1 (toVector2D (1,0))
    , createNode 2 (toVector2D (2,0))
    ]
  }



-- █   █ █████ ████  █████ █
-- ██ ██ █   █ █   █ █     █
-- █ █ █ █   █ █   █ ███   █
-- █   █ █   █ █   █ █     █
-- █   █ █████ ████  █████ █████

main =
  game view update initModel



-- █   █ █████ ████  █████ █████ █████
-- █   █ █   █ █   █ █   █   █   █
-- █   █ █████ █   █ █████   █   ███
-- █   █ █     █   █ █   █   █   █
-- █████ █     ████  █   █   █   █████

getHeadNode : List Node -> Node
getHeadNode nodes =
  Maybe.withDefault
    (createNode 0 (toVector2D (0,0)))
    (List.head nodes)

getTailNode : List Node -> Node
getTailNode nodes =
  Maybe.withDefault
    (createNode 0 (toVector2D (0,0)))
    (List.head <| List.drop (List.length nodes - 1) nodes)

updateNodes : List Node -> List Node -> List Node
updateNodes prevNodes nodes =
  let
    prevHead = getHeadNode prevNodes
    head = getHeadNode nodes
  in
    if List.length nodes == 0 then
      []
    else
      { head
      | position = toVector2D (prevHead.position.x, prevHead.position.y)
      } :: updateNodes (List.drop 1 prevNodes) (List.drop 1 nodes)

updateSnake : Direction -> List Node -> List Node
updateSnake direction snake =
  let
    snakeHead = getHeadNode snake
  in
    case direction of
      UP ->
        { snakeHead | position = toVector2D (snakeHead.position.x, snakeHead.position.y - 1) } :: updateNodes snake (List.drop 1 snake)
      DOWN ->
        { snakeHead | position = toVector2D (snakeHead.position.x, snakeHead.position.y + 1) } :: updateNodes snake (List.drop 1 snake)
      LEFT ->
        { snakeHead | position = toVector2D (snakeHead.position.x - 1, snakeHead.position.y) } :: updateNodes snake (List.drop 1 snake)
      RIGHT ->
        { snakeHead | position = toVector2D (snakeHead.position.x + 1, snakeHead.position.y) } :: updateNodes snake (List.drop 1 snake)

getDirectionFromKeyboardXAxis : Direction -> Vector2D Number -> Direction
getDirectionFromKeyboardXAxis currentDirection moveVector =
  if moveVector.x /= 0 && moveVector.y == 0 then
    currentDirection
  else
    if moveVector.y /= 0 then
      if moveVector.y > 0 then
        DOWN
      else
        UP
    else
      currentDirection

getDirectionFromKeyboardYAxis : Direction -> Vector2D Number -> Direction
getDirectionFromKeyboardYAxis currentDirection moveVector =
  if moveVector.y /= 0 && moveVector.x == 0 then
    currentDirection
  else
    if moveVector.x /= 0 then
      if moveVector.x > 0 then
        RIGHT
      else
        LEFT
    else
      currentDirection

getDirectionFromKeyboard : Direction -> Keyboard -> Direction
getDirectionFromKeyboard currentDirection keyboard =
  let
    moveVector = toVector2D (toX keyboard, toY keyboard)
  in
    case currentDirection of
      UP ->
        getDirectionFromKeyboardYAxis currentDirection moveVector
      DOWN ->
        getDirectionFromKeyboardYAxis currentDirection moveVector
      LEFT ->
        getDirectionFromKeyboardXAxis currentDirection moveVector
      RIGHT ->
        getDirectionFromKeyboardXAxis currentDirection moveVector

isOverlapping : Object a b -> Object c b -> Bool
isOverlapping node1 node2 =
  node1.position == node2.position

isOverlappingSnake : Int -> List (Object a b) -> Object c b -> Bool
isOverlappingSnake offset snake node =
  (List.length <| List.filter (isOverlapping node) snake) > (0 + offset)

update : Computer -> Model -> Model
update computer model =
  let
    currentTime = now computer.time
    keyDirection = getDirectionFromKeyboard model.direction computer.keyboard
    newDirection =
      if model.lastPosition /= (getHeadNode model.snake).position then
        keyDirection
      else
        model.direction

    newLastPosition =
      if model.direction /= newDirection then
        (getHeadNode model.snake).position
      else
        model.lastPosition
  in
    if isOverlappingSnake 1 model.snake (getHeadNode model.snake) then
      initModel
    else
      if currentTime - model.time < 200 then
        { model
        | direction = newDirection
        , lastPosition = newLastPosition
        , score = if isOverlapping (getHeadNode model.snake) model.apple then model.score + 1 else model.score
        , rands = if List.length model.rands < 2 then modBy 20 currentTime :: model.rands else model.rands
        , snake = if isOverlapping (getHeadNode model.snake) model.apple then model.snake ++ [ getTailNode model.snake ] else model.snake
        }
      else
        { model
        | time = currentTime
        , direction = newDirection
        , lastPosition = newLastPosition
        , snake = updateSnake model.direction model.snake
        }



-- █   █  ███  █████ █   █
-- █   █   █   █     █   █
-- █   █   █   ███   █ █ █
--  █ █    █   █     █ █ █
--   █    ███  █████  █ █

viewObject : Color -> Object a Number -> Shape
viewObject color node =
  let
    nodeSize = 30
  in
    square color nodeSize
      |> move (node.position.x * nodeSize) (node.position.y * nodeSize)

view : Computer -> Model -> List Shape
view _ model =
  viewObject red model.apple :: (words black (fromInt model.score) :: List.map (viewObject black) model.snake)
