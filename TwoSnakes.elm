import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Signal (Signal, map, map2, foldp, sampleOn)
import Text (asText)
import Time (fps)
import Window
import List
import List (take, member, (::), append)

-- MODEL

type SnakeState = Alive
                | Dead

type alias Coordinates = {x : Int, y : Int}

type alias SnakeModel = {i : Int, h : Coordinates, t : List Coordinates, s : SnakeState, d : Direction}

type GameState = Running
               | Snake1Won
               | Snake2Won

type alias Model = {s1 : SnakeModel, s2 : SnakeModel, gs: GameState}

crossMap : (SnakeModel -> SnakeModel -> SnakeModel) -> Model -> Model
crossMap f ({s1, s2} as m) = {m |s1 <- f s1 s2
                                ,s2 <- f s2 s1
                             } 

type alias GameSize = Coordinates

gameSize : GameSize
gameSize = {x = 24, y = 12}

-- UPDATE

type alias Directions = {d1 : Direction, d2 : Direction} 

moveHead : Coordinates -> Direction -> Coordinates
moveHead c d = if | d == left  -> {c | x <- c.x-1}
                  | d == up    -> {c | y <- c.y+1}
                  | d == right -> {c | x <- c.x+1}
                  | d == down  -> {c | y <- c.y-1}

lengthRatio : Int
lengthRatio = 5

applyMove :SnakeModel -> SnakeModel
applyMove ({i, h, t, s, d} as m) = if | s == Dead -> m
                                      | otherwise -> { m | h <- moveHead h d, t <- take (i // lengthRatio) (h :: t), i <- i + 1}
  
checkDeath : GameSize -> SnakeModel -> SnakeModel -> SnakeModel
checkDeath gs sm other = if | sm.h.x > gs.x|| sm.h.x < 0 - gs.x || sm.h.y > gs.y || sm.h.y < 0 - gs.y || member sm.h (other.h :: (append other.t sm.t)) -> {sm | s <- Dead}
                            | otherwise -> sm

moveSnake : Direction -> SnakeModel -> SnakeModel
moveSnake d sm = applyMove <| {sm | d <- d}

update : Directions -> Model -> Model
update {d1, d2} ({s1, s2, gs} as m) = crossMap (checkDeath gameSize)
                                      <| {m |s1 <- moveSnake d1 s1
                                            ,s2 <- moveSnake d2 s2
                                         }  

-- VIEW

scale : Int
scale = 15

display : Color -> Int -> Int -> Shape -> Form
display c x y shape = move (toFloat (x * scale), toFloat (y * scale)) (filled c shape)

scaledGameSize : Coordinates
scaledGameSize = {x = scale * (gameSize.x * 2 + 1) + 4
                 ,y = scale * (gameSize.y * 2 + 1) + 4
                 }

deathText : Form
deathText = toForm <| asText "Game over. Reload page"

court : Form
court = filled gray <| rect (toFloat scaledGameSize.x) (toFloat scaledGameSize.y)

headBall : Shape
headBall = oval (toFloat scale) (toFloat scale)

tailBall : Shape
tailBall = oval (toFloat scale * 0.5) (toFloat scale * 0.5)

ballList : Color -> SnakeModel -> List Form
ballList c m = let head = (display c m.h.x m.h.y headBall)
                   tail = (List.map (\e -> display c e.x e.y tailBall) m.t)
               in head :: tail

view : (Int, Int) -> Model -> Element
view (w, h) m = container w h middle 
                  <| collage scaledGameSize.x scaledGameSize.y 
                  <| court :: (append (ballList blue m.s1) (ballList red m.s2)) 
-- SIGNALS

updateDirection1 : Int -> Direction -> Direction
updateDirection1 key d = if | key == 65 -> left
                            | key == 87 -> up
                            | key == 68 -> right
                            | key == 83 -> down
                            | otherwise -> d

updateDirection2 : Int -> Direction -> Direction 
updateDirection2 key d = if | key ==  37 -> left
                            | key ==  38 -> up
                            | key ==  39 -> right
                            | key ==  40 -> down
                            | otherwise  -> d
                          

updateDirections : Int -> Directions -> Directions
updateDirections key ({d1, d2} as ds) = {ds |d1 <- updateDirection1 key d1
                                            ,d2 <- updateDirection2 key d2
                                        }

initDirections : Directions
initDirections = {d1 = left, d2 = right}

directions : Signal Directions
directions = foldp updateDirections initDirections Keyboard.lastPressed

initSnakeModel : Int -> Direction -> SnakeModel
initSnakeModel x d = {i = 0, s = Alive, d = d, h = {x = x, y = 0}, t = []}

init : Model
init = {s1 = initSnakeModel -1 left, s2 = initSnakeModel 1 right, gs = Running}

model : Signal Model
model = foldp update init (sampleOn (fps 2) directions) 

-- MAIN

main : Signal Element
main = map2 view Window.dimensions model

