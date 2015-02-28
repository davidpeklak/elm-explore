import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Signal (Signal, map, map2, merge, foldp, sampleOn)
import Text (asText)
import Time (Time, fps, inSeconds)
import Window
import List
import List (take, member, (::), append)

-- MODEL

type alias Coordinates = {x : Int, y : Int}

type SnakeState = Alive
                | Dead

type alias SnakeModel = {i : Int, h : Coordinates, t : List Coordinates, s : SnakeState, d : Direction}

type alias RunningModel = {s1 : SnakeModel, s2 : SnakeModel, tick : Time}

type Model = Running RunningModel
           | Snake1Won
           | Snake2Won
           | Draw

forSnakes : (SnakeModel -> SnakeModel) -> RunningModel -> RunningModel
forSnakes f ({s1, s2} as rm) = {rm |s1 <- f s1
                                   ,s2 <- f s2
                               }

crossMap : (SnakeModel -> SnakeModel -> SnakeModel) -> RunningModel -> RunningModel
crossMap f ({s1, s2} as rm) = {rm |s1 <- f s1 s2
                                  ,s2 <- f s2 s1
                              } 

type alias GameSize = Coordinates

gameSize : GameSize
gameSize = {x = 24, y = 12}

speed : Int
speed = 3

step : Float
step = 1.0 / (toFloat speed)

-- UPDATE

type Action = Direction1 Direction
            | Direction2 Direction
            | Restart
            | Tick Time 

moveHead : Coordinates -> Direction -> Coordinates
moveHead c d = if | d == left  -> {c | x <- c.x-1}
                  | d == up    -> {c | y <- c.y+1}
                  | d == right -> {c | x <- c.x+1}
                  | d == down  -> {c | y <- c.y-1}

lengthRatio : Int
lengthRatio = 2

applyMove : SnakeModel -> SnakeModel
applyMove ({i, h, t, s, d} as m) = if | s == Dead -> m
                                      | otherwise -> { m | h <- moveHead h d, t <- take (i // lengthRatio) (h :: t), i <- i + 1}
  
checkDeath : GameSize -> SnakeModel -> SnakeModel -> SnakeModel
checkDeath gs sm other = if | sm.h.x > gs.x|| sm.h.x < 0 - gs.x || sm.h.y > gs.y || sm.h.y < 0 - gs.y || member sm.h (other.h :: (append other.t sm.t)) -> {sm | s <- Dead}
                            | otherwise -> sm

moveSnake : Direction -> SnakeModel -> SnakeModel
moveSnake d sm = applyMove <| {sm | d <- d}

checkWon : RunningModel -> Model
checkWon ({s1, s2} as rm) = if | s1.s == Alive && s2.s == Dead  -> Snake1Won
                               | s1.s == Dead  && s2.s == Alive -> Snake2Won
                               | s1.s == Dead  && s2.s == Dead  -> Draw
                               | otherwise -> Running rm                                 

updateRunning : Action -> RunningModel -> RunningModel
updateRunning a ({s1, s2, tick} as m) = case a of
        Direction1 d1 -> {m |s1 <- {s1 |d <- d1}}
        Direction2 d2 -> {m |s2 <- {s2 |d <- d2}}
        Tick t -> let sum = tick + (inSeconds t) in
                  if | sum < step -> {m |tick <- sum} 
                     | otherwise  -> forSnakes applyMove {m |tick <- sum - step}
        _ -> m

update : Action -> Model -> Model
update a m = case m of
               Running rm -> checkWon <| crossMap (checkDeath gameSize) <| updateRunning a rm
               _ -> case a of 
                      Restart -> init
                      _ -> m

-- VIEW

scale : Int
scale = 15

display : Color -> Int -> Int -> Shape -> Form
display c x y shape = move (toFloat (x * scale), toFloat (y * scale)) (filled c shape)

scaledGameSize : Coordinates
scaledGameSize = {x = scale * (gameSize.x * 2 + 1) + 4
                 ,y = scale * (gameSize.y * 2 + 1) + 4
                 }

court : Form
court = filled gray <| rect (toFloat scaledGameSize.x) (toFloat scaledGameSize.y)

headBall : Shape
headBall = oval (toFloat scale) (toFloat scale)

tailBall : Shape
tailBall = oval (toFloat scale * 0.5) (toFloat scale * 0.5)

winBall : Shape
winBall = oval (toFloat scale * 5.0) (toFloat scale * 5.0)

ballList : Color -> SnakeModel -> List Form
ballList c m = let head = (display c m.h.x m.h.y headBall)
                   tail = (List.map (\e -> display c e.x e.y tailBall) m.t)
               in head :: tail

onCourt : Model -> List Form
onCourt m = case m of
              Running rm -> append (ballList blue rm.s1) (ballList red rm.s2)
              Snake1Won  -> [display blue -6 0 winBall]
              Snake2Won  -> [display red   6 0 winBall]
              Draw       -> [display blue -6 0 winBall, display red 6 0 winBall]

view : (Int, Int) -> Model -> Element
view (w, h) m = container w h middle 
                  <| collage scaledGameSize.x scaledGameSize.y 
                  <| court :: (onCourt m)
-- SIGNALS

keyToMaybeAction : Int -> Maybe Action
keyToMaybeAction key = if | key == 65 -> Just <| Direction1 left
                          | key == 87 -> Just <| Direction1 up
                          | key == 68 -> Just <| Direction1 right
                          | key == 83 -> Just <| Direction1 down
                          | key == 37 -> Just <| Direction2 left
                          | key == 38 -> Just <| Direction2 up
                          | key == 39 -> Just <| Direction2 right
                          | key == 40 -> Just <| Direction2 down
                          | key == 32 -> Just Restart
                          | otherwise -> Nothing

collect : Maybe Action -> Action -> Action
collect ma a = case ma of
                 Just na -> na
                 Nothing -> a

keyAction : Signal Action
keyAction = foldp collect (Tick 0) <| map keyToMaybeAction Keyboard.lastPressed

timeAction : Signal Action
timeAction = map (\t -> Tick t) <| fps 15

action : Signal Action
action = merge keyAction timeAction

initSnakeModel : Int -> Direction -> SnakeModel
initSnakeModel x d = {i = 0, s = Alive, d = d, h = {x = x, y = 0}, t = []}

init : Model
init = Running {s1 = initSnakeModel -1 left, s2 = initSnakeModel 1 right, tick = 0}

model : Signal Model
model = foldp update init action

-- MAIN

main : Signal Element
main = map2 view Window.dimensions model

