import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Signal (Signal, map, map2, foldp, sampleOn)
import Text (asText)
import Time (fps)
import Window
import List
import List (take, member, (::))

-- MODEL

type State = Alive
           | Dead

type alias Coordinates = {x : Int, y : Int}

type alias Model = {i : Int, h : Coordinates, t : List Coordinates, s : State, d : Direction}

type alias GameSize = Coordinates

gameSize : GameSize
gameSize = {x = 12, y = 6}

-- UPDATE

type Action = KeyPressed Int

moveHead : Coordinates -> Direction -> Coordinates
moveHead c d = if | d == left  -> {c | x <- c.x-1}
                  | d == up    -> {c | y <- c.y+1}
                  | d == right -> {c | x <- c.x+1}
                  | d == down  -> {c | y <- c.y-1}

lengthRatio : Int
lengthRatio = 5

applyMove : Model -> Model
applyMove ({i, h, t, s, d} as m) = if | s == Dead -> m
                                      | otherwise -> { m | h <- moveHead h d, t <- take (i // lengthRatio) (h :: t), i <- i + 1}
  
checkDeath : GameSize -> Model -> Model
checkDeath gs m = if | m.h.x > gs.x|| m.h.x < 0 - gs.x || m.h.y > gs.y || m.h.y < 0 - gs.y || member m.h m.t -> {m | s <- Dead}
                     | otherwise -> m

updateDirection : Int -> Model -> Model
updateDirection key m = if | key ==  37 -> { m | d <- left}
                           | key ==  38 -> { m | d <- up}
                           | key ==  39 -> { m | d <- right}
                           | key ==  40 -> { m | d <- down}
                           | otherwise  -> m

update : Action -> Model -> Model
update a m = case a of
               KeyPressed key -> checkDeath gameSize <| applyMove <| updateDirection key m

-- VIEW

scale : Int
scale = 15

display : Int -> Int -> Shape -> Form
display x y shape = move (toFloat (x * scale), toFloat (y * scale)) (filled black shape)

headBall : Shape
headBall = oval (toFloat scale) (toFloat scale)

tailBall : Shape
tailBall = oval (toFloat scale * 0.5) (toFloat scale * 0.5)

scaledGameSize : Coordinates
scaledGameSize = {x = scale * (gameSize.x * 2 + 1) + 4
                 ,y = scale * (gameSize.y * 2 + 1) + 4
                 }

deathText : Form
deathText = toForm <| asText "Game over. Reload page"

court : Form
court = filled gray <| rect (toFloat scaledGameSize.x) (toFloat scaledGameSize.y)

ballList : Model -> List Form
ballList m = let head = (display m.h.x m.h.y headBall)
                 tail = (List.map (\c -> display c.x c.y tailBall) m.t)
             in head :: tail

view : (Int, Int) -> Model -> Element
view (w, h) m = container w h middle 
                  <| collage scaledGameSize.x scaledGameSize.y 
                  <| court :: if | m.s == Alive -> ballList m 
                                 | m.s == Dead  -> [ deathText ]

-- SIGNALS

keyPressed : Signal Action
keyPressed = map KeyPressed Keyboard.lastPressed

init : Model
init = {i = 0, s = Alive, d = up, h = {x = 0, y = 0}, t = []}

model : Signal Model
model = foldp update init (sampleOn (fps 2) keyPressed) 

-- MAIN

main : Signal Element
main = map2 view Window.dimensions model

