module Slime where

import List
import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Signal
import Time (..)
import Window
import Keyboard

--these semicircles will form the physical rendering of the slime
semicircle : Float -> Shape
semicircle r =
  let n = 50
      t = 2 * pi / n
      hw = r/2
      hh = r/2
      f i = (hw * cos (t*i), hh * sin (t*i))
  in  List.map f [0..n-25]
  
--Signals
  
main =
 Signal.map2 view Window.dimensions
         (Signal.foldp upstate initState controller)

delta = Signal.map inSeconds (fps 30)

controller =
 Signal.sampleOn delta <|
  Signal.map5 Input
   (Signal.map .x Keyboard.wasd)
   (Signal.map .y Keyboard.wasd)
   (Signal.map .x Keyboard.arrows)
   (Signal.map .y Keyboard.arrows)
   delta

--Model

(gameWidth, gameHeight) = (1000,600)
   
type alias Input =
  {  x1 : Int
   , y1 : Int
   , x2 : Int
   , y2: Int
   , delta : Time
  }
  
type alias Ball = {x:Float, y:Float, vx:Float, vy:Float}
type alias Slime = {x:Float, y:Float, vx:Float, vy:Float, color:Color}
type alias State = {slime1:Slime, slime2:Slime, ball: Ball}




--initial state of the game
initState : State
initState =
  { slime1 =   { x=-300, y= 41 - (toFloat gameHeight / 2), vx=0, vy=0, color = lightGreen }
  , slime2 =   { x=300, y=41 - (toFloat gameHeight / 2), vx=0, vy=0,color = yellow }
  , ball   =  { x=-300, y=400 - (toFloat gameHeight / 2), vx=30, vy=-50 }
  }

slimeZero = 41 - (toFloat gameHeight / 2)
ballZero = 400 - (toFloat gameHeight / 2)

--Update

upstate : Input -> State -> State
upstate {x1, y1, x2, y2, delta} ({slime1, slime2, ball} as state) =
 if | ball.y < slimeZero -> initState
    | otherwise ->
            { state | slime1 <- moveSlime delta x1 y1 slime1,
                      slime2 <- moveSlime2 delta x2 y2 slime2,
                        ball <- moveBall delta ball slime1 slime2
            }
 
system t obj = { obj | x <- clamp -425 -75 (obj.x + t*obj.vx), y <- max slimeZero (obj.y + t*obj.vy) }
system2 t obj = { obj | x <- clamp 75 425  (obj.x + t*obj.vx), y <- max slimeZero (obj.y + t*obj.vy) }
jump y obj = if y > 0  && obj.y == slimeZero then {obj | vy <- 300} else obj
fall t obj = if obj.y  > slimeZero then {obj | vy <- obj.vy - 15} else obj
lateral x obj = {obj | vx <- toFloat x*200} 

--movement for slime 1 
movement t x y =
 jump y >> fall t >> lateral x >> system t
 
 --movement for slime 2
movement2 t x y =
 jump y >> fall t >> lateral x >> system2 t 
 
moveSlime t x y slime =
 movement t x y slime

moveSlime2 t x y slime =  
 movement2 t x y slime

--explicitly defines what "near enough" means for detecting collisions  
near l tol h = 
 h >= l - tol && h <= l + tol
 
--handles collisions between slimes and the ball  
moveBall : Time -> Ball -> Slime -> Slime -> Ball
moveBall t ({x,y,vx,vy} as ball) slime1 slime2 =
 if | near ball.x 3 0 && near ball.y 180 slimeZero ->
       physicsUpdate t
            { ball |
                vx <- -vx,
                vy <- hitY vy (hit ball slime1) (hit ball slime2) slime1.vy slime2.vy
            }
    | otherwise ->
        physicsUpdate t
            { ball |
                vx <- hitX vx (hit ball slime1) (hit ball slime2) slime1.vx slime2.vx,
                vy <- hitY vy (hit ball slime1) (hit ball slime2) slime1.vy slime2.vy
            }

physicsUpdate t ({x,y,vx,vy} as obj) =
 if obj.y  > slimeZero 
  then 
   {obj | vy <- obj.vy - 15,
          x <- x + vx * t,
          y <- y + vy * t}
   else
    { obj |
        x <- x + vx * t,
        y <- y + vy * t
    }
  
hit ball slime =
 (ball.x >= slime.x - 90 && ball.x <= slime.x + 90) && (ball.y >= slime.y - 80 && ball.y <= slime.y + 70)



hitY v col1 col2 v1 v2=
  if | col1 ->  min 385 (abs v1 + 355)
     | col2 -> min 385 (abs v2 + 355)
     | otherwise      -> v  

hitX v col1 col2 v1 v2=
  if | col1 ->  abs v1
     | col2 -> -(abs v2)
     | otherwise      -> v

 
--View
drawSlime slime = 
 filled slime.color (semicircle 150) |> move (slime.x, slime.y)

view (w, h) ({slime1, slime2, ball} as state) = 
 container w h middle <|
 let (w', h') = (gameWidth, gameHeight) in
  collage w' h' [ filled blue (rect (toFloat w') (toFloat h'))
                  , move (0,0 - (toFloat w')/3) (filled red (rect (toFloat w') (toFloat (h')/4)))
                  , drawSlime state.slime1
                  , drawSlime state.slime2
                  , filled white (circle 30) |> move (ball.x, ball.y)
                  , filled black (rect 10 150) |> move (0, -184)]
