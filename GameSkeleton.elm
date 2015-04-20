import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Signal
import Time (..)
import Window
import Text

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type alias UserInput = { dir : Int }

userInput : Signal UserInput
userInput = Signal.map UserInput (Signal.map .x Keyboard.arrows)

type alias Input =
    { timeDelta : Float
    , userInput : UserInput
    }

{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

Tasks: Redefine `GameState` to represent your particular game.
       Redefine `defaultGame` to represent your initial game state.

For example, if you want to represent many objects that just have a position,
your GameState might just be a list of coordinates and your default game might
be an empty list (no objects at the start):

    type GameState = { objects : [(Float,Float)] }
    defaultGame = { objects = [] }

------------------------------------------------------------------------------}

(gameWidth,gameHeight) = (400, 600)
(halfWidth,halfHeight) = (200, 300)

type alias Ball =
  { x:Float, y:Float, vx:Float, vy:Float }

type alias Player =
  { x:Float, y:Float, vx:Float, vy:Float }

type alias Block =
  { x:Float, y:Float, vx:Float, vy:Float, visible: Bool }

type alias GameState =
  { ball : Ball
  , player : Player
  , block1 : Block
  }

defaultGame : GameState
defaultGame =
  { ball =
      { x = 0
      , y = 0
      , vx = 200
      , vy = 200
      }
  , player =
      { x = 0
      , y = 20 - halfHeight
      , vx = 0
      , vy = 0
      }
  , block1 =
      { x = -20
      , y = halfWidth - 40
      , vx = 0
      , vy = 0
      , visible = True
      }
  }


{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

stepGame : Input -> GameState -> GameState
stepGame {timeDelta,userInput} ({ball,player,block1} as game) =
  { game |
      ball <- updateBall timeDelta ball player block1,
      player <- updatePlayer timeDelta userInput.dir player,
      block1 <- block1
  }

updateBall deltaTime ({x,y,vx,vy} as ball) player block1 =
  physicsUpdate deltaTime
    { ball |
        vx <- stepV vx (x < 7 - halfWidth) (x > halfWidth - 7),
        vy <- stepV vy ((ball `within` player) || (ball `within` block1)) ((y > halfHeight - 7) || (ball `within` block1))
    }

near k c n =
    n >= k-c && n <= k+c

within ball paddle =
    near paddle.x 20 ball.x && near paddle.y 10 ball.y

stepV v lowerCollision upperCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - abs v
     | otherwise      -> v

updatePlayer deltaTime dir player =
  let player1 = physicsUpdate deltaTime { player | vx <- toFloat dir * 200 }
  in
    { player1 |
        x <- clamp (22-halfWidth) (halfWidth-22) player1.x
    }

physicsUpdate t ({x,y,vx,vy} as obj) =
  { obj |
      x <- x + vx * t,
      y <- y + vy * t
  }

{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

Task: redefine `display` to use the GameState you defined in part 2.

------------------------------------------------------------------------------}

display : (Int,Int) -> GameState -> Element
display (w,h) ({ball,player,block1} as gameState) =
  container w h middle <|
    collage 1000 1000
      [ rect gameWidth gameHeight
          |> filled green
      , oval 15 15
            |> make ball
      , rect 40 10
          |> make player
      , showBlock block1
      --, toForm (asText gameState)
      --    |> move (0.0, halfHeight+20)
      ]


make obj shape =
  shape
    |> filled white
    |> move (obj.x, obj.y)

showBlock block =
  if block.visible == True then
    rect 40 10
      |> make block
  else
    toForm empty

{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and shows it on screen.

------------------------------------------------------------------------------}

delta : Signal Float
delta =
    Signal.map inSeconds (fps 30)


input : Signal Input
input =
    Signal.sampleOn delta (Signal.map2 Input delta userInput)


gameState : Signal GameState
gameState =
    Signal.foldp stepGame defaultGame input


main : Signal Element
main =
    Signal.map2 display Window.dimensions gameState
