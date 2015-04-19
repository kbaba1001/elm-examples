import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Signal
import Text (..)
import Time (..)
import Window

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

userInput : Signal UserInput
userInput =
  Signal.map3 UserInput
    Keyboard.space
    (Signal.map .y Keyboard.wasd)
    (Signal.map .y Keyboard.arrows)

--type alias Input =
--    { space : Bool
--    , dir1 : Int
--    , dir2 : Int
--    , timeDelta : Float
--    }

type alias Input =
    { timeDelta : Float
    , userInput : UserInput
    }

type alias UserInput =
  { space : Bool
  , dir1 : Int
  , dir2 : Int
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

(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

type State = Play | Pause
type alias Ball =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  }
type alias Player =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , score : Int
  }

type alias GameState =
  { state : State
  , ball : Ball
  , player1 : Player
  , player2 : Player
  }

player : Float -> Player
player x =
  { x = x
  , y = 0
  , vx = 0
  , vy = 0
  , score = 0
  }

defaultGame : GameState
defaultGame =
  { state = Pause
  , ball = { x=0, y=0, vx=200, vy=200 }
  , player1 = player (20 - halfWidth)
  , player2 = player (halfWidth - 20)
  }


{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

stepGame : Input -> GameState -> GameState
stepGame {timeDelta,userInput} ({state,ball,player1,player2} as gameState) =
  { gameState |
      player1 <- updatePlayer timeDelta userInput.dir1 player1
  }

updatePlayer timeDelta dir player =
  let player1 = physicsUpdate timeDelta { player | vy <- toFloat dir * 200 }
  in
    { player1 |
        y <- clamp (22-halfHeight) (halfHeight-22) player1.y
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
display (w,h) ({state,ball,player1,player2} as gameState) =
  container w h middle <|
    collage gameWidth gameHeight
      [ rect gameWidth gameHeight
          |> filled green
      , rect 10 40
          |> make player1
      , toForm (asText gameState)
      ]

make obj shape =
  shape
    |> filled white
    |> move (obj.x, obj.y)


{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and shows it on screen.

------------------------------------------------------------------------------}

delta : Signal Float
delta =
  Signal.map inSeconds (fps 35)


input : Signal Input
input =
    Signal.sampleOn delta (Signal.map2 Input delta userInput)


gameState : Signal GameState
gameState =
    Signal.foldp stepGame defaultGame input


main : Signal Element
main =
    Signal.map2 display Window.dimensions gameState
