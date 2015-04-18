import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Signal
import Time
import Window

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type alias UserInput = { space : Bool, dir : Int }

userInput : Signal UserInput
userInput =
  Signal.map2 UserInput
    Keyboard.space
    (Signal.map .x Keyboard.arrows)

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

type State = Play | Pause

type alias Player =
  { x:Float, y:Float, vx:Float, vy:Float }

type alias GameState =
  { state:State
  , player:Player
  }

defaultGame : GameState
defaultGame =
  { state = Pause
  , player =
      { x = 0
      , y = 20 - halfHeight
      , vx = 0
      , vy = 0
      }
  }


{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

stepGame : Input -> GameState -> GameState
stepGame {timeDelta,userInput} ({state,player} as game) =
  { game |
      player <- updatePlayer timeDelta userInput player
  }

--space : Bool, dir : Int
--updatePlayer : Time ->
--dir
updatePlayer deltaTime userInput player =
  let player = physicsUpdate deltaTime { player | vx <- toFloat userInput.dir }
  in
    { player |
        x <- player.x
    }

--clamp (22-halfWidth) (halfWidth-22) player.x

--updatePlayer t dir points player =
--  let player1 = physicsUpdate  t { player | vy <- toFloat dir * 200 }
--  in
--      { player1 |
--          y <- clamp (22-halfHeight) (halfHeight-22) player1.y,
--          score <- player.score + points
--      }

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
display (w,h) {state,player} =
  container w h middle <|
    collage gameWidth gameHeight
      [ rect gameWidth gameHeight
          |> filled green
      , rect 40 10
          |> make player
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
    Time.fps 30


input : Signal Input
input =
    Signal.sampleOn delta (Signal.map2 Input delta userInput)


gameState : Signal GameState
gameState =
    Signal.foldp stepGame defaultGame input


main : Signal Element
main =
    Signal.map2 display Window.dimensions gameState
