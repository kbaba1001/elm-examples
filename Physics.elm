import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Signal
import Text (..)
import Time (..)

main = Signal.map2 view sunAndEarth bouncingBall

view figure1 figure2 = flow down [ intro, figure1, body, figure2, outro]

-- Text

intro = plainText """
# Physics: How does it work?

Today we are going to learn how to gravity.
"""
body = plainText "Now that we can gravity, let's see if we can do it with elasticity!"
outro = plainText "Now you know how to gravity with elasticity! Good work physics friend!"

-- Diagrams

time = Signal.map (inSeconds << fst) (timestamp (fps 40))

sunAndEarth = Signal.map sunAndEarthAt time

sunAndEarthAt angle =
  let earth = group [ filled lightBlue (circle 20), toForm (plainText "Earth") ]
      sun = group [ filled lightYellow (circle 35), toForm (plainText "Sun") ]
  in
    collage 300 200
      [ earth
          |> move (120 * cos angle, 80 * sin angle)
      , sun
          |> move (25, 0)
      ]

bouncingBall = Signal.map bouncingBallAt time

bouncingBallAt angle =
  let ball = filled red (circle 15)
      ground = filled green (rect 300 50)
  in
    collage 300 200
      [ ball
          |> move (0, abs (150 * sin angle) - 75)
      , ground
          |> move (0, -100)
      ]
