import Text (..)
import Signal
import Mouse
import Time (..)

--(\a -> asText a)
main = Signal.map asText delta

clickCount : Signal Int
clickCount =
    Signal.foldp (\click total -> total + 1) 0 Mouse.clicks

timeSoFar : Signal Time
timeSoFar =
    Signal.foldp (+) 0 (fps 30)

delta = Signal.map (\t -> t/20) (fps 30)

-- foldp : (a -> state -> state) -> state -> Signal a -> Signal state
