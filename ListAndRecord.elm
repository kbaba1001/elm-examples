import Graphics.Collage (..)
import Graphics.Element (..)
import Text (..)
import List (..)

main =
  flow down
    [ asText (product' [1,2,3])  -- 6
    , asText (product' [4,4])  -- 16
    , asText (product' [])  -- 1
    , asText (squareAll' [1,2,3]) -- [1,4,9]
    , asText (dropTom ["Sue","Tom","Bill"]) -- ["Sue","Bill"]
    , asText (dropTom ["Sam","Peter"])  -- ["Sue","Bill"]
    ]

---- product ----

product' = foldl (*) 1

--product' list =
--  case list of
--    [] -> 1
--    (x::xs) -> x * product' xs

--product' list =
--  case list of
--    [] -> 1
--    _  -> head list * (product' << tail) list

---- squareAll ----

squareAll' x = map (\a -> a ^ 2) x

---- dropTom ----

dropTom x = filter (\name -> name /= "Tom") x
