import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)
import Text (..)

main =
  collage 1200 450
  [ move (-400, -210) (toForm (asText "aaa"))
  , move (-400, 0) (toForm (fittedImage 400 400 "http://mensfashion.cc/matome/resources/upload/titleimage/190.jpg"))
  , move (   0, -210) (toForm (asText "bbb"))
  , move (   0, 0) (toForm (fittedImage 400 400 "http://livedoor.blogimg.jp/h2111/imgs/3/2/3245b160.jpg"))
  , move ( 400, -210) (toForm (asText "ccc"))
  , move ( 400, 0) (toForm (fittedImage 400 400 "http://blog-imgs-44.fc2.com/s/t/r/strengersred/monhan.jpg"))
  ]
