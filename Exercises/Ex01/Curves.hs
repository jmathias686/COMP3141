module Curves where


import Codec.Picture


import ShapeGraphics


curves :: Picture
curves = map (makeShape sin 100 red) xvals
      ++ map (makeShape cos 200 green) xvals
  where 
    xvals = [0,10..780]
        -- enumFromThenTo 0 10 780

    origin :: Point
    origin = Point 20 380

    moveBy :: (Float -> Float) -> Float -> Float -> Point
    moveBy func amp x = movePoint origin (Vector x $ func (pi * x/200) * amp)

    makeShape :: (Float -> Float) -> Float -> Colour -> Float -> PictureObject
    makeShape func amp colour x = Circle (moveBy func amp x) 10 colour Solid SolidFill


movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector dx dy)
    = Point (x + dx) (y + dy)


writeToFile pic
    = writePng "output2.png"
        (drawPicture 3 pic)
