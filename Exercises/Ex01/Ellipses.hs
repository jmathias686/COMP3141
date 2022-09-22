module Ellipses where



import Codec.Picture



import ShapeGraphics


simpleEllipsePic :: Float -> Picture
simpleEllipsePic n = map greenEllipse [0, pi/n..(n-1)*pi/n]
  where
    greenEllipse :: Float -> PictureObject
    greenEllipse angle = Ellipse (Point 400 400) 250 70 angle (myGreen angle) Solid SolidFill
    myGreen angle = Colour (round (angle * 255 / pi)) 126 (255 - round (angle * 255 / pi)) 80


writeToFile pic
    = writePng "output.png"
           (drawPicture 3 pic)
