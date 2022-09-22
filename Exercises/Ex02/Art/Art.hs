module Art where  

import ShapeGraphics
import Codec.Picture


-- call art2, providing depth of recursion, scale factor (1 is base),
--     primary colour, rotation in degrees, and what kind of picture (0-3 ``1 is optimal``)
--     and if there is an outline (will change appearance for some)


art :: Picture
art = art2 4 2 magenta 45 1 False 



art2 :: Int -> Float -> Colour -> Float -> Int -> Bool -> Picture
art2 depth scale' colour rotation state outline =
  background : cube depth (Point 400 400) (scale scale' (Vector 100 0)) colour (rotation * pi/180) state outline
  where
    background = Polygon [(Point 0 0), (Point 0 800), (Point 800 800), (Point 800 0)] (neg colour) Solid SolidFill

    neg :: Colour -> Colour
    neg (Colour r g b o) =
      Colour (255-r) (255-g) (255-b) o




-- given depth, base of recursion, length of each cube's edge, primary colour, rotation in rads and the type of image
cube :: Int -> Point -> Vector -> Colour -> Float -> Int -> Bool -> Picture
cube depth base l colour rotation state isOutline
  | depth == 0 = [s1, s2, s3, outline]
  | otherwise =
    case state of
    0 -> [s1, s2, s3, outline]
    1 ->
      -- before this iteration's pic to hide  specific faces to appear like they are behind
      cube (depth - 1) p2 (scale 0.5 l) colour rotation state isOutline
      ++ cube (depth - 1) p4 (scale 0.5 l) colour rotation state isOutline
      ++ cube (depth - 1) p6 (scale 0.5 l) colour rotation state isOutline
      -- Current Iteration
      ++ [s1, s2, s3, outline]
      -- after iteration to appear in front of
      ++ cube (depth - 1) p3 (scale 0.5 l) colour rotation state isOutline
      ++ cube (depth - 1) p5 (scale 0.5 l) colour rotation state isOutline
      ++ cube (depth - 1) p7 (scale 0.5 l) colour rotation state isOutline

    2 ->
      -- Only show going backwards
      cube (depth - 1) p2 (scale 0.5 l) colour rotation state isOutline
      ++ cube (depth - 1) p4 (scale 0.5 l) colour rotation state isOutline
      ++ cube (depth - 1) p6 (scale 0.5 l) colour rotation state isOutline
      ++ [s1, s2, s3, outline]

    3 ->
      -- Only show going forwards
      [s1, s2, s3, outline]
      ++ cube (depth - 1) p3 (scale 0.5 l) colour rotation state isOutline
      ++ cube (depth - 1) p5 (scale 0.5 l) colour rotation state isOutline
      ++ cube (depth - 1) p7 (scale 0.5 l) colour rotation state isOutline

  
  where
    -- Three visible sides of the cube
    s1 = Polygon [p1, p2, p3, p4] colour3 Solid SolidFill
    s2 = Polygon [p1, p4, p5, p6] colour Solid SolidFill
    s3 = Polygon [p1, p2, p7, p6] colour2 Solid SolidFill

    -- Outline of the cubes, to distinguish
    outline
      | isOutline = Path [p1, p2, p3, p4, p1, p6, p5, p4, p1, p6, p7, p2] black Solid
      | otherwise = Path [Point 0 0] black Solid
    

    -- All 7 visible corners of the cube
    p1 = base
    p2 = movePoint p1 (rotateVector (-5*pi/6 + rotation) l)
    p3 = movePoint p2 (rotateVector (pi/2 + rotation) l)
    p4 = movePoint p3 (rotateVector (pi/6 + rotation) l)
    p5 = movePoint p4 (rotateVector (-pi/6 + rotation) l)
    p6 = movePoint p5 (rotateVector (-pi/2 + rotation) l)
    p7 = movePoint p6 (rotateVector (-5*pi/6 + rotation) l)

    -- Uses mixing to get brighter variants of the primary colour
    colour2 = mix colour white
    colour3 = mix (mix colour white) white


mix :: Colour -> Colour -> Colour
mix (Colour r1 g1 b1 o1) (Colour r2 g2 b2 o2)
  = Colour ((r1+r2) `div` 2) ((g1+g2) `div` 2) ((b1+b2) `div` 2) ((o1+o2)`div` 2)





tree :: Int -> Point -> Vector -> Colour -> Picture
tree depth base direction colour
  | depth == 0 = drawLine line
  | otherwise
    =  drawLine line
    ++ tree (depth - 1) nextBase left nextColour -- left tree
    ++ tree (depth - 1) nextBase right nextColour -- right tree
  where
    drawLine :: Line -> Picture
    drawLine (Line start end) =
      [ Path [start, end] colour Solid ]

    line = Line base nextBase
    nextBase = offset direction base

    left = rotateVector (-pi /12) $ scale 0.8 $ direction
    right = rotateVector (pi /12) $ scale 0.8 $ direction

    nextColour =
      colour { redC = (redC colour) - 24, blueC = (blueC colour) + 24 }




-- Offset a point by a vector
offset :: Vector -> Point -> Point
offset (Vector vx vy) (Point px py)
  = Point (px + vx) (py + vy)

-- Scale a vector
scale :: Float -> Vector -> Vector
scale factor (Vector x y) = Vector (factor * x) (factor * y)

-- Rotate a vector (in radians)
rotateVector :: Float -> Vector -> Vector
rotateVector alpha (Vector vx vy)
  = Vector (cos alpha * vx - sin alpha * vy)
           (sin alpha * vx + cos alpha * vy)


movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector xv yv)
  = Point (x + xv) (y + yv)

-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]


writeToFile pic
  = writePng "art.png" (drawPicture 3 pic)