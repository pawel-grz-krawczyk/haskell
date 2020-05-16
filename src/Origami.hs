module Origami(rectangle, circle, foldMany) where

 -- r = rectangle (0, 0) (10, 10)
 -- fr = foldOnce ((0, 5), (1, 5)) r

type Point = (Float, Float)
type OrigamiSheet = Point -> Int

type Line = (Point, Point)

data FoldingDirection = L | R

rectangle :: Point -> Point -> OrigamiSheet
rectangle (x1, y1) (x2, y2) = layers
  where
    layers (a, b) =
      if a >= x1 && a <= x2 && b >= y1 && b <= y2
        then 1
        else 0

circle :: Point -> Float -> OrigamiSheet
circle (x, y) r = layers
  where
    layers (a, b) = if distance <= r then 1 else 0 where
        distance = sqrt ((a - x)^2 + (b - y)^2)

-- Folds origami sheet given two points p1 and p2
-- Paper on the right side of the line (in p1 -> p2 direction) is folded to the left side of the line
-- The result of folding is following:
-- - points on the right side of the line have 0 layers
-- - points on the line have the same number of layers as the sheet before folding
-- - points on the left side of the line have the same number of layers as the sheet before folding + layers of
--   folded are of sheet
foldOnce :: Line -> OrigamiSheet -> OrigamiSheet
foldOnce  = foldOnce' L -- TODO compute direction

foldOnce' :: FoldingDirection -> Line -> OrigamiSheet -> Point -> Int
foldOnce' direction line@((x1, y1), (x2, y2)) sheet p@(a, b) = fold direction (pointPosition line p) where
  fold L GT = addFoldings
  fold R LT = addFoldings
  fold _ EQ = sheet p
  fold _ _ = 0
  addFoldings = sheet p + sheet (mirrorPoint line p)


mirrorPoint :: Line -> Point  -> Point
mirrorPoint line@((x1, y1), (x2, y2)) p@(x3, y3) =
  if y1 == y2 then (x3, y3 + 2 * (y1 - y3)) else mirrorPoint' line p where

    mirrorPoint' ((x1, y1), (x2, y2)) (x3, y3) = (mirrorX, mirrorY) where
        a1 = (y2 - y1) / (x2 - x1)
        b1 = y1 - a1 * x1
        a2 = -1 * a1
        b2 = y3 - a2 * x3
        interX = (b2 - b1) / (a1 - a2)
        interY = a1 * interX + b1
        mirrorX = x3 + 2 * (interX - x3)
        mirrorY = y3 + 2 * (interY - y3)

pointPosition :: Line -> Point -> Ordering -- GT = point is left of the line
pointPosition line@((x1, _), (x2, _)) =
  if x1 == x2
    then pointPositionVerticalLine line
    else pointPositionProperLine line

pointPositionVerticalLine :: Line -> Point -> Ordering
pointPositionVerticalLine ((x1, y1), (_, y2)) (x3, y3) =
  if y1 <= y2
    then compare x1 x3
    else compare x3 x1

pointPositionProperLine :: Line -> Point -> Ordering -- GT = point is left of the line
pointPositionProperLine ((x1, y1), (x2, y2)) (x3, y3) = compare y3 (a * x3 + b) where
    a = (y2 - y1) / (x2 - x1)
    b = y1 - a * x1

foldMany :: [Line] -> OrigamiSheet -> OrigamiSheet
foldMany lines sheet = foldl (flip foldOnce) sheet lines
