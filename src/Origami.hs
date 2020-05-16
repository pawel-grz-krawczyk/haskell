module Origami(rectangle, circle, foldMany) where

type Point = (Float, Float)
type OrigamiSheet = Point -> Int

type FoldingDirection = Ordering
data Line = HorizontalLine {y :: Float} | VerticalLine {x :: Float} | LinearFunction { a :: Float, b :: Float}

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
foldOnce :: (Point, Point) -> OrigamiSheet -> OrigamiSheet
foldOnce points = foldOnce' direction line
  where
    (line, direction) = lineAndDirection points

foldOnce' :: Ordering -> Line -> OrigamiSheet -> Point -> Int
foldOnce' direction line sheet p@(a, b) = fold direction (pointPosition line p)
  where
    fold GT GT = addSheetsLayers
    fold LT LT = addSheetsLayers
    fold _ EQ = sheet p
    fold _ _ = 0
    addSheetsLayers = sheet p + sheet (mirrorPoint line p)


mirrorPoint :: Line -> Point  -> Point
mirrorPoint HorizontalLine {y=y} (x1, y1) = (x1, y1 + 2 * (y - y1))
mirrorPoint VerticalLine {x=x} (x1, y1) = (x1 + 2 * (x - x1), y1)
mirrorPoint LinearFunction {a=a, b=b} (x, y) = (mirrorX, mirrorY)
  where
    a2 = -1 * a
    b2 = y - a2 * x
    interX = (b2 - b) / (a - a2)
    interY = a * interX + b
    mirrorX = x + 2 * (interX - x)
    mirrorY = y + 2 * (interY - y)


pointPosition :: Line -> Point -> FoldingDirection
pointPosition HorizontalLine {y=y} (_, y1) = compare y1 y
pointPosition VerticalLine {x=x} (x1, _) = compare x1 x
pointPosition LinearFunction {a=a, b=b} (x, y) = compare y (a * x + b)


lineAndDirection:: (Point, Point) -> (Line, FoldingDirection)
lineAndDirection line@((x1, y1), (x2, y2))
  | x1 == x2 = (VerticalLine {x=x1}, compare y1 y2)
  | y1 == y2 = (HorizontalLine {y=y1}, compare x2 x1)
  | otherwise = (LinearFunction {a=a, b=b}, compare x2 x1)
      where
        a = (y2 - y1) / (x2 - x1)
        b = y1 - a * x1


foldMany :: [(Point, Point)] -> OrigamiSheet -> OrigamiSheet
foldMany lines sheet = foldl (flip foldOnce) sheet $ filter properLine lines


properLine (p1, p2)
  | p1 == p2 = False
  | otherwise = True