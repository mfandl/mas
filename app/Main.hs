module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.Random

width :: Float
width = 640

height :: Float
height = 480

windowDisplay :: Display
windowDisplay = InWindow "Window" (truncate width, truncate height) (10, 10)

animationFunc :: Float -> Picture
animationFunc time = Circle $ 100 * sin (time)

type Model = (Float, Float, Float)

data World = World {
               elapsedTime :: Float,
               entities :: [Entity]
             } deriving (Show)

data Entity = Static Point EntityShape | Moving Point Point EntityShape
  deriving (Show)

data EntityShape = Dot Float | Rect Float Float
  deriving (Show)

class Renderable a where
  drawRenderable :: a -> Picture

instance Renderable EntityShape where
  drawRenderable (Dot size) = Circle size
  drawRenderable (Rect width height) = Polygon [
    (-width / 2, -height / 2),
    (width / 2, -height / 2),
    (width / 2, height / 2),
    (-width / 2, height / 2)]

instance Renderable Entity where
  drawRenderable (Static (x, y) shape) = translate x y $ drawRenderable shape
  drawRenderable (Moving (x, y) _ shape) = translate x y $ drawRenderable shape

class Updatable a where
  update :: Float -> a -> a

instance Updatable Entity where
  update dt (Moving (x, y) (tx, ty) shape) =
    Moving (x + tx * dt, y + ty * dt) (tx, ty) shape
  update _ e = e

instance Updatable World where
  update dt (World { entities = es, elapsedTime = et }) =
    World { elapsedTime = et + dt, entities = (update dt) <$> es }

count :: Int
count = 10000

main :: IO ()
main = do
  g <- getStdGen
  xs <- pure $ (* width) <$> (take count (randoms g :: [Float]))
  ys <- pure $ (* height) <$> ((take count) . (drop count)) (randoms g :: [Float])
  pos <- pure $ zip xs ys
  types <- pure $ ((take count) . (drop (2 * count))) (randoms g :: [Float])
  vxs <- pure $ ((* 500) . (0.5-)) <$> ((take count) . (drop (3 * count))) (randoms g :: [Float])
  vys <- pure $ ((* 500) . (0.5-)) <$> ((take count) . (drop (4 * count))) (randoms g :: [Float])
  vel <- pure $ zip vxs vys
  es <- pure $ zipWith3
    (\(x, y) (vx, vy) t -> if t < 0.5 then Static (x, y) (Dot (2 + 6 * t))
                             else Moving (x, y) (vx * t * 0.01, vy * t * 0.01) (Rect 2 2))
                             pos vel types
  initialModel <- pure $ World {elapsedTime = 0, entities = es}
  simulate
    windowDisplay
    yellow
    simulationRate
    initialModel
    drawingFunc
    updateFunc
      where
        simulationRate :: Int
        simulationRate = 60

        drawingFunc :: World -> Picture
        drawingFunc (World { elapsedTime = et, entities = es }) =
          translate (-width / 2) (-height / 2) $ Pictures $ drawRenderable <$> es

        updateFunc :: ViewPort -> Float -> World -> World
        updateFunc _ = update
