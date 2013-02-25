module BoidWorld(moveBoid, getPoints,createWorld,World,Boid,Vector) where

import Graphics.Rendering.OpenGL
import BoidShape

data Vector = Vector GLfloat GLfloat GLfloat deriving (Show)
data Boid  = Boid { direction :: Vector
                    , pos:: Vector 
                    } deriving (Show)
data World = World { boids :: [Boid] }  deriving (Show)

getPoints :: World -> [(GLfloat,GLfloat,GLfloat)]
getPoints (World bs) = [(x,y,z) |  (Boid _ (Vector x y z)) <- bs ]

createBoids :: Int -> Vector -> [Boid]
createBoids 0 _ = []
createBoids nrOfBoids (Vector x y z) = let b = createBoid newPos
                                        in  b : createBoids (nrOfBoids - 1) (pos b)
    where
      newPos = ((x + boidRadious),(y + boidRadious),(z + boidRadious))
 
createBoid (x,y,z) = Boid { 
 direction = Vector 0 0 0,
 pos = Vector x y z
 }

createBoidWithDir (dx,dy,dz) (x,y,z) = 
  Boid { direction = Vector dx dy dz,
          pos = Vector x y z
        }

createWorld :: Int -> World
createWorld nrOfBoids = World {boids = createBoids nrOfBoids $ Vector 0 0 0 }

moveBoid (Vector dx dy dz )
         (Boid (Vector dirX dirY dirZ)
                (Vector x y z)) = createBoidWithDir (x + (dirX * dx), y + (dirY * dy), z + (dirZ * dz))
turnBoid angle
         (Boid (Vector dirX dirY dirZ)
                (Vector x y z)) = createBoidWithDir (x + (dirX * dx), y + (dirY * dy), z + (dirZ * dz))