module BoidWorld(getPoints,createWorld,World,Boid,Vector) where

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
createBoids nrOfBoids (Vector x y z) = let b = createBoid
                                        in  b : createBoids (nrOfBoids - 1) (pos b)
    where
        createBoid =  Boid { 
            direction = Vector 0 0 0,
            pos = Vector (x + boidRadious) (y + boidRadious) (z + boidRadious) 
        } 
 

createWorld :: Int -> World
createWorld nrOfBoids = World {boids = createBoids nrOfBoids $ Vector 0 0 0 }