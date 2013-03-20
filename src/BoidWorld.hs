module BoidWorld where

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


createBoid::(GLfloat,GLfloat,GLfloat) -> Boid
createBoid (x,y,z) = 
  Boid { 
        direction = unit $ Vector 1 1 1,
        pos = Vector x y z
  }

createBoidWithDir:: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat) -> Boid 
createBoidWithDir (dx,dy,dz) (x,y,z) = 
  Boid { 
    direction = unit $ Vector dx dy dz,
    pos = Vector x y z
  }

createWorld :: Int -> World
createWorld nrOfBoids = World {boids = createBoids nrOfBoids $ Vector 0 0 0 }

unit :: Vector -> Vector
unit (Vector x y z) = Vector (x/l) (y/l) (z/l)
  where
    l = sqrt(x * x + y * y + z * z)
    
moveBoid:: GLfloat -> Boid -> Boid
moveBoid l
         (Boid (Vector dirX dirY dirZ)
                (Vector x y z)) = createBoidWithDir (dirX,dirY,dirZ) (newX,newY ,newZ)
                where 
                  newX = x + dirX * l
                  newY = y + dirY * l
                  newZ = z + dirZ * l


x_rotate a = [1,0      ,0      ,
              0,cos(a) ,sin(a) ,
              0,-sin(a),cos(a) ]
              
y_rotate a = [cos(a),0,-sin(a),
              0     ,1,0      , 
              sin(a),0,cos(a) ]

z_rotate a = [cos(a) ,sin(a),0,
              -sin(a),cos(a),0, 
              0      ,0     ,1]
 
multi :: [GLfloat] -> Vector -> Vector
multi  m (Vector x y z) = Vector calcX calcY calcZ
        where 
                calcX = m!!0 * x + m!!1 * y + m!!2 * z  
                calcY = m!!3 * x + m!!4 * y + m!!5 * z
                calcZ = m!!6 * x + m!!7 * y + m!!8 * z
               
        
turnBoid rotate_matrix (Boid dirV (Vector x y z)) = 
  createBoidWithDir (dirX, dirY, dirZ) (x,y,z)
    where
      (Vector dirX dirY dirZ) = multi rotate_matrix dirV

