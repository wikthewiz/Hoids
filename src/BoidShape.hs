module BoidShape (boid, boidRadious) where

import Graphics.Rendering.OpenGL

boidRadious:: GLfloat
boidRadious = 0.05

boid :: (Num a, VertexComponent a) => a -> IO ()
boid w = do 
  renderPrimitive Quads $ do
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 w (-w) w

    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) w w

    vertex $ Vertex3 w w w
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 (-w) w w

    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w

    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w

    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) w (-w)
  

-- boid :: (GLfloat,GLfloat,GLfloat) -> [(GLfloat,GLfloat,GLfloat)]
--boid (x,y,z) = map (\n -> ( xCalc(n) * r * 400/680 + x, yCalc(n) * r + y,0.0 * r + z )) [1..nrOfLines]
--        where 
--                nrOfLines = 100
--                xCalc n = sin(2*pi*n/nrOfLines)
--                yCalc n = cos(2*pi*n/nrOfLines)
--                r = boidRadious