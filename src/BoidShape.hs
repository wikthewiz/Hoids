module BoidShape (boid, boidRadious,boidFrame) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT.Objects


boidRadious:: GLfloat
boidRadious = (sqrt 3) * 0.3

--boid :: (Num a, VertexComponent a) => a -> IO ()
boid = renderObject  Solid Tetrahedron
  
boidFrame = renderObject Wireframe Tetrahedron

vertify3 :: [(GLfloat,GLfloat,GLfloat)] -> IO ()
vertify3 verts = sequence_ $ map (\(a,b,c) -> vertex $ Vertex3 a b c) verts 

cube w = vertify3 
  [( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
   ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
   ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
   (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
   ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
   ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w)]
    

pyramid d = vertify3 
  [(d  * 400/680,d,0),(0,d,0),(-d  * 400/680,0,0),
   (d  * 400/680,d,0),(0,0,d),(-d  * 400/680,0,0)]
