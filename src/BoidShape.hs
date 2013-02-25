module BoidShape (boid, boidRadious,boidFrame) where

import Graphics.Rendering.OpenGL

boidRadious:: GLfloat
boidRadious = 0.1

--boid :: (Num a, VertexComponent a) => a -> IO ()
boid w = do 
  renderPrimitive Quads $ do (pyramid w)
  
boidFrame w = renderPrimitive Lines $ pyramid w

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

--circle :: (GLfloat,GLfloat,GLfloat) -> [(GLfloat,GLfloat,GLfloat)]
--circle (x,y,z) = map (\n -> ( xCalc(n) * r * 400/680 + x, yCalc(n) * r + y,0.0 * r + z )) [1..nrOfLines]
--        where 
--                nrOfLines = 100
--                xCalc n = sin(2*pi*n/nrOfLines)
--                yCalc n = cos(2*pi*n/nrOfLines)
--                r = boidRadious