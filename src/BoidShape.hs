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
