module Main where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

import BoidShape
import BoidWorld

main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Hoids"
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, RGBMode]
  depthFunc  $= Just Less
  windowSize  $= (Size 680 400)
  windowPosition $= (Position 500 500)
  cursor $= None
  world <- newIORef (createWorld 12)
  closeCallback $= Just close
  displayCallback $= display world
  idleCallback $= Just (idle world)
  mainLoop


close :: IO ()
close = do
        putStrLn "exit"
        flush
        
-- Use this function to move and impact the displaying.
frameUpdate :: IORef World -> IO()
frameUpdate world = do
        postRedisplay Nothing

bgColor :: Color4 GLfloat   
bgColor = (Color4 1 (242/255) (229/255) 0.5)


--drawWorld :: World -> [(GLfloat,GLfloat,GLfloat)]
--drawWorld (World bs) = do 
--    renderPrimitive Polygon $ do
--     (map boid  [(x,y,z) |  (Boid _ (Vector x y z)) <- bs ])

display :: IORef World -> IO()
display world = do
        w <- get world
        clearColor $= bgColor
        clear [ ColorBuffer, DepthBuffer ]
        loadIdentity
        do mapM_ (\(x, y, z)-> preservingMatrix $ do
            color $ Color3 ((x+1.0)/2.0) ((y+1.0)/2.0) ((z+1.0)/2.0)
            translate $ Vector3 x y z
            boid boidRadious
            color $ Color3 (0.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat) --set outline color to black
            boidFrame boidRadious
            ) $ getPoints w
        swapBuffers


idle world = do
  w <- get world
  world $=! (updateWorld w)
  postRedisplay Nothing
 
 
updateWorld :: a -> a
updateWorld (World bs) =  [ moveBoid b | b <-bs]

--moveInCircle r (Boid _ (Vector x y z)) =
getCirclePos r x y z = map (\n -> ( xCalc(n) * r * 400/680 + x, yCalc(n) * r + y,0.0 * r + z )) [1..nrOfLines]
        where 
                nrOfLines = 100
                xCalc n = sin(2*pi*n/nrOfLines)
                yCalc n = cos(2*pi*n/nrOfLines)
                r = boidRadious     
--circle :: (GLfloat,GLfloat,GLfloat) -> [(GLfloat,GLfloat,GLfloat)]
--circle (x,y,z) = map (\n -> ( xCalc(n) * r * 400/680 + x, yCalc(n) * r + y,0.0 * r + z )) [1..nrOfLines]
--        where 
--                nrOfLines = 100
--                xCalc n = sin(2*pi*n/nrOfLines)
--                yCalc n = cos(2*pi*n/nrOfLines)
--                r = boidRadious
