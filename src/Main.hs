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
  idleCallback $= Just (frameUpdate world)
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
        world $=! updateWorld(w)
        clearColor $= bgColor
        clear [ ColorBuffer, DepthBuffer ]
        loadIdentity
        do mapM_ (\(x, y, z)-> preservingMatrix $ do
            color $ Color3 ((x+1.0)/2.0) ((y+1.0)/2.0) ((z+1.0)/2.0)
            translate $ Vector3 x y z
            boid boidRadious
            ) $ getPoints w
        swapBuffers


updateWorld :: a -> a
updateWorld world = world
