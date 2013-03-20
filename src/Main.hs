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
  windowSize  $= (Size 500 500)
  windowPosition $= (Position 500 500)
  cursor $= None
  world <- newIORef (createWorld 2)
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


display :: IORef World -> IO()
display world = do
        w <- get world
        clearColor $= bgColor
        clear [ ColorBuffer, DepthBuffer ]
        loadIdentity
        preservingMatrix $ do
        scale 0.3 0.3 (0.3::GLfloat)
        rotate (90) $ Vector3 1 1 (-2::GLfloat)
        do mapM_ (\(x, y, z)-> preservingMatrix $ do
            color $ Color3 ((x+1)/2.0) ((y)/2.0) ((z+30.0)/2.0)
            translate $ Vector3 x y z
            boid
            color $ Color3 (0.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat) --set outline color to black
            boidFrame 
            ) $ getPoints w
        swapBuffers

idle world = do
  w <- get world
  world $=! (updateWorld w)
  postRedisplay Nothing
 
 
updateWorld :: World -> World
updateWorld (World bs) =
  World {
    boids = [turnBoid matrix b | b <-bs]
  }
    where
        ticks = 1000
        length =  2 * boidRadious * pi / ticks
        matrix =    x_rotate $ pi/3
