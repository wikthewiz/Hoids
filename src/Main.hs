module Main where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT


main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Hoids"
  initialDisplayMode $= [DoubleBuffered]
  windowSize  $= (Size 680 400)
  windowPosition $= (Position 500 500)
  cursor $= None
  closeCallback $= Just close
  displayCallback $= display
  idleCallback $= Just frameUpdate 
  mainLoop
 

close :: IO ()
close = do
  putStrLn "exit"
  flush
-- Use this function tp move and impact the displaying.
-- There should be no things done i display
frameUpdate :: IO ()
frameUpdate = do
  flush

bgColor :: Color4 GLfloat   
bgColor = (Color4 1 (242/255) (229/255) 1)

display :: IO ()
display = do
  clearColor $= bgColor
  clear [ ColorBuffer, DepthBuffer ]
  loadIdentity
  swapBuffers
