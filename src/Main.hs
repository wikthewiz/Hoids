module Main where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT


main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Hoids"
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, RGBMode]
  depthFunc  $= Just Less
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
-- Use this function to move and impact the displaying.
frameUpdate :: IO ()
frameUpdate = do
  flush

bgColor :: Color4 GLfloat   
bgColor = (Color4 1 (242/255) (229/255) 0.5)

display :: IO ()
display = do
  clearColor $= bgColor
  clear [ ColorBuffer, DepthBuffer ]
  loadIdentity
  preservingMatrix $ renderPrimitive Polygon $ do
        color $ (Color4 (0.0::GLfloat) 0 0 0.4)
        mapM_ (\(x, y, z)->vertex$Vertex3 x y z) $ circle 0.1 (0,0,-0.4)
  swapBuffers

circle :: GLfloat -> (GLfloat,GLfloat,GLfloat) -> [(GLfloat,GLfloat,GLfloat)]
circle r (x,y,z) = map (\n -> ( xCalc(n) * r * 400/680 + x, yCalc(n) * r + y,0.0 * r + z )) [1..nrOfLines]
        where 
                nrOfLines = 100
                xCalc n = sin(2*pi*n/nrOfLines)
                yCalc n = cos(2*pi*n/nrOfLines) 
  