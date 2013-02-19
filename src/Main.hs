module Main where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Hoids"
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, RGBMode]
  depthFunc  $= Just Less
  windowSize  $= (Size 680 400)
  windowPosition $= (Position 500 500)
  cursor $= None
  world <- newIORef 1.0
  closeCallback $= Just close
  displayCallback $= display world
  idleCallback $= Just (frameUpdate world)
  mainLoop

boidRadious = 0.1

createBoids :: Int -> Vector -> [Boid]
createBoids 0 _ = []
createBoids nrOfBoids prevPos =  let boid = createBoid
                                 in  boid : createBoids (nrOfBoids - 1) (pos boid)
      where
        createBoid =  let x = prevPos
                      in 
                        Boid { 
                              direction = Vector 0 0 0,
                              pos = Vector x + boidRadious y + boidRadious z + boidRadious 
                            } 
 

createWorld nrOfBoids = World {boids = createBoids nrOfBoids $ Vector 0 0 0 }

close :: IO ()
close = do
        putStrLn "exit"
        flush
-- Use this function to move and impact the displaying.
frameUpdate :: IORef GLfloat -> IO()
frameUpdate world = do
        postRedisplay Nothing

bgColor :: Color4 GLfloat   
bgColor = (Color4 1 (242/255) (229/255) 0.5)

display :: IORef GLfloat -> IO()
display world = do
        w <- get world
        world $=! updateWorld(w)
        clearColor $= bgColor
        clear [ ColorBuffer, DepthBuffer ]
        loadIdentity
        preservingMatrix $ renderPrimitive Polygon $ do
                color $ (Color4 (0.0::GLfloat) 0 0 0.4)
                mapM_ (\(x, y, z)->vertex$Vertex3 x y z) $ circle (w*0.1) (0,0,-0.4)
        swapBuffers

circle :: GLfloat -> (GLfloat,GLfloat,GLfloat) -> [(GLfloat,GLfloat,GLfloat)]
circle r (x,y,z) = map (\n -> ( xCalc(n) * r * 400/680 + x, yCalc(n) * r + y,0.0 * r + z )) [1..nrOfLines]
        where 
                nrOfLines = 100
                xCalc n = sin(2*pi*n/nrOfLines)
                yCalc n = cos(2*pi*n/nrOfLines)

updateWorld :: Fractional a => a -> a
updateWorld world = world + 0.001

data Vector = Vector GLfloat GLfloat GLfloat deriving (Show)
data Boid  = Boid { direction :: Vector
                    , pos:: Vector 
                    } deriving (Show)
data World = World { boids :: [Boid] }  deriving (Show)
