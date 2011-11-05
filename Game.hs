-- Written by Aur Saraf
-- 
-- Released to the public domain
-- 
-- 3D Tetris

module Main where

import Matrix

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Graphics.Rendering.OpenGL.Raw (glUniformMatrix4fv)
import Control.Applicative
import Data.IORef
import System.Exit

import Unsafe.Coerce
import Foreign.Ptr


data Resources = Resources { 
    cubeMesh :: Mesh
  , rShaders :: Shaders
  , cameraPosition :: V3f
  , rWindowSize :: Size
  , projectionMatrix :: M4f
  , modelViewMatrix :: M4f
  }

data Shaders = Shaders { vertexShader :: VertexShader
                       , fragmentShader :: FragmentShader
                       , program :: Program
                         
                       , rUniforms :: Uniforms
                       , rAttribs :: Attribs
                       }

data Uniforms = Uniforms { projectionMatrixU :: UniformLocation
                         , modelViewMatrixU :: UniformLocation
                         }

data Attribs = Attribs { positionA :: AttribLocation
                       }

data Mesh = Mesh { vertexBuffer :: BufferObject
                 , elementBuffer :: BufferObject
                 , elementCount :: NumArrayIndices
                 }


vertexData :: [V3f]
vertexData = [
  v3 (-0.875) (-1.0) (-2.45)
  , v3 1.875 (-1.0) (-2.45)
  , v3 1.875 (-1.0) 0.2
  , v3 (-0.875) (-1.0) 0.2
  , v3 (-0.875) (-1.0) 0.2
  , v3 1.875 (-1.0) 0.2
  , v3 1.875 (-1.0) 0.2
  , v3 (-0.875) (-1.0) 0.2
  ]
elementData :: [GLuint]
elementData = [0,1,2,0,2,3,4,5,6,4,6,7]


makeResources :: FilePath -> FilePath -> IO Resources
makeResources vertexShaderPath fragmentShaderPath =
  let initialCameraPosition = v3 0.5 (-0.25) (-1.25)
  in Resources
     <$> makeMesh vertexData elementData
     <*> makeShaders vertexShaderPath fragmentShaderPath
     <*> pure initialCameraPosition
     <*> get initialWindowSize
     <*> (get initialWindowSize >>= (pure . calculateProjectionMatrix) :: IO M4f)
     <*> (pure (calculateModelViewMatrix initialCameraPosition) :: IO M4f)

makeShaders :: FilePath -> FilePath -> IO Shaders
makeShaders vertexShaderPath fragmentShaderPath = do
  vs <- loadShader vertexShaderPath
  fs <- loadShader fragmentShaderPath
  p <- linkShaderProgram [vs] [fs]
  let uniforms = Uniforms
                 <$> get (uniformLocation p "p_matrix")
                 <*> get (uniformLocation p "mv_matrix")
      attribs = Attribs
                <$> get (attribLocation p "position")
  Shaders vs fs p
    <$> uniforms
    <*> attribs

makeMesh :: [V3f] -> [GLuint] -> IO Mesh
makeMesh vertices elements = Mesh
    <$> makeBuffer ArrayBuffer verticesList
    <*> makeBuffer ElementArrayBuffer elements
    <*> pure (fromIntegral . toInteger $ length elements)
  where verticesList = concatMap toList vertices


display resources = do
  r <- get resources
  let shaders = rShaders r
      uniforms = rUniforms shaders
      attribs = rAttribs shaders
  
  clearColor $= Color4 0.1 0 0 1
  clear [ColorBuffer, DepthBuffer]
  
  hint PerspectiveCorrection $= Nicest
  polygonMode $= (Fill, Line)
  
  currentProgram $= Just (program shaders)
  
  setUniformM4f (projectionMatrixU uniforms) (projectionMatrix r)
  setUniformM4f (modelViewMatrixU uniforms) (modelViewMatrix r)
  
  vertexAttribArray (positionA attribs)  $= Enabled
  
  renderMesh r (cubeMesh r)
  
  vertexAttribArray (positionA attribs)  $= Disabled

  swapBuffers

glMatrix :: M4f -> IO (GLmatrix GLfloat)
glMatrix m = newMatrix ColumnMajor $ toList m

setUniformM4f :: UniformLocation -> M4f -> IO ()
setUniformM4f location value = do
  mat <- glMatrix value
  withMatrix mat (\order ptr -> 
                        glUniformMatrix4fv location' 1 false (castPtr ptr :: Ptr GLfloat))
    where location' = unsafeCoerce location :: GLint
          false = 0


renderMesh :: Resources -> Mesh -> IO ()
renderMesh r mesh = do
  let shaders = rShaders r
      attribs = rAttribs shaders
  
  vertexAttribPointer (positionA attribs) $= (ToFloat, VertexArrayDescriptor 3 Float 0 (wordPtrToPtr . fromIntegral $ 0))
  
  bindBuffer ArrayBuffer $= Just (vertexBuffer mesh)
  
  bindBuffer ElementArrayBuffer $= Just (elementBuffer mesh)
  drawElements Triangles (elementCount mesh) UnsignedInt offset0
  

move resources delta = do
  r <- get resources
  let newCameraPosition = cameraPosition r ^+ delta
  
  resources $= r { cameraPosition=newCameraPosition
                 , modelViewMatrix=calculateModelViewMatrix newCameraPosition
                 }
  postRedisplay Nothing

idle :: IORef Resources -> IO ()
idle _resources = do
  postRedisplay Nothing

reshape resources size = do
  r <- get resources
  resources $= r { rWindowSize=size
                 , projectionMatrix=calculateProjectionMatrix size
                 }
  viewport $= (Position 0 0, size)

keyboardMouse _resources (Char '\27')             Down _ _ = exitWith ExitSuccess
keyboardMouse resources (Char 'w')                Down _ _ = move resources (v3 0 0 0.1)
keyboardMouse resources (Char 's')                Down _ _ = move resources (v3 0 0 (-0.1))
keyboardMouse resources (Char 'a')                Down _ _ = move resources (v3 (-0.1) 0 0)
keyboardMouse resources (Char 'd')                Down _ _ = move resources (v3 0.1 0 0)
keyboardMouse resources (Char 'r')                Down _ _ = move resources (v3 0 0.1 0)
keyboardMouse resources (Char 'f')                Down _ _ = move resources (v3 0 (-0.1) 0)
keyboardMouse _resources _key                     _state _modifiers _position = return ()

main :: IO ()
main = do
  resources <- newIORef $ undefined
  
  (progname, args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer, RGBMode]
  initialWindowSize $= Size 640 480
  initialWindowPosition $= Position 300 200
  
  createWindow progname
  
  let (vertexShaderPath, fragmentShaderPath) = case args of
        [vp, fp] -> (vp, fp)
        [fp] -> ("v.glsl", fp)
        [] -> ("v.glsl", "f.glsl")
  r <- makeResources vertexShaderPath fragmentShaderPath
  resources $= r
  
--  motionCallback $= Just (drag resources . Just)
  
  displayCallback $= (display resources)
  idleCallback $= Just (idle resources)
  reshapeCallback $= Just (reshape resources)
  keyboardMouseCallback $= Just (keyboardMouse resources)
  
  mainLoop