-- Written by Aur Saraf
-- 
-- Released to the public domain
-- 
-- 3D Tetris

module Main where

import Matrix

import Graphics.UI.GLUT hiding (rect)
import Graphics.Rendering.OpenGL hiding (rect)
import Graphics.GLUtil
import Graphics.Rendering.OpenGL.Raw (glUniformMatrix4fv)
import Control.Applicative
import Data.IORef
import System.Exit

import qualified Control.Monad.State as S
import Data.Monoid
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


type ListBuilder a = [a] -> [a]
data MeshBuilder = MB M4f GLuint (ListBuilder V3f) (ListBuilder GLuint)
type MeshM = S.State MeshBuilder
runMesh :: MeshM () -> IO Mesh
runMesh mesh = do
  let MB m i vs es = S.execState mesh $ MB identity 0 id id
  print $ m
  print $ vs []
  makeMesh (vs []) (es [])

transform :: M4f -> MeshM ()
transform m = do
  MB old_m i vs es <- S.get
  S.put $ MB (old_m ^*^ m) i vs es

point :: GLfloat -> GLfloat -> GLfloat -> MeshM GLuint
point a b c = do
  MB m i vs es <- S.get
  let transformed = m *^ v4 a b c 1
      V4 a' b' c' 1 = transformed
      transformed' = v3 a' b' c'
  S.put $ MB m (i+1) (vs . (transformed':)) es
  return i

triangle :: GLuint -> GLuint -> GLuint -> MeshM ()
triangle a b c = do
  MB m i vs es <- S.get
  S.put $ MB m i vs (es . (a:) . (b:) . (c:))

rect a b c d = do
  triangle a b c
  triangle a c d

cube :: MeshM ()
cube = do
    a <- point 0 0 0
    b <- point 0 1 0
    c <- point 1 1 0
    d <- point 1 0 0
    a' <- point 0 0 1
    b' <- point 0 1 0
    c' <- point 1 1 0
    d' <- point 1 0 0
    rect a b c d
    rect d c c' d'
    rect d' c' b' a'
    rect a' b' b a
    rect a d d' a'
    rect b b' c' c


makeResources :: FilePath -> FilePath -> IO Resources
makeResources vertexShaderPath fragmentShaderPath =
  let initialCameraPosition = v3 0.5 (-0.25) (-1.25)
  in Resources
     <$> runMesh cube
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