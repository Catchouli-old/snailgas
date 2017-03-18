{-# LANGUAGE OverloadedStrings #-}

module Snailgas
(
  runGame
)
where

import SDL
import Foreign (Storable, Ptr, alloca, peek, allocaArray, pokeArray, sizeOf, poke)
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.C.String (withCString, peekCString)
import Foreign.C.Types (CFloat(..))
import Data.List (null)
import Control.Arrow ((***))
import Control.Monad (unless, join, when, void, unless)
import Linear.V2
import Graphics.GL.Core33
import qualified Data.Text as T
import qualified Data.Text.IO as T


width, height :: Int
(width, height) = (800, 600)


-- | Takes the parameters for a game to run, and launches it in a window
runGame :: IO ()
runGame = do
  initializeAll
  let windowDesc = defaultWindow { windowOpenGL = Just defaultOpenGL }
  window <- createWindow "My little window" windowDesc
  renderer <- createRenderer window (-1) defaultRenderer
  loop <- gameLoop window renderer
  loop
  destroyRenderer renderer
  destroyWindow window


-- | The main loop for our game
-- The outer IO () should be evaluated once and then the resulting io action can
-- be used to start the game loop
gameLoop :: Window -> Renderer -> IO (IO ())
gameLoop window renderer = do
  program <- loadShaderProgram "data/basic.vert" "data/basic.frag"
  triangle <- uploadBuffer undefined CFloat [-1, -1, 0, 0, 1, -1, 1, 1, 0, 1, 0.5, 1]
  let triangleStride = fromIntegral (4 * sizeOf (undefined :: CFloat)) :: GLsizei
  let triangleVertOffset = nullPtr
  let triangleUvOffset = plusPtr nullPtr (2 * sizeOf (undefined :: CFloat))

  let loop = do
      events <- pollEvents
      let eventPayloads = map eventPayload events

      let quitting = not . null . filter (==QuitEvent) $ map eventPayload events

      (mode, P (V2 x y)) <- getModalMouseLocation
      
      let (px, py) = (fromIntegral x - fromIntegral width * 0.5, fromIntegral height * 0.5 - fromIntegral y)

      -- render
      glClearColor 0 0 0 1
      glClear GL_COLOR_BUFFER_BIT

      glEnableVertexAttribArray =<< attribLoc program "in_pos"
      glBindBuffer GL_ARRAY_BUFFER triangle
      glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE triangleStride triangleVertOffset
      glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE triangleStride triangleUvOffset
      glDrawArrays GL_TRIANGLES 0 3
      glDisableVertexAttribArray =<< attribLoc program "in_pos"

      glSwapWindow window

      unless quitting loop

  return loop


-- | Map a function across a pair
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

-- | Turn any pair of integers into a Num a => SDL.V2 a
mkV2 :: (Integral a, Num b) => (a, a) -> SDL.V2 b
mkV2 = uncurry SDL.V2 . mapTuple fromIntegral

-- | Abstracts a common pattern with pointers
withNewPtr :: Storable b => (Ptr b -> IO a) -> IO b
withNewPtr f = alloca (\p -> f p >> peek p)

-- | Upload a list of floats into a buffer object
-- Call like this: uploadBuffer undefined CFloat [1..10]
-- The undefined is annoying and it pisses me off, sorry...
-- It can be any value of the type b though if you want.
-- Remember to delete it
uploadBuffer :: Storable b => b -> (a -> b) -> [a] -> IO GLuint
uploadBuffer v f hlist = do
  let len = length hlist
  let cfsize = sizeOf v
  let buflen = fromIntegral $ len * cfsize
  buffer <- withNewPtr (glGenBuffers 1)
  glBindBuffer GL_ARRAY_BUFFER buffer
  ptr <- allocaArray len
                     (\ptr -> pokeArray ptr (map f hlist)
                           >> glBufferData GL_ARRAY_BUFFER buflen ptr GL_STATIC_DRAW
                     )
  return buffer

-- | Load a shader and compile it, returning the id
-- A shader id is returned whether it succeeds or not so remember to delete it
loadShader :: GLenum -> T.Text -> IO GLuint
loadShader shaderType datafile = do
  -- Load source
  let path = T.unpack datafile
  source <- T.readFile path >>= return . T.unpack

  -- Compile shader
  shader <- glCreateShader shaderType
  withCString source
    (\str -> alloca
               (\ptr -> poke ptr str
                     >> glShaderSource shader 1 ptr nullPtr
               )
    )
  glCompileShader shader

  -- Check result
  isCompiled <- withNewPtr (glGetShaderiv shader GL_COMPILE_STATUS)

  -- Output any errors
  when (isCompiled == 0) $ do
    length <- withNewPtr (glGetShaderiv shader GL_INFO_LOG_LENGTH)
    error <- allocaArray (fromIntegral length)
                         (\ptr -> glGetShaderInfoLog shader length nullPtr ptr
                               >> (peekCString ptr))
    putStr $ "Failed to compile shader" ++ path ++ ":\n" ++ error

  return shader

-- | Link program
-- A program id is returned regardless of whether link succeeds, so free it
linkProgram :: GLuint -> GLuint -> IO GLuint
linkProgram vert frag = do
  -- Link program
  program <- glCreateProgram
  glAttachShader program vert
  glAttachShader program frag
  glLinkProgram program

  -- Check result
  isLinked <- withNewPtr (glGetProgramiv program GL_LINK_STATUS)

  -- Output any errors
  when (isLinked == 0) $ do
    length <- withNewPtr (glGetProgramiv program GL_INFO_LOG_LENGTH)
    error <- allocaArray (fromIntegral length)
                         (\ptr -> glGetProgramInfoLog program length nullPtr ptr
                               >> (peekCString ptr))
    putStr $ "Failed to link program:\n" ++ error

  return program

-- | Loads a vertex and fragment shader, compiles them and links them
-- A program id is returned regardless of whether this succeeds or not so free it
loadShaderProgram :: T.Text -> T.Text -> IO GLuint
loadShaderProgram v f = do
  vert <- loadShader GL_VERTEX_SHADER v
  frag <- loadShader GL_FRAGMENT_SHADER f
  prog <- linkProgram vert frag
  glDeleteShader vert
  glDeleteShader frag
  return prog

-- | Get the location for a shader attribute
attribLoc :: GLuint -> String -> IO GLuint
attribLoc program str = do withCString str (glGetAttribLocation program) >>= return . fromIntegral

-- | Get the location for a shader uniform
uniformLoc :: GLuint -> String -> IO GLuint
uniformLoc program str = do withCString str (glGetUniformLocation program) >>= return . fromIntegral
