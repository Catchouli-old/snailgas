module Graphics where

import SDL hiding (glBindTexture)
import Foreign (Storable, Ptr, alloca, peek, allocaArray, pokeArray, sizeOf, poke)
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.C.String (withCString, peekCString)
import Foreign.C.Types (CFloat(..), CUInt(..))
import Data.Vector.Storable (unsafeWith)
import Data.List (null)
import Control.Arrow ((***))
import Control.Monad (unless, join, when, void, unless)
import Data.Either
import Graphics.GL.Core33
import Codec.Picture
import qualified Data.Text as T
import qualified Data.Text.IO as T

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

-- | Load a texture from a file
loadTexture :: String -> IO GLuint
loadTexture filename = do
  -- Load image
  eitherImgErr <- readImage filename

  -- function for creating a new texture and copying the data from a ptr
  let createTex dimx dimy ptr = do
       tex <- withNewPtr (glGenTextures 1)
       glBindTexture GL_TEXTURE_2D tex
       glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
       glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_REPEAT)
       glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
       glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR)
       glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA8) dimx dimy 0 GL_RGBA GL_UNSIGNED_BYTE ptr
       return tex

  case eitherImgErr of
       -- upload image to gpu texture
       Right img -> do
         let imgRGBA8 = convertRGBA8 img
         let (dimx, dimy) = mapTuple fromIntegral (imageWidth imgRGBA8, imageHeight imgRGBA8)
         unsafeWith (imageData imgRGBA8) (createTex dimx dimy)
       -- return default texture
       Left err -> do (putStrLn $ "Error loading image " ++ filename ++ ": " ++ err)
                      alloca $ \ptr -> do
                        poke ptr (0xFFFF00FF :: CUInt)
                        createTex 1 1 ptr
