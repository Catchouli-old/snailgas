{-# LANGUAGE OverloadedStrings #-}

module Snailgas.Graphics
  ( loadTexture
  , drawTexture
  , createAtlas
  , drawAtlas
  , loadTextureA
  )
where

import Graphics.GL.Core33
import System.IO.Unsafe
import Foreign.C.Types (CFloat(..), CUInt(..))
import Foreign (sizeOf, alloca, poke, allocaArray, pokeArray)
import Foreign.Ptr (nullPtr, plusPtr)
import Data.Vector.Storable (unsafeWith)
import Codec.Picture
import qualified Data.Map.Strict as M
import qualified Snailgas.Graphics.GL as GL

data Texture = Texture { tex_id :: GLuint
                       , tex_uvs :: (Float, Float, Float, Float)
                       , tex_vbuf :: GLuint
                       }


data Atlas = Atlas { atlas_tex_id :: GLuint
                   , width :: Int
                   , height :: Int
                   , atlas_texture_entries :: M.Map String (Float, Float, Float, Float)
                   }


loadTexture :: String -> IO (Texture)
loadTexture path = do
  tex_id <- GL.loadTexture path
  createTexture tex_id (0, 1, 1, 0)


createTexture :: GLuint -> (Float, Float, Float, Float) -> IO Texture
createTexture tex_id uvs = do
  vbuf <- GL.uploadBuffer undefined CFloat [ -1, -1, 0, 1
                                           ,  1, -1, 1, 1
                                           ,  1,  1, 1, 0
                                           , -1,  1, 0, 0
                                           ]
  return $ Texture tex_id uvs vbuf


spriteShader :: GLuint
spriteShader = unsafePerformIO $ GL.loadShaderProgram "data/sprite.vert" "data/sprite.frag"
  

-- just a test function
atlastex = unsafePerformIO $ createTexture (fromIntegral 0) (0, 1, 1, 0)
drawAtlas :: Atlas -> IO ()
drawAtlas atlas = do
  let texid = atlas_tex_id atlas
  drawTexture (atlastex { tex_id = texid })

drawTexture :: Texture -> IO ()
drawTexture tex = do
  let stride = fromIntegral (4 * sizeOf (undefined :: CFloat)) :: GLsizei
  let vertOffset = nullPtr
  let uvOffset = plusPtr nullPtr (2 * sizeOf (undefined :: CFloat))

  in_pos_loc <- GL.attribLoc spriteShader "in_pos"
  in_uv_loc <- GL.attribLoc spriteShader "in_uv"
  sampler_tex <- GL.uniformLoc spriteShader "sampler_tex"

  glUseProgram spriteShader

  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D (tex_id tex)
  glUniform1i (fromIntegral sampler_tex) 0

  glEnableVertexAttribArray in_pos_loc
  glEnableVertexAttribArray in_uv_loc
  glBindBuffer GL_ARRAY_BUFFER (tex_vbuf tex)
  glVertexAttribPointer in_pos_loc 2 GL_FLOAT GL_FALSE stride vertOffset
  glVertexAttribPointer in_uv_loc 2 GL_FLOAT GL_FALSE stride uvOffset
  glDrawArrays GL_TRIANGLE_FAN 0 4
  glDisableVertexAttribArray in_pos_loc
  glDisableVertexAttribArray in_uv_loc


createAtlas :: Int -> Int -> IO Atlas
createAtlas width height = do
  texid <- allocaArray (width*height) $ \ptr -> do
              pokeArray ptr $ replicate (width*height) (0xFFFF00FF :: CUInt)
              GL.createTex (fromIntegral width) (fromIntegral height) ptr
  return $ Atlas texid width height M.empty


loadTextureA :: String -> Atlas -> IO Atlas
loadTextureA filename atlas = do
  eitherImgErr <- readImage filename
  let texid = atlas_tex_id atlas

  case eitherImgErr of
       Left err -> do (putStrLn $ "Error loading image " ++ filename ++ ": " ++ err)
       Right img -> do
         -- convert image and get size
         let imgRGBA8 = convertRGBA8 img
         let (dimx, dimy) = (fromIntegral . imageWidth $ imgRGBA8, fromIntegral . imageHeight $ imgRGBA8)

         -- find space in atlas

         -- add to atlas
         unsafeWith (imageData imgRGBA8) $ \ptr -> do
           glTexSubImage2D GL_TEXTURE_2D 0 0 0 dimx dimy GL_RGBA GL_UNSIGNED_BYTE ptr
           putStrLn "stuf"

  return atlas
