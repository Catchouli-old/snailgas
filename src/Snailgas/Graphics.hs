{-# LANGUAGE OverloadedStrings #-}

module Snailgas.Graphics
  ( initialiseGraphics
  , loadTexture
  , drawTexture
  , createAtlas
  , drawAtlas
  , loadTextureA
  )
where

import Debug.Trace
import Data.List
import Data.IORef
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
                   , atlas_width :: Int
                   , atlas_height :: Int
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


spriteShader :: IORef GLuint
spriteShader = unsafePerformIO $ newIORef undefined


initialiseGraphics :: IO ()
initialiseGraphics = do
  shader <- GL.loadShaderProgram "data/sprite.vert" "data/sprite.frag"
  writeIORef spriteShader shader
  writeIORef atlastex =<< createTexture (fromIntegral 0) (0, 1, 1, 0)


-- just a test function
--atlastex = unsafePerformIO $ createTexture (fromIntegral 0) (0, 1, 1, 0)
atlastex :: IORef Texture
atlastex = unsafePerformIO $ newIORef undefined
drawAtlas :: Atlas -> IO ()
drawAtlas atlas = do
  let texid = atlas_tex_id atlas
  atlastex <- readIORef atlastex
  drawTexture (atlastex { tex_id = texid })

drawTexture :: Texture -> IO ()
drawTexture tex = do
  let stride = fromIntegral (4 * sizeOf (undefined :: CFloat)) :: GLsizei
  let vertOffset = nullPtr
  let uvOffset = plusPtr nullPtr (2 * sizeOf (undefined :: CFloat))

  shader <- readIORef spriteShader

  in_pos_loc <- GL.attribLoc shader "in_pos"
  in_uv_loc <- GL.attribLoc shader "in_uv"
  sampler_tex <- GL.uniformLoc shader "sampler_tex"

  glUseProgram shader

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


-- | Finds (dimx, dimy) of free space in the atlas, returning (-1, -1) if there wasn't a big enough block
findFreeSpace :: Atlas -> (Int, Int) -> Maybe (Int, Int)
findFreeSpace atlas (dimx, dimy) =
  let atlasWidth = atlas_width atlas
      atlasHeight = atlas_height atlas
      checkCoord (x, y) = let m@(ax0, ay0, ax1, ay1) = (x, y, x + dimx, y + dimy)
                              overlap = (flip find) (M.toList $ atlas_texture_entries atlas) isOverlapping
                              isOverlapping (_, (u0, v0, u1, v1)) =
                                let o@(bx0, by0, bx1, by1) = (round (u0 * fromIntegral atlasWidth),
                                                            round (v0 * fromIntegral atlasHeight),
                                                            round (u1 * fromIntegral atlasWidth),
                                                            round (v1 * fromIntegral atlasHeight))
                                    overlaps = (ax0 <= bx1+1 && bx0 <= ax1+1 && ay0 <= by1+1 && by0 <= ay1+1)
                                in overlaps
                          in case overlap of
                                  Nothing -> True
                                  _       -> False
  in find checkCoord [(x, y) | y <- [0..atlasWidth-1-dimx], x <- [0..atlasHeight-1-dimy]]


loadTextureA :: Atlas -> String-> IO Atlas
loadTextureA atlas filename = do
  eitherImgErr <- readImage filename
  let texid = atlas_tex_id atlas
  let atlasWidth = fromIntegral $ atlas_width atlas
  let atlasHeight = fromIntegral $ atlas_height atlas

  if (/=Nothing) (M.lookup filename (atlas_texture_entries atlas))
   then putStrLn ("Image with filename " ++ filename ++ " already exists in atlas") >> return atlas
   else
    case eitherImgErr of
        Left err -> (putStrLn $ "Error loading image " ++ filename ++ ": " ++ err) >> return atlas
        Right img -> do
          -- convert image and get size
          let imgRGBA8 = convertRGBA8 img
          let (dimx, dimy) = (fromIntegral . imageWidth $ imgRGBA8, fromIntegral . imageHeight $ imgRGBA8)

          -- find space in atlas
          let res = GL.mapTuple fromIntegral <$> findFreeSpace atlas (fromIntegral dimx, fromIntegral dimy)

          case res of
               Nothing -> (putStrLn $ "Not enough free space in atlas for texture " ++ filename)
                       >> return atlas
               Just (x, y) -> unsafeWith (imageData imgRGBA8) $ \ptr -> do
                    glTexSubImage2D GL_TEXTURE_2D 0 x y dimx dimy GL_RGBA GL_UNSIGNED_BYTE ptr
                    let newAtlas = atlas { atlas_texture_entries =
                      M.insert filename ( (fromIntegral x)/atlasWidth, (fromIntegral y)/atlasHeight,
                                    (fromIntegral $ x + dimx)/atlasWidth, (fromIntegral $ y + dimy)/atlasHeight)
                                  (atlas_texture_entries atlas) }
                    return newAtlas
