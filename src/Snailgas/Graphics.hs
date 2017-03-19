{-# LANGUAGE OverloadedStrings #-}

module Snailgas.Graphics
  ( initialiseGraphics
  , loadTexture
  , drawTexture
  , createAtlas
  , drawAtlas
  , loadTextureA
  , getTextureA
  )
where

import Data.List
import Data.IORef
import Graphics.GL.Core33
import System.IO.Unsafe
import Foreign.C.Types (CFloat(..), CUInt(..))
import Foreign (sizeOf, alloca, poke, allocaArray, pokeArray)
import Foreign.Ptr (nullPtr, plusPtr)
import Data.Vector.Storable (unsafeWith)
import Codec.Picture
import Data.Maybe (isNothing, isJust)
import System.FilePath (takeFileName, dropExtensions)
import qualified Data.Map.Strict as M
import qualified Snailgas.Graphics.GL as GL

data Texture = Texture { tex_id :: GLuint
                       , tex_uvs :: (Float, Float, Float, Float)
                       , tex_vbuf :: GLuint
                       }


data Atlas = Atlas { atlas_tex_id :: GLuint
                   , atlas_width :: Int
                   , atlas_height :: Int
                   , atlas_texture_entries :: M.Map String Texture
                   , atlas_binary_tree :: AtlasNode
                   }


data AtlasNode = AtlasNode { node_children :: Maybe (AtlasNode, AtlasNode)
                           , node_rect :: (Int, Int, Int, Int)
                           , node_image :: Maybe String
                           } deriving Show


loadTexture :: String -> IO (Texture)
loadTexture path = do
  tex_id <- GL.loadTexture path
  createTexture tex_id (0, 1, 1, 0)


createTexture :: GLuint -> (Float, Float, Float, Float) -> IO Texture
createTexture tex_id uvs@(u0, v0, u1, v1) = do
  vbuf <- GL.uploadBuffer undefined CFloat [ -1, -1, u0, v1
                                           ,  1, -1, u1, v1
                                           ,  1,  1, u1, v0
                                           , -1,  1, u0, v0
                                           ]
  return $ Texture tex_id uvs vbuf


spriteShader :: IORef GLuint
spriteShader = unsafePerformIO $ newIORef undefined


initialiseGraphics :: IO ()
initialiseGraphics = do
  shader <- GL.loadShaderProgram "data/shaders/sprite.vert" "data/shaders/sprite.frag"
  writeIORef spriteShader shader
  writeIORef atlastex =<< createTexture (fromIntegral 0) (0, 1, 1, 0)


-- just a test function
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
  let root = AtlasNode Nothing (0, 0, width, height) Nothing
  return $ Atlas texid width height M.empty root


-- | Finds free space in the atlas binary tree, updating it and returning the free coordinates
-- Returns an unchanged tree and Nothing if there was a large enough block of free space
findFreeSpace :: AtlasNode -> (Int, Int) -> (AtlasNode, Maybe (Int, Int, Int, Int))
findFreeSpace tree@(AtlasNode children rect@(x0,y0,x1,y1) image) (dimx, dimy) =
  case children of
       Just (childA, childB) ->
         let (tA, rA) = findFreeSpace childA (dimx, dimy)
             (tB, rB) = findFreeSpace childB (dimx, dimy)
         in (if isJust rA
                  then (tree { node_children = Just (tA, childB) }, rA)
                  else (tree { node_children = Just (childA, tB) }, rB))
       Nothing ->
         let containsImage = isJust image
             rectW = x1-x0
             rectH = y1-y0
             fits = rectW >= dimx && rectH >= dimy
             fitsExactly = rectW == dimx && rectH == dimy
         in case image of
           Just img -> (tree, Nothing)
           Nothing  ->
             if not fits
              then (tree, Nothing)
              else if fitsExactly
                    then (tree { node_image = Just "" }, Just rect)
                    else
                      let dw = rectW - dimx
                          dh = rectH - dimy
                          rectA = if dw > dh
                                      then (x0, y0, x0+dimx-0, y1)
                                      else (x0, y0, x1, y0+dimy-0)
                          rectB = if dw > dh
                                      then (x0+dimx, y0, x1, y1)
                                      else (x0, y0+dimy, x1, y1)
                          childA = AtlasNode Nothing rectA Nothing
                          childB = AtlasNode Nothing rectB Nothing
                          (newChildA, res) = findFreeSpace childA (dimx, dimy)
                          newTree = tree { node_children = Just (newChildA, childB) }
                      in (newTree, res)


loadTextureA :: Atlas -> String-> IO Atlas
loadTextureA atlas filename = do
  eitherImgErr <- readImage filename
  let texid = atlas_tex_id atlas
  let atlasWidth = fromIntegral $ atlas_width atlas
  let atlasHeight = fromIntegral $ atlas_height atlas
  let nameOnly = dropExtensions . takeFileName $ filename

  if isJust (M.lookup nameOnly (atlas_texture_entries atlas))
   then putStrLn ("Image with filename " ++ nameOnly ++ " already exists in atlas") >> return atlas
   else
    case eitherImgErr of
        Left err -> (putStrLn $ "Error loading image " ++ filename ++ ": " ++ err) >> return atlas
        Right img -> do
          -- convert image and get size
          let imgRGBA8 = convertRGBA8 img
          let (dimx, dimy) = (fromIntegral . imageWidth $ imgRGBA8, fromIntegral . imageHeight $ imgRGBA8)

          -- Find free space
          let (updatedTree, space) =
                findFreeSpace (atlas_binary_tree atlas) (fromIntegral dimx, fromIntegral dimy)

          case space of
               Nothing -> (putStrLn $ "Not enough free space in atlas for texture " ++ filename)
                       >> return atlas
               Just (x, y, _, _) -> unsafeWith (imageData imgRGBA8) $ \ptr -> do
                    glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) dimx dimy GL_RGBA GL_UNSIGNED_BYTE ptr
                    let texCoords = ((fromIntegral x)/atlasWidth,
                                     (fromIntegral y)/atlasHeight,
                                     (fromIntegral x + fromIntegral dimx)/atlasWidth,
                                     (fromIntegral y + fromIntegral dimy)/atlasHeight)
                    texture <- createTexture (atlas_tex_id atlas) texCoords
                    let newTextureEntries = M.insert nameOnly texture (atlas_texture_entries atlas)
                    let newAtlas = atlas { atlas_binary_tree = updatedTree,
                                           atlas_texture_entries = newTextureEntries }
                    return newAtlas


-- | Get a texture from an atlas with a given name
getTextureA :: Atlas -> String -> Maybe Texture
getTextureA atlas name = M.lookup name (atlas_texture_entries atlas)
