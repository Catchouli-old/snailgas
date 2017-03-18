{-# LANGUAGE OverloadedStrings #-}

module Snailgas
(
  runGame
)
where

import Graphics
import SDL hiding (glBindTexture)
import Foreign (Storable, Ptr, alloca, peek, allocaArray, pokeArray, sizeOf, poke)
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.C.String (withCString, peekCString)
import Foreign.C.Types (CFloat(..), CUInt(..))
import Control.Monad (unless)
import Graphics.GL.Core33


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
  program <- loadShaderProgram "data/sprite.vert" "data/sprite.frag"
  triangle <- uploadBuffer undefined CFloat [-1, -1, 0, 0, 1, -1, 1, 1, 0, 1, 0.5, 1]
  let triangleStride = fromIntegral (4 * sizeOf (undefined :: CFloat)) :: GLsizei
  let triangleVertOffset = nullPtr
  let triangleUvOffset = plusPtr nullPtr (2 * sizeOf (undefined :: CFloat))

  tex <- loadTexture "data/tex.gif"

  let loop = do
        events <- pollEvents
        let eventPayloads = map eventPayload events

        let quitting = not . null . filter (==QuitEvent) $ map eventPayload events

        (mode, P (V2 x y)) <- getModalMouseLocation
        
        let (px, py) = (fromIntegral x - fromIntegral width * 0.5, fromIntegral height * 0.5 - fromIntegral y)

        -- render
        glClearColor 0 0 0 1
        glClear GL_COLOR_BUFFER_BIT

        in_pos_loc <- attribLoc program "in_pos"
        in_uv_loc <- attribLoc program "in_uv"
        sampler_tex <- uniformLoc program "sampler_tex"

        glUseProgram program

        glActiveTexture GL_TEXTURE0
        glBindTexture GL_TEXTURE_2D tex
        glUniform1i (fromIntegral sampler_tex) 0

        glEnableVertexAttribArray in_pos_loc
        glEnableVertexAttribArray in_uv_loc
        glBindBuffer GL_ARRAY_BUFFER triangle
        glVertexAttribPointer in_pos_loc 2 GL_FLOAT GL_FALSE triangleStride triangleVertOffset
        glVertexAttribPointer in_uv_loc 2 GL_FLOAT GL_FALSE triangleStride triangleUvOffset
        glDrawArrays GL_TRIANGLES 0 3
        glDisableVertexAttribArray in_pos_loc
        glDisableVertexAttribArray in_uv_loc

        x <- glGetError
        unless (x == 0) $ putStrLn $ "opengl error: " ++ show x

        glSwapWindow window

        unless quitting loop

  return loop
