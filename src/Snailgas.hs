{-# LANGUAGE OverloadedStrings #-}

module Snailgas
(
  runGame
)
where

import SDL hiding (glBindTexture)
import Foreign (Storable, Ptr, alloca, peek, allocaArray, pokeArray, sizeOf, poke)
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.C.String (withCString, peekCString)
import Foreign.C.Types (CFloat(..), CUInt(..))
import Control.Monad (unless, foldM)
import Graphics.GL.Core33
import Snailgas.Graphics;
import qualified Snailgas.Graphics.GL as GL


width, height :: Int
(width, height) = (800, 600)


-- | Takes the parameters for a game to run, and launches it in a window
runGame :: IO ()
runGame = do
  initializeAll
  let windowDesc = defaultWindow { windowOpenGL = Just defaultOpenGL }
  window <- createWindow "My little window" windowDesc
  renderer <- createRenderer window (-1) defaultRenderer
  initialiseGraphics
  loop <- gameLoop window renderer
  loop
  destroyRenderer renderer
  destroyWindow window


-- | The main loop for our game
-- The outer IO () should be evaluated once and then the resulting io action can
-- be used to start the game loop
gameLoop :: Window -> Renderer -> IO (IO ())
gameLoop window renderer = do
  tex <- loadTexture "data/tex.gif"
  --atlas <- loadTextureA "data/tex.gif" =<< createAtlas 1024 1024
  emptyAtlas <- createAtlas 1024 1024
  atlas <- foldM loadTextureA emptyAtlas ["data/tex.gif",
                                          "data/tex1.gif",
                                          "data/tex2.gif",
                                          --"data/tex3.gif",
                                          --"data/tex4.gif",
                                          --"data/tex5.gif",
                                          --"data/tex6.gif",
                                          --"data/tex7.gif",
                                          --"data/tex8.gif",
                                          "data/tex9.gif"]

  let loop = do
        events <- pollEvents
        let eventPayloads = map eventPayload events

        let quitting = not . null . filter (==QuitEvent) $ map eventPayload events

        (mode, P (V2 x y)) <- getModalMouseLocation
        
        let (px, py) = (fromIntegral x - fromIntegral width * 0.5, fromIntegral height * 0.5 - fromIntegral y)

        -- render
        glClearColor 0 0 0 1
        glClear GL_COLOR_BUFFER_BIT

        --drawTexture tex
        drawAtlas atlas
        
        -- check for gl errors
        err <- glGetError
        unless (err == 0) $ putStrLn $ "opengl error: " ++ show err

        glSwapWindow window

        unless quitting loop

  return loop
