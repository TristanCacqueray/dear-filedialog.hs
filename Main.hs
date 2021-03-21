{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Managed
import DearImGui
import qualified DearImGui.FileDialog as ImFD
import DearImGui.GLFW
import DearImGui.GLFW.OpenGL
import DearImGui.OpenGL3
import Graphics.GL
import Graphics.UI.GLFW (Window)
import qualified Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
  initialised <- GLFW.init
  unless initialised $ error "GLFW init failed"

  runManaged $ do
    mwin <-
      managed $
        bracket
          (GLFW.createWindow 800 600 "Hello, Dear ImGui!" Nothing Nothing)
          (maybe (return ()) GLFW.destroyWindow)
    case mwin of
      Just win -> do
        liftIO $ do
          GLFW.makeContextCurrent (Just win)
          GLFW.swapInterval 1

        -- Create an ImGui context
        _ <- managed $ bracket createContext destroyContext

        -- Initialize ImGui's GLFW backend
        _ <- managed_ $ bracket_ (glfwInitForOpenGL win True) glfwShutdown

        -- Initialize ImGui's OpenGL backend
        _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown

        liftIO $ mainLoop win
      Nothing -> do
        error "GLFW createWindow failed"

  GLFW.terminate

mainLoop :: Window -> IO ()
mainLoop win = do
  -- Process the event loop
  GLFW.pollEvents
  close <- GLFW.windowShouldClose win
  unless close do
    -- Tell ImGui we're starting a new frame
    openGL3NewFrame
    glfwNewFrame
    newFrame

    -- Build the GUI
    bracket_ (begin "Hello, ImGui!") end do
      button "Open File Dialog" >>= \case
        False -> return ()
        True -> ImFD.openDialog "ChooseFileDlgKey" "Choose File" ".hs,.cabal" "."

      ImFD.display "ChooseFileDlgKey" >>= \case
        False -> return ()
        True -> do
          ImFD.isOk >>= \case
            False -> return ()
            True -> do
              selection <- ImFD.getSelection
              putStrLn $ "Selection: " <> selection

          ImFD.close
    -- Render
    glClear GL_COLOR_BUFFER_BIT

    render
    openGL3RenderDrawData =<< getDrawData

    GLFW.swapBuffers win

    mainLoop win
