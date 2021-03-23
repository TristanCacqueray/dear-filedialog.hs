{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | DearImGui bindings for https://github.com/aiekick/ImGuiFileDialog
module DearImGui.FileDialog where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Traversable (for)
import Foreign
import Foreign.C
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp

C.context (Cpp.cppCtx <> C.bsCtx)
C.include "ImGuiFileDialog.h"
C.include "ImGuiFileDialogConfig.h"
Cpp.using "namespace IGFD"

openDialog :: (MonadIO m) => String -> String -> String -> String -> CInt -> m ()
openDialog desc title fileFilter loc limit = liftIO $ do
  withCString desc \descPtr -> do
    withCString title \titlePtr -> do
      withCString fileFilter \filterPtr -> do
        withCString loc \locPtr -> do
          [C.exp| void { ImGuiFileDialog::Instance()->OpenDialog(
                $(char* descPtr), $(char* titlePtr), $(char* filterPtr), $(char* locPtr), $(int limit) ) } |]

display :: (MonadIO m) => String -> m Bool
display desc = liftIO $ do
  withCString desc \descPtr ->
    (0 /=) <$> [C.exp| bool { ImGuiFileDialog::Instance()->Display( $(char* descPtr) ) } |]

isOk :: MonadIO m => m Bool
isOk = liftIO $ do
  (0 /=) <$> [C.exp| bool { ImGuiFileDialog::Instance()->IsOk( ) } |]

getSelection :: MonadIO m => m [FilePath]
getSelection = liftIO do
  selected <- fromIntegral <$> [C.block|
    int {
      typedef std::map <std::string, std::string> SelMap;
      SelMap m = ImGuiFileDialog::Instance()->GetSelection();
      return m.size();
    }
  |]

  if selected <= 0 then
    pure []
  else do
    allocaBytes (selected * pathLen) $ \ptr -> do
      [C.block|
        void {
          typedef std::map <std::string, std::string> SelMap;
          SelMap m = ImGuiFileDialog::Instance()->GetSelection();
          int ix = 0;
          for( SelMap::iterator it = m.begin(); it != m.end(); ++it ) {
            strncpy(
              $(char* ptr) + ix * $(int c'pathLen),
              it->second.c_str(),
              $(int c'pathLen) - 1
            );
            ix++;
          }
          return;
        }
      |]
      for [0 .. selected - 1] \ix ->
        peekCString (ptr `plusPtr` (pathLen * ix))
  where
    pathLen = 2048
    c'pathLen = fromIntegral pathLen

getFilePathName :: MonadIO m => m FilePath
getFilePathName = liftIO do
  peekCString =<< [C.exp| const char * { ImGuiFileDialog::Instance()->GetFilePathName().c_str() } |]

getCurrentPath :: MonadIO m => m FilePath
getCurrentPath = liftIO do
  peekCString =<< [C.exp| const char * { ImGuiFileDialog::Instance()->GetCurrentPath().c_str() } |]

getCurrentFilter :: MonadIO m => m String
getCurrentFilter = liftIO do
  peekCString =<< [C.exp| const char * { ImGuiFileDialog::Instance()->GetCurrentFilter().c_str() } |]

close :: MonadIO m => m ()
close = liftIO [C.exp| void { ImGuiFileDialog::Instance()->Close() } |]
