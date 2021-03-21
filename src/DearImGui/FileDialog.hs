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
import Foreign
import Foreign.C
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp

C.context (Cpp.cppCtx <> C.bsCtx)
C.include "ImGuiFileDialog.h"
C.include "ImGuiFileDialogConfig.h"
Cpp.using "namespace IGFD"

openDialog :: (MonadIO m) => String -> String -> String -> String -> m ()
openDialog desc title fileFilter loc = liftIO $ do
  withCString desc \descPtr -> do
    withCString title \titlePtr -> do
      withCString fileFilter \filterPtr -> do
        withCString loc \locPtr -> do
          [C.exp| void { ImGuiFileDialog::Instance()->OpenDialog(
                $(char* descPtr), $(char* titlePtr), $(char* filterPtr), $(char* locPtr) ) } |]

display :: (MonadIO m) => String -> m Bool
display desc = liftIO $ do
  withCString desc \descPtr ->
    (0 /=) <$> [C.exp| bool { ImGuiFileDialog::Instance()->Display( $(char* descPtr) ) } |]

isOk :: MonadIO m => m Bool
isOk = liftIO $ do
  (0 /=) <$> [C.exp| bool { ImGuiFileDialog::Instance()->IsOk( ) } |]

-- TODO: figure out how to make this version work:
-- getSelection :: MonadIO m => m [FilePath]
-- getSelection = liftIO do
--   v <-
--     [C.block| const char** {
--         typedef std::map <std::string, std::string> SelMap;
--         SelMap m = ImGuiFileDialog::Instance()->GetSelection();
--         std::vector <const char*> v;
--         for( SelMap::iterator it = m.begin(); it != m.end(); ++it ) {
--           printf("Pushing %s\n", it->second.c_str());
--           v.push_back( it->second.c_str() );
--         }
--         v.push_back(0);
--         return &v[0];
--     } |]
--   res <- peekArray0 nullPtr v
--   mapM peekCString res

-- getSelection that only support a single selection...
getSelection :: MonadIO m => m FilePath
getSelection = liftIO $
  allocaBytes 1024 $ \ptr -> do
    [C.block| void {
        typedef std::map <std::string, std::string> SelMap;
        SelMap m = ImGuiFileDialog::Instance()->GetSelection();
        for( SelMap::iterator it = m.begin(); it != m.end(); ++it ) {
          strncpy($( char* ptr), it->second.c_str(), 1023);
          break;
        }
        return;
    } |]
    -- res <- peekArray0 nullPtr v
    peekCString ptr

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
