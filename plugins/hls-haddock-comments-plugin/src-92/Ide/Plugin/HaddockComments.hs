{-# LANGUAGE DuplicateRecordFields #-}

module Ide.Plugin.HaddockComments
    ( descriptor
    ) where

import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans.Maybe
import           Data.Maybe                            (fromMaybe)
import           Development.IDE                       hiding (pluginHandlers)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.ExactPrint
import           Development.IDE.GHC.ExactPrint        (GetAnnotatedParsedSource (..))
import qualified Development.IDE.GHC.ExactPrint        as ExactPrint
import           Development.IDE.Plugin.CodeAction     (mkExactprintPluginDescriptor)
import           Ide.Types
import           Language.Haskell.GHC.ExactPrint
import           Language.LSP.Types

data Log = LogExactPrint ExactPrint.Log

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = mkExactprintPluginDescriptor (cmapWithPrio LogExactPrint recorder) $
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionProvider
    }

codeActionProvider :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider ideState _pId (CodeActionParams _ _ (TextDocumentIdentifier uri) range
    CodeActionContext {_diagnostics = List diags}) = fmap (fromMaybe defaultResult) . runMaybeT $ do
    nfp <- MaybeT . pure . uriToNormalizedFilePath . toNormalizedUri $ uri
    pm <- MaybeT . liftIO $ runAction "HaddockComments.GetAnnotatedParsedSource" ideState $
        use GetAnnotatedParsedSource nfp
    let locDecls = hsmodDecls . unLoc . astA $ pm
        -- TODO
        -- edits = [runGenComments gen locDecls anns range | noErr, gen <- genList]
    return $ Right $ List [] -- [InR $ toAction title uri edit | (Just (title, edit)) <- edits]
  where
    defaultResult = Right $ List []
    noErr = and $ (/= Just DsError) . _severity <$> diags

declHaddockGenerator :: [LHsDecl GhcPs -> Maybe (LHsDecl GhcPs)]
declHaddockGenerator =
    [
    ]
