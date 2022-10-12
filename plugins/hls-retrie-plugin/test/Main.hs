{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}

module Main (main) where

import           Control.Monad                     (void)
import           Data.Aeson
import qualified Data.Map                          as M
import           Data.Text                         (Text)
import qualified Development.IDE.Plugin.CodeAction as Refactor
import           Ide.Plugin.Config
import qualified Ide.Plugin.Retrie                 as Retrie
import           Ide.Types                         (IdePlugins (IdePlugins),
                                                    defConfigForPlugins)
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

retriePlugin :: PluginDescriptor IdeState
retriePlugin = Retrie.descriptor "retrie"

refactorPlugin :: PluginDescriptor IdeState
refactorPlugin = Refactor.iePluginDescriptor mempty "refactor"

tests :: TestTree
tests = testGroup "Retrie"
    [ inlineThisTests
    ]

inlineThisTests :: TestTree
inlineThisTests = testGroup "Inline this"
    [
        testCase "provider" testInlineThisProvider,
        testInlineThisIdentity
    ]

testInlineThisProvider :: Assertion
testInlineThisProvider = runWithRetrie $ do
    adoc <- openDoc "A.hs" "haskell"
    waitForTypecheck adoc
    let p = Position 4 16
    codeActions <- getCodeActions adoc $ Range p p
    liftIO $ map codeActionTitle codeActions @?= [Just "Inline identity"]

testInlineThisIdentity :: TestTree
testInlineThisIdentity = goldenWithRetrie "identity" "A" $ \adoc -> do
    waitForTypecheck adoc
    let p = Position 4 16
    codeActions <- getCodeActions adoc $ Range p p
    liftIO $ map codeActionTitle codeActions @?= [Just "Inline identity"]
    case codeActions of
        [InR ca] -> do
            executeCodeAction ca
            void $ skipManyTill anyMessage $ getDocumentEdit adoc

codeActionTitle :: (Command |? CodeAction) -> Maybe Text
codeActionTitle (InR CodeAction {_title}) = Just _title
codeActionTitle _                         = Nothing

goldenWithRetrie :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRetrie title path act =
    goldenWithHaskellDoc testPlugins title testDataDir path "expected" "hs" $ \doc -> do
        sendConfigurationChanged $ toJSON $
            def { plugins = M.fromList [("retrie", def)] }
        act doc

runWithRetrie :: Session a -> IO a
runWithRetrie = runSessionWithServer testPlugins testDataDir

testPlugins = IdePlugins [refactorPlugin, retriePlugin]

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
