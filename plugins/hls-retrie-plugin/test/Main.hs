{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}

module Main (main) where

import           Data.Aeson
import qualified Data.Map          as M
import           Data.Text         (Text)
import           Ide.Plugin.Config
import qualified Ide.Plugin.Retrie as Retrie
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

retriePlugin :: PluginDescriptor IdeState
retriePlugin = Retrie.descriptor "retrie"

tests :: TestTree
tests = testGroup "Retrie"
    [ inlineThisTests
    ]

inlineThisTests :: TestTree
inlineThisTests = testGroup "Inline this" [
    testCase "provider" testInlineThisProvider
    ]

testInlineThisProvider :: Assertion
testInlineThisProvider = runWithRetrie $ do
    adoc <- openDoc "A.hs" "haskell"
    waitForTypecheck adoc
    let p = Position 4 16
    codeActions <- getCodeActions adoc $ Range p p
    liftIO $ map codeActionTitle codeActions @?= [Just "Inline identity"]

codeActionTitle :: (Command |? CodeAction) -> Maybe Text
codeActionTitle (InR CodeAction {_title}) = Just _title
codeActionTitle _                         = Nothing

goldenWithRetrie :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRetrie title path act =
    goldenWithHaskellDoc retriePlugin title testDataDir path "expected" "hs" $ \doc -> do
        sendConfigurationChanged $ toJSON $
            def { plugins = M.fromList [("retrie", def)] }
        act doc

runWithRetrie = runSessionWithServer retriePlugin testDataDir

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
