{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Ide.Plugin.HaddockComments.Data
    ( generateHaddockComments
    ) where

import           Control.Lens
import           Control.Monad                         (unless)
import           Data.Generics                         (Data, cast, everything)
import           Data.List                             (isPrefixOf)
import           Data.Maybe                            (isJust)
import           Debug.Trace
import           Development.IDE.GHC.Compat
import           GHC                                   (AddEpAnn, AnnContext,
                                                        AnnList, EpAnn (..),
                                                        EpAnnComments (..),
                                                        EpaComment, LEpaComment,
                                                        SrcSpanAnnA, ann,
                                                        comments,
                                                        getFollowingComments)
import           Ide.Plugin.HaddockComments.Prelude
import           Language.Haskell.GHC.ExactPrint       (balanceCommentsList',
                                                        runTransform)
import           Language.Haskell.GHC.ExactPrint.Utils (ghcCommentText)

generateHaddockComments :: LHsDecl GhcPs -> Maybe (LHsDecl GhcPs)
generateHaddockComments hsDecl@(L _ (TyClD _ (DataDecl { tcdDataDefn = HsDataDefn { dd_cons = cons } }))) = do
    let cons' = runTransform (fmap balanceFieldsComments <$> balanceCommentsList' cons) ^. _1
        allComments = collectComments hsDecl
    traceShow allComments $ unless (missingSomeHaddock cons') Nothing

    pure hsDecl
generateHaddockComments _ = Nothing

balanceFieldsComments :: LConDecl GhcPs -> LConDecl GhcPs
balanceFieldsComments (L locCon con@ConDeclH98 {con_args = RecCon (L locFields fields)}) =
    let fields' = runTransform (balanceCommentsList' fields) ^. _1
     in L locCon con {con_args = RecCon (L locFields fields')}
balanceFieldsComments lCon = lCon

missingSomeHaddock :: [LConDecl GhcPs] -> Bool
missingSomeHaddock = any $ \lcon@(L _ conDecl) -> case conDecl of
    ConDeclH98 { con_args = RecCon (L _ fields) } ->
        elem (Just False) $ hasHaddock lcon : fmap hasHaddock fields
    _ -> False

collectComments :: Data a => a -> [String]
collectComments = everything (++) go
  where
    go node
      | Just (ann :: EpAnn AnnContext) <- cast node = fmap ghcCommentText (priorComments (comments ann) ++ getFollowingComments (comments ann))
      | otherwise = []

hasHaddock :: (HasHsDocString a, HasAddEpAnn a) => GenLocated SrcSpanAnnA a -> Maybe Bool
hasHaddock (L (ann -> EpAnn { comments }) node) = Just $
    -- any (matchCommentPrefix priorCommentPrefix) (priorComments comments)
    -- || any (matchCommentPrefix followingCommentPrefix) (getFollowingComments comments)
    -- || isJust (getHsDocString node)
    epAnnHasHaddock (getAddEpAnn node)
hasHaddock _                                 = Nothing

-------------------------------------------------------

epAnnHasHaddock :: EpAnn [AddEpAnn] -> Bool
epAnnHasHaddock EpAnnNotUsed = error "unexpected EpAnnNotUsed"
epAnnHasHaddock EpAnn{comments} =
    any (matchCommentPrefix priorCommentPrefix) (priorComments comments)
    || any (matchCommentPrefix followingCommentPrefix) (getFollowingComments comments)

priorCommentPrefix :: [String]
priorCommentPrefix = ["-- |", "{-|", "{- |"]

followingCommentPrefix :: [String]
followingCommentPrefix = ["-- ^", "{-^", "{- ^"]

matchCommentPrefix :: [String] -> LEpaComment -> Bool
matchCommentPrefix prefix comment = any (`isPrefixOf` ghcCommentText comment) prefix

-- NOTE: we should probably use the following code instead of matching strings on our own.
-- However, all haddock comments are provided as EpaLineComment or EpaBlockComment.
-- I think that indicates an upstream bug.
--
-- priorCommentIsHaddock :: LEpaComment -> Bool
-- priorCommentIsHaddock comment = getCommentTok comment & \case
--     EpaDocCommentNext _ -> True
--     _                   -> False
-- followingCommentIsHaddock :: LEpaComment -> Bool
-- followingCommentIsHaddock comment = getCommentTok comment & \case
--     EpaDocCommentPrev _ -> True
--     _                   -> False
-- getCommentTok :: LEpaComment -> EpaCommentTok
-- getCommentTok (L _ EpaComment {ac_tok}) = traceShowId ac_tok

-------------------------------------------------------
