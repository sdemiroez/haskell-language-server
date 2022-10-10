{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns   #-}
module Ide.Plugin.HaddockComments.Data
    ( generateHaddockComments
    ) where

import           Control.Monad              (unless)
import           Data.Function              ((&))
import           Debug.Trace
import           Development.IDE.GHC.Compat
import           GHC                        (EpAnn (EpAnn), EpAnnComments (..),
                                             EpaComment (..),
                                             EpaCommentTok (..), LEpaComment,
                                             SrcSpanAnnA, ann, comments,
                                             getFollowingComments)

generateHaddockComments :: LHsDecl GhcPs -> Maybe (LHsDecl GhcPs)
generateHaddockComments hsDecl@(L _ (TyClD _ (DataDecl { tcdDataDefn = HsDataDefn { dd_cons = cons } }))) = do
    unless (missingSomeHaddock cons) Nothing

    pure hsDecl
generateHaddockComments _ = Nothing
-- 1. detect if existing comments exist

missingSomeHaddock :: [LConDecl GhcPs] -> Bool
missingSomeHaddock = any $ \lcon@(L _ conDecl) -> case conDecl of
    ConDeclH98 { con_args = RecCon (L _ fields) } ->
        elem (Just False) $ hasHaddock lcon : fmap hasHaddock fields
    _ -> False

hasHaddock :: GenLocated SrcSpanAnnA a -> Maybe Bool
hasHaddock (L (ann -> EpAnn { comments }) _) = Just $
    any priorCommentIsHaddock (priorComments comments)
    || any followingCommentIsHaddock (getFollowingComments comments)
hasHaddock _                                 = Nothing

priorCommentIsHaddock :: LEpaComment -> Bool
priorCommentIsHaddock comment = getCommentTok comment & \case
    EpaDocCommentNext _ -> True
    _                   -> False

followingCommentIsHaddock :: LEpaComment -> Bool
followingCommentIsHaddock comment = getCommentTok comment & \case
    EpaDocCommentPrev _ -> True
    _                   -> False

getCommentTok :: LEpaComment -> EpaCommentTok
getCommentTok (L _ EpaComment {ac_tok}) = ac_tok
