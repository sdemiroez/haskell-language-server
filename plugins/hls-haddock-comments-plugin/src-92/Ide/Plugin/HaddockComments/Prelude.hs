{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ide.Plugin.HaddockComments.Prelude
    ( HasHsDocString(..)
    , HasAddEpAnn(..)
    , module Development.IDE.GHC.Compat
    ) where

import           Development.IDE.GHC.Compat
import           GHC                        (AddEpAnn, EpAnn)

class HasHsDocString a where
    getHsDocString :: a -> Maybe LHsDocString

instance HasHsDocString (ConDecl GhcPs) where
    getHsDocString ConDeclGADT{con_doc} = con_doc
    getHsDocString ConDeclH98{con_doc}  = con_doc

instance HasHsDocString (ConDeclField GhcPs) where
    getHsDocString ConDeclField{cd_fld_doc} = cd_fld_doc

class HasAddEpAnn a where
    getAddEpAnn :: a -> EpAnn [AddEpAnn]

instance HasAddEpAnn (ConDecl GhcPs) where
    getAddEpAnn :: ConDecl GhcPs -> EpAnn [AddEpAnn]
    getAddEpAnn ConDeclH98{con_ext}    = con_ext
    getAddEpAnn ConDeclGADT{con_g_ext} = con_g_ext

instance HasAddEpAnn (ConDeclField GhcPs) where
    getAddEpAnn :: ConDeclField GhcPs -> EpAnn [AddEpAnn]
    getAddEpAnn = cd_fld_ext
