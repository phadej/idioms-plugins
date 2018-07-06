module IdiomsPlugin (plugin) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable          (for_)
import Data.List              (foldl')
import Data.List.NonEmpty     (NonEmpty (..))

import qualified Data.Generics as SYB

import qualified ErrUtils    as Err
import qualified GhcPlugins  as GHC
import           HsExtension (GhcPs, NoExt (..))
import           HsSyn
import           SrcLoc

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
    { GHC.parsedResultAction = \_cliOptions -> pluginImpl
    }

pluginImpl :: GHC.ModSummary -> GHC.HsParsedModule -> GHC.Hsc GHC.HsParsedModule
pluginImpl _modSummary m = do
    dflags <- GHC.getDynFlags
    debug $ GHC.showPpr dflags (GHC.hpm_module m )
    hpm_module' <- transform dflags (GHC.hpm_module m)
    let module' = m { GHC.hpm_module = hpm_module' }
    return module'

debug :: MonadIO m => String -> m ()
-- debug = liftIO . putStrLn
debug _ = pure ()

transform
    :: GHC.DynFlags
    -> GHC.Located (HsModule GhcPs)
    -> GHC.Hsc (GHC.Located (HsModule GhcPs))
transform dflags = SYB.everywhereM (SYB.mkM transform') where
    transform' :: LHsExpr GhcPs -> GHC.Hsc (LHsExpr GhcPs)
    transform' e@(L l (HsPar x (L l' (ExplicitList  _ Nothing exprs)))) | inside l l' =
        case exprs of
            [L _ (OpApp _ lhs op rhs)] ->
                return (pureExpr op `apExpr` lhs `apExpr` rhs)
            [expr] -> do
                let (f :| args) = matchApp expr
                let f' = pureExpr f
                debug $ "FUN : " ++ GHC.showPpr dflags f
                debug $ "FUN+: " ++ GHC.showPpr dflags f'
                for_ args $ \arg ->
                    debug $ "ARG : " ++ GHC.showPpr dflags arg
                let result = foldl' apExpr f' args
                debug $ "RES : " ++ GHC.showPpr dflags result
                return (L l (HsPar x result))
            _ -> do
                liftIO $ GHC.putLogMsg dflags GHC.NoReason Err.SevWarning l (GHC.defaultErrStyle dflags) $
                    GHC.text "Non singleton idiom bracket list"
                    GHC.$$
                    GHC.ppr exprs
                return e
    transform' expr = return expr

-------------------------------------------------------------------------------
-- Pure
-------------------------------------------------------------------------------

-- f ~> pure f
pureExpr :: LHsExpr GhcPs -> LHsExpr GhcPs
pureExpr (L l f) =
    L l $ HsApp NoExt (L l' (HsVar NoExt (L l' pureRdrName))) (L l' f)
  where
    l' = GHC.noSrcSpan

pureRdrName :: GHC.RdrName
pureRdrName = GHC.mkRdrUnqual (GHC.mkVarOcc "pure")

-------------------------------------------------------------------------------
-- Ap
-------------------------------------------------------------------------------

-- f x ~> f <*> x
apExpr :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
apExpr f x =
    L l' $ OpApp NoExt f (L l' (HsVar NoExt (L l' apRdrName))) x
  where
    l' = GHC.noSrcSpan

apRdrName :: GHC.RdrName
apRdrName = GHC.mkRdrUnqual (GHC.mkVarOcc "<*>")

-------------------------------------------------------------------------------
-- Matching helper
-------------------------------------------------------------------------------

-- | Match nested applications:
-- f x y z ~> f :| [x,y,z]
--
matchApp :: LHsExpr p -> NonEmpty (LHsExpr p)
matchApp (L _ (HsApp _ f x)) = neSnoc (matchApp f) x
matchApp e = pure e

neSnoc :: NonEmpty a -> a -> NonEmpty a
neSnoc (x :| xs) y = x :| xs ++ [y]

-------------------------------------------------------------------------------
-- Location checker
-------------------------------------------------------------------------------

-- Check that spans are right inside each others, i.e. we match
-- that there are no spaces between parens and brackets
inside :: SrcSpan -> SrcSpan -> Bool
inside (RealSrcSpan a) (RealSrcSpan b) = and
    [ srcSpanStartLine a == srcSpanStartLine b
    , srcSpanEndLine a == srcSpanEndLine b
    , srcSpanStartCol a + 1 == srcSpanStartCol b
    , srcSpanEndCol a == srcSpanEndCol b + 1
    ]
inside _ _ = False
