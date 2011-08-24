{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.Generics
import Control.Applicative ((<$>))
import Language.C
import Language.C.Data.Ident (Ident(..))
import Language.C.System.GCC
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.List (isPrefixOf)


------------------------------------------------------------------------

main :: IO ()
main = do
    (CTranslUnit extDecls _) <- parseFile "netcdf.h"
    mapM_ print'
        $ map ncsPrefix
        $ map wrapWithStdcall
        $ filter (not . isVariadic)
        $ filter (maybe False ("nc_" `isPrefixOf`) . name)
        $ filter isExtern extDecls

  where
    -- prefixes the function with ncs_ instead of nc_
    ncsPrefix = rename $ ("ncs" ++) . drop 2


------------------------------------------------------------------------

-- | Wraps any function declarations with stdcall function definitions
wrapWithStdcall :: CExtDecl -> CExtDecl
wrapWithStdcall orig@(CDeclExt d) = maybe orig CFDefExt (wrapWithStdcallD d)
wrapWithStdcall x                 = x

wrapWithStdcallD :: CDecl -> Maybe CFunDef
wrapWithStdcallD (CDecl ss ds n) = do
    declr <- headDeclr ds
    origFun <- fmap var (name declr)
    let origArgs = map var (funArgs declr)
    return $ CFunDef
        (stdcall ss) declr []
        (block $ call origFun origArgs) n
  where
    -- gets the first declarator
    headDeclr :: [(Maybe CDeclr, b, c)] -> Maybe CDeclr
    headDeclr = listToMaybe . mapMaybe fst3

    fst3 (x,_,_) = x


-- | Gets all the arguments defined in function declarators
funArgs :: CDeclr -> [String]
funArgs (CDeclr _ dds _ _ _) = catMaybes (concatMap go dds)
  where
    go :: CDerivedDeclr -> [Maybe String]
    go (CFunDeclr (Left ids) _ _)      = map name ids
    go (CFunDeclr (Right (ds, _)) _ _) = map name ds
    go _                               = []

-- | Checks if a function declarator has a variadic argument.
-- *This would mean that cdecl is the only valid calling convention*
isVariadic :: Data a => a -> Bool
isVariadic = gany p
  where
    p :: CDerivedDeclr -> Bool
    p (CFunDeclr (Right (_, True)) _ _) = True
    p _                                 = False

------------------------------------------------------------------------
-- Statements / Expressions

-- | Wraps a statement in its own block
block :: CStat -> CStat
block x = CCompound [] [CBlockStmt x] undefNode

expr :: CExpr -> CStat
expr x = CExpr (Just x) undefNode

call :: CExpr -> [CExpr] -> CStat
call fun args = expr (call' fun args)

call' :: CExpr -> [CExpr] -> CExpr
call' fun args = CCall fun args undefNode

var :: String -> CExpr
var nam = CVar (ident nam) undefNode

ident :: String -> Ident
ident nam = Ident nam 0 undefNode

------------------------------------------------------------------------
-- Exported Functions / Calling Conventions

-- | Changes any declaration or definition which is marked
-- as 'extern' to use the 'stdcall' calling convention.
stdcall :: Data a => a -> a
stdcall = everywhere (mkT add)
  where
    add :: [CDeclSpec] -> [CDeclSpec]
    add xs | isExtern xs = attr "stdcall" : xs
           | otherwise   = xs

-- | Creates a attribute type qualification with the
-- specified identifier.
attr :: String -> CDeclSpec
attr x = CTypeQual $ CAttrQual $ CAttr (ident x) [] undefNode

-- | Checks if the declaration or definition is marked
-- as extern.
isExtern :: Data a => a -> Bool
isExtern = gany p
  where
    p :: CStorageSpec -> Bool
    p (CExtern _) = True
    p _           = False

------------------------------------------------------------------------
-- Naming

class Named a where
    name :: a -> Maybe String

instance Named Ident where
    name (Ident x _ _) = Just x

instance Named CDeclr where
    name (CDeclr x _ _ _ _) = x >>= name

instance Named CDecl where
    name (CDecl _ ds _) = listToMaybe (mapMaybe go ds)
      where
        go (declr, _, _) = declr >>= name

instance Named CFunDef where
    name (CFunDef _ d _ _ _) = name d

instance Named CExtDecl where
    name (CDeclExt d)  = name d
    name (CFDefExt f)  = name f
    name (CAsmExt _ _) = Nothing

class Rename a where
    rename :: (String -> String) -> a -> a

instance Rename Ident where
    rename f (Ident nam h n) = Ident (f nam) h n

instance Rename CDeclr where
    rename f (CDeclr idnt d t a n) = CDeclr (rename f <$> idnt) d t a n

instance Rename CDecl where
    rename f (CDecl ss ds n) = CDecl ss (map go ds) n
      where
        go (d, i, e) = (rename f <$> d, i, e)

instance Rename CFunDef where
    rename f (CFunDef ss d ds st n) = CFunDef ss (rename f d) ds st n

instance Rename CExtDecl where
    rename f (CDeclExt d)  = CDeclExt (rename f d)
    rename f (CFDefExt fd) = CFDefExt (rename f fd)
    rename _ asm           = asm

------------------------------------------------------------------------
-- Scrap Your Boilerplat

gany :: forall a b. (Typeable a, Data b) => (a -> Bool) -> b -> Bool
gany p = everything (||) (False `mkQ` p)

------------------------------------------------------------------------
-- Pretty Printing

parseFile :: String -> IO CTranslUnit
parseFile path = do
    result <- parseCFile (newGCC "gcc") Nothing [] path
    case result of
        Left msg  -> error (show msg)
        Right ast -> return ast

print' :: Pretty a => a -> IO ()
print' = print . pretty
