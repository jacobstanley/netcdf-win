{-# LANGUAGE DeriveDataTypeable #-}
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
import System.Console.CmdArgs hiding (name)
import System.IO (IOMode(..), withFile, hGetContents)

------------------------------------------------------------------------

data MkStdcall = MkStdcall
    { path :: FilePath
    , exclude :: FilePath
    } deriving (Data, Typeable, Show)

defaultArgs :: MkStdcall
defaultArgs = MkStdcall
    { path    = "netcdf.h"      &= typ "PATH" &= args
    , exclude = "netcdf.ignore" &= typ "PATH" &= help "File containing functions to exclude"
    } &=
    help "Create stdcall wrappers for a C header file" &=
    summary "mkstdcall v0.1" &=
    details [ "To create stdcall wrappers, pass the path to a header file:"
            , "  mkstdcall ../netcdf-4.1.3/include/netcdf.h"
            ]

main :: IO ()
main = do
    args <- cmdArgs defaultArgs
    exclusions <- lines <$> readFile (exclude args)
    (CTranslUnit extDecls _) <- parseFile (path args)
    putStrLn "#include \"netcdf.h\""
    mapM_ print'
        $ map ncsPrefix
        $ map wrapWithStdcall
        $ filter (not . isVariadic)
        $ filter (maybe False (shouldWrap exclusions) . name)
        $ filter isExtern extDecls

  where
    -- Prefixes the function with ncs_ instead of nc_.
    -- The extra underscore at the start is to achieve the
    -- name mangling windows expects for a stdcall function
    ncsPrefix = rename $ ("_ncs" ++) . drop 2

    shouldWrap :: [String] -> String -> Bool
    shouldWrap exs fn = "nc_" `isPrefixOf` fn && not (fn `elem` exs)


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
        (stdcall $ dllexport ss) declr []
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
stdcall = addAttr "stdcall"

dllexport :: Data a => a -> a
dllexport = addAttr "dllexport"

addAttr :: Data a => String -> a -> a
addAttr x = everywhere (mkT add)
  where
    add :: [CDeclSpec] -> [CDeclSpec]
    add xs | isExtern xs = attr x : xs
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
-- Scrap Your Boilerplate

gany :: forall a b. (Typeable a, Data b) => (a -> Bool) -> b -> Bool
gany p = everything (||) (False `mkQ` p)

------------------------------------------------------------------------
-- Parsing / Pretty Printing

parseFile :: String -> IO CTranslUnit
parseFile path = do
    result <- parseCFile (newGCC "gcc") Nothing [] path
    case result of
        Left msg  -> error (show msg)
        Right ast -> return ast

print' :: Pretty a => a -> IO ()
print' = print . pretty
