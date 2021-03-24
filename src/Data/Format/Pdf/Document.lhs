> {-# LANGUAGE
>     RankNTypes
>   , TypeFamilies
>   , InstanceSigs
>   , TypeOperators
>   , OverloadedStrings
>   , FlexibleInstances
>   , ScopedTypeVariables
> #-}
> 
> module Data.Format.Pdf.Document where
> 
> import           Control.Monad (forM)
> import           Control.Monad.IO.Class
> import qualified Data.ByteString.Lazy as LBS
> import qualified Data.Map as M
> 
> import Data.Format.Pdf.FromBytes
> import Data.Format.Pdf.Object
> import Data.Format.Pdf.With
> import Data.Format.Pdf.Monad





> data Catalog = Catalog
>   { pages     :: Pages
>   , nameTrees :: M.Map LBS.ByteString NameTree
>   }
> 
> emptyCatalog :: Catalog
> emptyCatalog = Catalog
>   { pages     = emptyPages
>   , nameTrees = mempty
>   }

> renderCatalog
>   :: ( FromBytes p )
>   => Catalog -> PdfM p ()
> renderCatalog catalog = do
>   let
>     mkObj :: ObjRef -> Obj
>     mkObj pagesRef = dict'
>       [ ( "Type", name "Catalog" )
>       , ( "Pages", indRef pagesRef )
>       ]
> 
>   withObjRef mkObj $ \catRef -> do
>     setRoot catRef
>     pagesRef <- renderPages True catRef (pages catalog)
>     return pagesRef
> 
>   return ()

> withCatalog
>   :: ( FromBytes p )
>   => (Catalog :> PdfM p) () -> PdfM p ()
> withCatalog x =
>   makeWith emptyCatalog x renderCatalog





> data Pages = Pages
>   { subPages       :: [Either Page Pages]
>   , pagesResources :: M.Map Name Obj
>   }
> 
> emptyPages :: Pages
> emptyPages = Pages
>   { subPages = []
>   , pagesResources = mempty
>   }
 
> pageDescendants :: Pages -> Int
> pageDescendants =
>   sum . map leafCount . subPages
>   where
>     leafCount :: Either Page Pages -> Int
>     leafCount x = case x of
>       Left _ -> 1
>       Right ps -> pageDescendants ps

> renderPages
>   :: ( FromBytes p )
>   => Bool -> ObjRef -> Pages -> PdfM p ObjRef
> renderPages isRoot catRef pages = do
>   let
>     mkObj :: [ObjRef] -> Obj
>     mkObj pageRefs = dict' $ concat
>       [ [ ( "Type", name "Pages" )
>         , ( "Kids", array $ map indRef pageRefs )
>         , ( "Count", int $ pageDescendants pages )
>         ]
>       , if isRoot
>           then []
>           else [ ( "Parent", indRef catRef ) ]
>       , if M.null (pagesResources pages)
>           then []
>           else [ ( "Resources", dict (pagesResources pages) ) ]
>       ]
> 
>   withObjRef mkObj $ \pagesRef -> do
>     forM (subPages pages) $ \sub ->
>       case sub of
>         Left p -> renderPage pagesRef p
>         Right ps -> renderPages False pagesRef ps

> withPages
>   :: ( FromBytes p )
>   => (Pages :> Catalog :> PdfM p) () -> (Catalog :> PdfM p) ()
> withPages x =
>   makeWith emptyPages x $ \newPages ->
>     mutateWith $ \cat -> cat { pages = newPages }

> addPages
>   :: ( IsPdfM m )
>   => (Pages :> Pages :> m) () -> (Pages :> m) ()
> addPages x =
>   makeWith emptyPages x $ \newPages ->
>     mutateWith $ \oldPages -> oldPages
>       { subPages = subPages oldPages <> [Right newPages] }





> data Page = Page
>   { pageContents  :: [ObjRef]
>   , pageResources :: M.Map Name Obj
>   }
> 
> emptyPage :: Page
> emptyPage = Page
>   { pageContents  = []
>   , pageResources = mempty
>   }

> addContent
>   :: ( MonadIO m ) => ObjRef -> (Page :> m) ()
> addContent ref = mutateWith $ \st ->
>   st { pageContents = (pageContents st) <> [ref] }

> renderPage
>   :: ( FromBytes p )
>   => ObjRef -> Page -> PdfM p ObjRef
> renderPage pagesRef page = do
>   let
>     pageObj :: [ObjRef] -> Obj
>     pageObj refs = dict' $ concat
>       [ [ ( "Type", name "Page" )
>         , ( "Parent", indRef pagesRef )
>         , ( "Contents", array $ map indRef refs )
>         ]
>       , if M.null (pageResources page)
>           then []
>           else [ ( "Resources", dict (pageResources page) ) ]
>       ]
> 
>   let refs = pageContents page
>   addObj $ pageObj refs

> addPage
>   :: ( IsPdfM m )
>   => (Page :> Pages :> m) () -> (Pages :> m) ()
> addPage x =
>   makeWith emptyPage x $ \newPage ->
>     mutateWith $ \oldPages -> oldPages
>       { subPages = subPages oldPages <> [Left newPage] }





> class HasResources m where
>   updateResources :: (M.Map Name Obj -> M.Map Name Obj) -> m ()
> 
> instance
>   ( MonadIO m
>   ) => HasResources (Pages :> m)
>   where
>     updateResources
>       :: (M.Map Name Obj -> M.Map Name Obj) -> (Pages :> m) ()
>     updateResources f = mutateWith $ \st ->
>       st { pagesResources = f (pagesResources st) }
> 
> instance
>   ( MonadIO m
>   ) => HasResources (Page :> m)
>   where
>     updateResources
>       :: (M.Map Name Obj -> M.Map Name Obj) -> (Page :> m) ()
>     updateResources f = mutateWith $ \st ->
>       st { pageResources = f (pageResources st) }















> newtype NameTree = NameTree (M.Map LBS.ByteString Obj)

> testPdf :: IO ()
> testPdf = do
>   printLazyByteStringPdf id myPdf
> 
> putPdf :: FilePath -> IO ()
> putPdf path = do
>   writeLazyByteStringPdf path id myPdf
> 
> myPdf = withCatalog $ do
>   withPages $ do
>     fontRef <- addObj $ dict'
>       [ ( "Type", name "Font" )
>       , ( "Subtype", name "Type1" )
>       , ( "BaseFont", name "Helvetica" )
>       ]
>     updateResources $ \res -> M.insert "Font" (dict' [( "F13", indRef fontRef)]) res
>     addPage $ do
>       ref <- addObj $ stream' [] "BT /F13 12 Tf 288 720 Td (ABC) Tj ET 10 w 10 10 m 25 25 l S"
>       addContent ref
>       ref2 <- draw $ do
>         setLineWidth 10
>         moveTo 100 100
>         lineTo 200 100
>         lineTo 200 200
>         lineTo 100 200
>         lineTo 100 100
>         strokePath
>       addContent ref2
>       ref3 <- draw $ do
>         moveTo 300 100
>         lineTo 300 200
>         lineTo 400 200
>         lineTo 400 100
>         fillPath Winding
>         closeAndStrokePath
>       addContent ref3
>       return ()
>     addPage $ do
>       return ()
>     addPages $ do
>       addPage $ do
>         return ()

BT /F13 12 Tf 288 720 Td (ABC) Tj ET



> data GfxOperator
>   = Move Int Int
>   | Line Int Int
>   | LineWidth Int
>   | StrokePath
>   | CloseAndStrokePath
>   | FillPath FillRule

> data FillRule
>   = Winding
>   | EvenOdd

> moveTo :: ( IsPdfM m ) => Int -> Int -> (Draw :> Page :> m) ()
> moveTo x y = mutateWith $ \st -> st
>   { gfxOperators = (gfxOperators st) <> [Move x y] }
> 
> lineTo :: ( IsPdfM m ) => Int -> Int -> (Draw :> Page :> m) ()
> lineTo x y = mutateWith $ \st -> st
>   { gfxOperators = (gfxOperators st) <> [Line x y] }
> 
> setLineWidth :: ( IsPdfM m ) => Int -> (Draw :> Page :> m) ()
> setLineWidth w = mutateWith $ \st -> st
>   { gfxOperators = (gfxOperators st) <> [LineWidth w] }
> 
> strokePath :: ( IsPdfM m ) => (Draw :> Page :> m) ()
> strokePath = mutateWith $ \st -> st
>   { gfxOperators = (gfxOperators st) <> [StrokePath] }
> 
> fillPath :: ( IsPdfM m ) => FillRule -> (Draw :> Page :> m) ()
> fillPath rule = mutateWith $ \st -> st
>   { gfxOperators = (gfxOperators st) <> [FillPath rule] }

> closeAndStrokePath :: ( IsPdfM m ) => (Draw :> Page :> m) ()
> closeAndStrokePath = mutateWith $ \st -> st
>   { gfxOperators = (gfxOperators st) <> [CloseAndStrokePath] }

> serializeGfxOp :: GfxOperator -> LBS.ByteString
> serializeGfxOp op = case op of
>   Move x y -> mconcat
>     [ showLazyByteString x, " ", showLazyByteString y, " m" ]
>   Line x y -> mconcat
>     [ showLazyByteString x, " ", showLazyByteString y, " l" ]
>   LineWidth w -> mconcat
>     [ showLazyByteString w, " w" ]
>   StrokePath -> "S"
>   CloseAndStrokePath -> "s"
>   FillPath rule -> case rule of
>     Winding -> "f"
>     EvenOdd -> "f*"

> data Draw = Draw
>   { gfxOperators :: [GfxOperator] -- TODO: list is the wrong thing for this
>   }

> serializeGfxOps :: [GfxOperator] -> LBS.ByteString
> serializeGfxOps ops = case ops of
>   [] -> mempty
>   [a] -> serializeGfxOp a
>   a:as -> serializeGfxOp a <> " " <> serializeGfxOps as -- TODO: rewrite with foldr

> draw
>   :: forall m. ( IsPdfM m )
>   => (Draw :> Page :> m) () -> (Page :> m) ObjRef
> draw cmds =
>   let
>     initDraw :: Draw
>     initDraw = Draw
>       { gfxOperators = []
>       }
> 
>     addGfx :: Draw -> (Page :> m) ObjRef
>     addGfx gfx = do
>       let str = serializeGfxOps $ gfxOperators gfx
>       addObj $ stream' [] $ fromLazyByteString str
> 
>   in makeWith initDraw cmds addGfx
