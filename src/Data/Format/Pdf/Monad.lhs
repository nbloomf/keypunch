> {-# LANGUAGE
>     RankNTypes
>   , TypeFamilies
>   , InstanceSigs
>   , TypeOperators
>   , FlexibleContexts
>   , OverloadedStrings
>   , ScopedTypeVariables
> #-}
> 
> module Data.Format.Pdf.Monad (
>     PdfSt()
>   , withPdfVersion
> 
>   , IsPdfM(..)
> 
>   , addObj
>   , withObjRef
>   , setRoot
> 
>   , PdfM()
> 
>   , buildPdf
>   , printLazyByteStringPdf
>   , writeLazyByteStringPdf
> ) where



> import           Control.Monad.Fail
> import           Control.Monad.IO.Class
> import qualified Data.ByteString.Lazy as LBS
> import           Data.IORef
> 
> import Data.Format.Pdf.FromBytes
> import Data.Format.Pdf.Object





Lexical Structure
================================================================================

When I first looked at it, the PDF file format looked like a complete mess.
After spending some more time with it... well it's still a mess, but some
structure does emerge.

We will divide the PDF format into two major pieces: the /lexical structure/
and the /semantic structure/. The former is more fundamental such that we can
handle it on its own and implement the latter as a layer on top, more or less
independently.

At its core, the /lexical/ structure of a PDF-formatted document consists of
four parts:

0. A header
1. A list of objects, each labeled by an indirect reference
2. A cross reference section, with one entry per object in the list
3. A trailer

At first glance, we can define the PDF file by listing its constituent objects.
This is complicated by a few constraints on the format.

- Indirect reference numbers must be globally unique.
- Many objects need to refer to other objects by reference, and the digraph of
    object references may (in fact must) have cycles.
- For each object in the list, we need to know the /number of bytes/ which
    precede it in the file.

On top of these, recall our overall design constraint of producing bytes
incrementally. With this in mind, the naive implementation - constructing a list
of objects and serializing them one by one - has some apparent problems. If an
object at index position i refers to an object at j, but j also refers to i,
then we can't start emitting bytes for i until we know the reference for j and
swapping their relative order doesn't help. Fortunately, the PDF spec has a nice
property we can exploit to circumvent this: objects do not have to be listed "in
order". That is, every object in the list has a numeric reference ID, but the
objects do not need to be sorted by ID. In fact this is exactly what the cross
reference section is for. So we can "reserve" a numeric ID, serialize objects
which refer to it, and then use it at a later time.

We define a monad, `PdfM`, which abstracts all this detail away. The primary
interface of this monad is two functions: `addObj`, which adds a new object
to the list, and `withObjRef`, which handles the reserve-and-use juggling. We
implement this as a state monad, where the state tracks the serialized list of
objects and the serialized cross reference list, as well as the number and size
(in bytes) of the object list so far. In this way we can start emitting bytes
for the next object immediately, regardless of any reference cycle it may be in.

The state we track is fairly simple.

> data PdfSt p = PdfSt
>   { pdfVersion :: PdfVersion -- for future use (maybe)
>   , objects    :: p          -- stream of object bytes
>   , crossRef   :: p          -- stream of cross reference bytes
>   , objCount   :: !Int       -- number of objects emitted so far
>   , objSize    :: !Int       -- number of bytes emitted so far
>   , rootObj    :: ObjRef     -- required in the trailer
>   }
> 
> data PdfVersion
>   = PdfVersion_1_7

We implement `PdfM` as a reader monad with an IORef as the read-only context.
This part is standard stuff.

> newtype PdfM p a = PdfM
>   { unPdfM :: IORef (PdfSt p) -> IO a }
> 
> instance Functor (PdfM p) where
>   fmap f (PdfM pdf) = PdfM $
>     \ref -> do
>       a <- pdf ref
>       return (f $! a)
>   {-# INLINE fmap #-}
> 
> instance Applicative (PdfM p) where
>   pure a = PdfM $ \_ -> return a
>   {-# INLINE pure #-}
> 
>   (PdfM f) <*> (PdfM x) = PdfM $
>     \ref -> do
>       f' <- f ref
>       x' <- x ref
>       return (f' $! x')
>   {-# INLINE (<*>) #-}
> 
> instance Monad (PdfM p) where
>   return = pure
>   {-# INLINE return #-}
> 
>   (PdfM x) >>= f = PdfM $
>     \ref -> do
>       a <- x ref
>       unPdfM (f a) ref
>   {-# INLINE (>>=) #-}
> 
> instance MonadIO (PdfM p) where
>   liftIO x = PdfM $ \_ -> x
>   {-# INLINE liftIO #-}
> 
> instance MonadFail (PdfM p) where
>   fail msg = PdfM $ \_ -> fail msg
>   {-# INLINE fail #-}

We implement the state monad primitives using a custom class. This will come in
handy later when we want to layer more specialized DSLs on top of the base PDF
monad. Note the use of an associated type family to represent the type of the
byte stream.

> class
>   ( MonadIO m, FromBytes (Emits m)
>   ) => IsPdfM m
>   where
>     type Emits m :: * -- type of the emitted stream of bytes
> 
>     -- extract a value from the current state
>     getsPdfSt
>       :: (PdfSt (Emits m) -> a) -> m a
> 
>     -- alter the state
>     mutatesPdfSt
>       :: (PdfSt (Emits m) -> PdfSt (Emits m)) -> m ()

Of course, `PdfM` is itself an instance of `IsPdfM` in the expected way.

> instance
>   ( FromBytes p
>   ) => IsPdfM (PdfM p)
>   where
>     type Emits (PdfM p) = p
> 
>     getsPdfSt
>       :: (PdfSt p -> a) -> PdfM p a
>     getsPdfSt f = PdfM $ \ref -> do
>       st <- readIORef ref
>       return (f $! st)
>     {-# INLINE getsPdfSt #-}
> 
>     mutatesPdfSt
>       :: (PdfSt p -> PdfSt p) -> PdfM p ()
>     mutatesPdfSt f = PdfM $ \ref -> do
>       modifyIORef' ref f
>     {-# INLINE mutatesPdfSt #-}

We can now implement the basic PDF monad interface in terms of `IsPdfM`. First,
the operation of adding an object to the stream. This function takes an `Obj`,
adds it and its cross reference data to the stream, and returns the indirect
object reference corresponding to the object.

> addObj
>   :: ( IsPdfM m ) => Obj -> m ObjRef
> addObj obj = do
>   count <- getsPdfSt objCount
>   size <- getsPdfSt objSize
> 
>   let
>     theCount = showLazyByteString count
> 
>     -- this is where we use the combined bytes/count computation
>     -- from Data.Format.Pdf.Object
>     (frag, fragSize) = makeFragment obj
> 
>   mutatesPdfSt $ \st -> st
>     { objects = mconcat
>         [ objects st
>         , fromLazyByteString theCount
>         , fromLazyByteString " 0 obj\n", frag
>         , fromLazyByteString "\nendobj\n"
>         ]
> 
>     , objCount = 1 + count
> 
>     -- add object size to running byte count. magic number 15 is
>     -- the extra bytes used for 'obj', 'endobj', etc.
>     , objSize =
>         let countDigits = fromIntegral $ LBS.length theCount
>         in fragSize + size + countDigits + 15
> 
>     -- append object address to cross reference table
>     , crossRef = mconcat
>         [ crossRef st
>         , fromLazyByteString $ padDec10 size <> " 00000 n \n"
>         ]
>     }
> 
>   return (ObjRef count)

`withObjRef` is our primitive for reserving an object reference, using it in the
construction of some other objects, and then creating an object with that ID.

> withObjRef
>   :: ( IsPdfM m )
>   => (a -> Obj) -> (ObjRef -> m a) -> m ObjRef
> withObjRef make prepare = do
>   -- extract the current object count and cross reference table.
>   -- the entries in the cross reference table /do/ have to come in order
>   -- (this is not strictly true, but it's much simpler to assume it is)
>   count <- getsPdfSt objCount
>   xref <- getsPdfSt crossRef
>   let objRef = ObjRef count -- this is the "reserved" object reference
> 
>   -- update the next available reference and empty the cross reference
>   -- table. 
>   mutatesPdfSt $ \st -> st
>     { objCount = 1 + count
>     , crossRef = mempty
>     }
> 
>   -- do stuff with the reserved reference. my intuition here is that /prepare/
>   -- is a monadic computation returning some configuration data we need to
>   -- construct an object, like the indirect references to its children.
>   config <- prepare objRef
> 
>   let
>     theCount = showLazyByteString count
>     countDigits = fromIntegral $ LBS.length theCount
>     (frag, fragSize) = makeFragment (make config)
>   size <- getsPdfSt objSize
> 
>   -- now we can put the "old" state and "new" state back together again
>   -- in the right order.
>   mutatesPdfSt $ \st -> st
>     -- the reserved object can go anywhere (in particular, at the end of
>     -- the object stream)
>     { objects = mconcat
>         [ objects st
>         , fromLazyByteString theCount
>         , fromLazyByteString " 0 obj\n", frag
>         , fromLazyByteString "\nendobj\n"
>         ]
> 
>     , objCount = 1 + count
>     , objSize = fragSize + size + countDigits + 15
> 
>     -- insert the reserved indirect reference in the right place
>     , crossRef = mconcat
>         [ xref        -- the "old" cross reference table
>         , fromLazyByteString $ padDec10 size <> " 00000 n \n"
>         , crossRef st -- the "new" cross reference table
>         ]
>     }
> 
>   return objRef

`padDec10` is a helper for padding numbers to 10 decimal digits; this format
is required for byte counts of cross reference entries in the PDF spec.

> padDec10 :: Int -> LBS.ByteString
> padDec10 = LBS.reverse . LBS.take 10 .
>   (<> "0000000000") . LBS.reverse . showLazyByteString
> {-# INLINE padDec10 #-}





Constructing the actual file
--------------------------------------------------------------------------------

`buildPdf` runs a value in the PDF monad and assembles the final stream of
bytes. This function is /not/ polymorphic over the `IsPdfM` class; the intention
is that any extra context layered on top of `PdfM` has been discharged before we
assemble the file.

Note that `buildPdf` does not take an initial value for the PDF state, but a
/function/ on that state. The reason for this is that the PDF spec is very
fiddly, so we only expose a handful of well behaved options.

> buildPdf
>   :: forall p. ( FromBytes p )
>   => (PdfSt p -> PdfSt p) -> PdfM p () -> IO p
> buildPdf config (PdfM pdf) = do
>   let
>     initPdfSt :: PdfSt p
>     initPdfSt = PdfSt
>       { pdfVersion = PdfVersion_1_7
>       , rootObj    = ObjRef 1
>       , objects    = mempty
>       , objCount   = 1
>       , objSize    = 0
>       , crossRef   = fromLazyByteString
>           "0000000000 65535 f \n" -- required by the spec
>       }
> 
>     setInitialObjSize
>       :: PdfSt p -> PdfSt p
>     setInitialObjSize st =
>       let
>         header = headerString (pdfVersion st)
>         size = fromIntegral $ LBS.length header
>       in st
>         { objects = fromLazyByteString header
>         , objSize = size
>         }
> 
>   ref <- newIORef $ setInitialObjSize $ config initPdfSt
>   pdf ref
>   finalSt <- readIORef ref
> 
>   let
>     PdfSt
>       { pdfVersion = pdfVersion'
>       , rootObj    = rootObj'
>       , objects    = objects'
>       , objCount   = objCount'
>       , objSize    = objSize'
>       , crossRef   = crossRef'
>       } = finalSt
> 
>   return $ mconcat
>     [ objects'
>     , fromLazyByteString "xref\n"
>     , fromLazyByteString $ "0 " <> showLazyByteString objCount' <> "\n"
>     , crossRef'
>     , trailerString rootObj' objCount' objSize'
>     ]

The following are helper functions for constructing the header and trailer.

> headerString
>   :: PdfVersion -> LBS.ByteString
> headerString version = case version of
>   PdfVersion_1_7 -> "%PDF-1.7\n"
> 
> trailerString :: ( FromBytes p ) => ObjRef -> Int -> Int -> p
> trailerString (ObjRef root) count size = mconcat
>   [ fromLazyByteString "trailer\n"
>   , fromLazyByteString $ mconcat
>       [ "<< /Size ", showLazyByteString count, " /Root "
>       , showLazyByteString root, " 0 R >>\n"
>       ]
>   , fromLazyByteString "startxref\n"
>   , fromLazyByteString $ showLazyByteString size
>   , fromLazyByteString "\n%%EOF"
>   ]

As of this writing, there's only one interesting part of the initial state we
might consider changing -- the spec version -- but even that is not very useful
in practice because this library only produces files against spec version 1.7.

> withPdfVersion
>   :: PdfVersion -> PdfSt p -> PdfSt p
> withPdfVersion version st = st
>   { pdfVersion = version }

One little piece of the semantic structure reaches into the lexical structure.
Each PDF must have one /root/ object, and the reference for this object must
be included in the trailer. Since we have no way of knowing what the root object
will be, we maintain it in the PDF state and allow the root object to declare
itself.

> setRoot
>   :: ( FromBytes p )
>   => ObjRef -> PdfM p ()
> setRoot ref = do
>   mutatesPdfSt $ \st -> st
>     { rootObj = ref
>     }

We end with some utility functions. `buildPdf` is polymorphic over byte stream
types. However, when testing in ghci, it is a huge pain to have to specify this
type all the time, since there is usually very little reason /not/ to just
default to lazy bytestrings. So we expose two helpers for this purpose; one
prints a raw PDF file to the console, and the other writes it to a file.

> -- handy for testing
> printLazyByteStringPdf
>   :: (PdfSt LBS.ByteString -> PdfSt LBS.ByteString)
>   -> PdfM LBS.ByteString () -> IO ()
> printLazyByteStringPdf config pdf = do
>   buildPdf config pdf >>= printToConsole
>   putStrLn "" -- real PDFs do not end with a newline
> 
> -- handy for testing
> writeLazyByteStringPdf
>   :: FilePath -> (PdfSt LBS.ByteString -> PdfSt LBS.ByteString)
>   -> PdfM LBS.ByteString () -> IO ()
> writeLazyByteStringPdf path config pdf = do
>   pdf <- buildPdf config pdf
>   LBS.writeFile path pdf
