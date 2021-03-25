> {-# LANGUAGE
>     BangPatterns
>   , OverloadedStrings
>   , ScopedTypeVariables
> #-}
> 
> module Data.Format.Pdf.Object (
>     Obj(), Name(..), ObjRef(..)
>   , bool, int, real, real', nullObj
>   , litString, hexString, name, stream, stream'
>   , array, dict, dict', indRef
> 
>   , makeFragment
> 
>   , showLazyByteString
> 
>   -- exported for testing; you can't do anything dangerous with these,
>   -- nor can you do anything useful.
>   , escapeLiteralString
>   , escapeHexString
>   , escapeName
> ) where



> import qualified Data.ByteString as SBS
> import qualified Data.ByteString.Builder as BS
> import qualified Data.ByteString.Lazy as LBS
> import qualified Data.Map.Strict as M
> import           Data.String
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as T
> import           Data.Word
> import           System.IO
> 
> import Data.Format.Pdf.Decimal
> import Data.Format.Pdf.FromBytes





Objects
================================================================================

The PDF specification defines several different sorts of /object/; these are
textual representations of data using 8-bit ASCII. The types are as follows:

0. Booleans
1. Integers
2. "Real" numbers; written in decimal notation
3. Strings; which come in two flavors, /literal/ and /hexadecimal/
4. Names; which are a restricted kind of strings used as identifiers
5. Arrays; which are heterogeneous lists of objects
6. Dictionaries; which are lists of key-value pairs with names as keys
7. Streams; which are bytestrings of arbitrary length and cannot be nested
    directly inside arrays or dictionaries
8. Null
9. Indirect References; which the spec doesn't explicitly include among the
    object types, but they act like objects in many respects

More abstract structure is defined in terms of these types, but it is possible
(and will simplify the library) if we handle the lexical and semantic content
of objects separately.

The bulk of a PDF file is essentially a list of objects expressed as a sequence
of bytes. As we will see, writing a valid PDF file requires knowing the /length
in bytes/ of any object we wish to include. In this module we define the
(Haskell) type of PDF objects and a utility function for serializing them as
byte strings along with a precomputed length.

Note that our serialization function is polymorphic over types inhabiting the
`FromBytes` class. This allows us to swap out bytestrings for different
bytestring builder implementations, depending on the needs of our application.

First we define an opaque type for object references. These are used widely;
many objects in a rendered PDF file are used by reference.

> data ObjRef
>   = ObjRef Int
>   deriving (Eq, Show)

Next we define a type for name objects. These are used as keys in dictionaries
and so it is useful to have a separate type for them. Any sequence of non-null
bytes can be used as a name and there is an escaping mechanism to allow writing
them with 7-bit ascii characters only. However in this form we assume this
escaping has not been performed.

> -- not escaped in this form
> newtype Name
>   = Name LBS.ByteString
>   deriving (Eq, Ord, Show)
> 
> instance IsString Name where
>   fromString str =
>     let x = fromString str
>     in if elem '\x00' str
>       then error $ "Invalid name: null byte in " <> str
>       else Name x

Now we define the type for PDF objects as a basic discriminated union.

> data Obj
>   = BoolObj      Bool
>   | IntObj       Int
>   | RealObj      Decimal
>   | LitStringObj LBS.ByteString
>   | HexStringObj LBS.ByteString
>   | NameObj      Name
>   | ArrayObj     [Obj]
>   | DictObj      (M.Map Name Obj)
>   | StreamObj    (M.Map Name Obj) LBS.ByteString
>   | NullObj
>   | IndRefObj    ObjRef
>   deriving (Eq, Show)

Stream objects are a bit special because they can only be defined at the "top"
level; that is, streams cannot be defined directly in the body of an array or
dictionary. Instead they are included using an indirect reference. For this
reason we need to be able to detect whether an object is a stream or not.

> isStream :: Obj -> Bool
> isStream x = case x of
>   StreamObj _ _ -> True
>   _             -> False

Lastly, to avoid making our library too dependent on the internal structure of
the `Obj` type, we export some utility functions for defining objects.

> bool :: Bool -> Obj
> bool = BoolObj
> 
> int :: Int -> Obj
> int = IntObj
> 
> real :: Int -> Int -> Obj
> real n d = RealObj $
>   normalizeDecimal $ Decimal n d
> 
> real' :: String -> Obj
> real' = RealObj . fromString
> 
> litString :: LBS.ByteString -> Obj
> litString = LitStringObj
> 
> hexString :: LBS.ByteString -> Obj
> hexString = HexStringObj
> 
> name :: LBS.ByteString -> Obj
> name = NameObj . Name
> 
> array :: [Obj] -> Obj
> array = ArrayObj
> 
> dict :: M.Map Name Obj -> Obj
> dict = DictObj
> 
> dict' :: [(Name, Obj)] -> Obj
> dict' = DictObj . M.fromList
> 
> stream :: M.Map Name Obj -> LBS.ByteString -> Obj
> stream = StreamObj
> 
> stream' :: [(Name, Obj)] -> LBS.ByteString -> Obj
> stream' dict bytes = StreamObj (M.fromList dict) bytes
> 
> nullObj :: Obj
> nullObj = NullObj
> 
> indRef :: ObjRef -> Obj
> indRef = IndRefObj





Serialization
--------------------------------------------------------------------------------

The most important thing we'll do with an object is turn it into a sequence of
bytes. The `makeFragment` function handles this task. Details of the PDF format
require us to also know the length of an object in bytes at the time we write it
to a file. We could do this by defining an object, serializing it, and counting
the bytes. However since we know the content of the object at definition time
we might as well construct the length and the bytestring simultaneously.

As an example, `escapeName` serializes name objects, taking care to escape any
bytes outside the ASCII range.

> escapeName
>   :: LBS.ByteString -> (LBS.ByteString, Int)
> escapeName str =
>   let
>     process
>       :: Word8 -> (LBS.ByteString, Int, Int)
>       -> (LBS.ByteString, Int, Int)
>     process byte (accum, !size, !num) =
>       let
>       in if num > 127
>         then error "Invalid name: longer than 127 bytes"
>         else if byte == 0x00
>           then error "Invalid name: includes a null byte (0x00)"
>           else if byte == 0x23 -- escape character (#)
>             then
>               ( LBS.cons' 0x23 $ LBS.cons' 0x32 $ LBS.cons' 0x33 accum
>               , 3 + size, 1 + num
>               )
>             else if (0x21 <= byte) && (byte <= 0x7e) -- "regular characters"
>               then (LBS.cons' byte accum, 1 + size, 1 + num)
>               else
>                 let
>                   d0 = hexDigit $ rem byte 16
>                   d1 = hexDigit $ rem (quot byte 16) 16
>                 in
>                   ( LBS.cons' 0x23 $ LBS.cons' d1 $ LBS.cons' d0 $ accum
>                   , 3 + size, 1 + num
>                   )
> 
>     (buf, size, _) = LBS.foldr process (LBS.empty, 0, 0) str
> 
>   in (buf, size)
> 
> -- convert a byte to its ASCII representation as a hex digit
> hexDigit :: Word8 -> Word8
> hexDigit octet =
>   if (0x00 <= octet) && (octet <= 0x09)
>     then octet + 0x30
>     else if (0x0a <= octet) && (octet <= 0x0f)
>       then octet + 0x57
>       else error "hexDigit: out of bounds"

We can handle hexadecimal strings similarly. Recall that this is simply an ASCII
representation of the hex digits comprising the stream. This doubles the amount
of space required to represent the data, but uses only 7-bit ASCII characters.

> escapeHexString
>   :: LBS.ByteString -> (LBS.ByteString, Int)
> escapeHexString str =
>   let
>     process
>       :: Word8 -> (LBS.ByteString, Int)
>       -> (LBS.ByteString, Int)
>     process byte (accum, !size) =
>       let
>         d0 = hexDigit $ rem byte 16
>         d1 = hexDigit $ rem (quot byte 16) 16
> 
>       in if size >= 32767
>         then error $ "Hex string exceeds size limit of 32767 bytes"
>         else (LBS.cons' d1 $ LBS.cons' d0 $ accum, 2 + size)
> 
>   in LBS.foldr process (LBS.empty, 0) str

Literal strings are a little more complex, although the structure of our
escaping function is basically the same. Note that his implementation is not
maximally space-efficient in the result; for instance, it always escapes
parentheses even when this is not necessary. The reason for that is that knowing
whether a parenthesis must be escaped requires scanning possibly to the end of
the string. Our primary design goal for this library is to make producing the
final byte string as efficient as possible for streaming, so it's better to
emit a possibly unnecessary escape character /now/ than to postpone emitting
the next byte.

> escapeLiteralString
>   :: LBS.ByteString -> (LBS.ByteString, Int)
> escapeLiteralString str =
>   if LBS.null str
>     then (str, 0)
>     else
>       let
>         process
>           :: Word8 -> (LBS.ByteString, Maybe Word8, Int)
>           -> (LBS.ByteString, Maybe Word8, Int)
>         process byte (accum, lastByte, !size) =
>           let
>             backslashEsc :: Word8 -> (LBS.ByteString, Maybe Word8, Int)
>             backslashEsc alias =
>               (LBS.cons' 0x5c $ LBS.cons' alias accum, Just byte, 2 + size)
> 
>             -- must have 0x00 <= octet <= 0x07
>             octalEsc1 :: Word8 -> (LBS.ByteString, Maybe Word8, Int)
>             octalEsc1 octet =
>               let
>                 d0 = octalDigit $ rem octet 8
>               in case lastByte of
>                 Nothing ->
>                   ( LBS.cons' 0x5c $ LBS.cons' d0 accum
>                   , Just byte, 2 + size
>                   )
>                 Just c -> if isDigit c
>                   then
>                     ( LBS.cons' 0x5c $ LBS.cons' 0x30 $
>                         LBS.cons' 0x30 $ LBS.cons' d0 accum
>                     , Just byte, 4 + size
>                     )
>                   else
>                     ( LBS.cons' 0x5c $ LBS.cons' d0 accum
>                     , Just byte, 2 + size
>                     )
> 
>             -- must have 0x08 <= octet <= 0x77 (in practice <= 0x37)
>             octalEsc2 :: Word8 -> (LBS.ByteString, Maybe Word8, Int)
>             octalEsc2 octet =
>               let
>                 d0 = octalDigit $ rem octet 8
>                 d1 = octalDigit $ rem (quot octet 8) 8
>               in case lastByte of
>                 Nothing ->
>                   ( LBS.cons' 0x5c $ LBS.cons' d1 $ LBS.cons' d0 accum
>                   , Just byte, 3 + size
>                   )
>                 Just c -> if isDigit c
>                   then
>                     ( LBS.cons' 0x5c $ LBS.cons' 0x30 $
>                         LBS.cons' d1 $ LBS.cons' d0 accum
>                     , Just byte, 4 + size
>                     )
>                   else
>                     ( LBS.cons' 0x5c $ LBS.cons' d1 $ LBS.cons' d0 accum
>                     , Just byte, 3 + size
>                     )
> 
>             octalEsc3 :: Word8 -> (LBS.ByteString, Maybe Word8, Int)
>             octalEsc3 octet =
>               let
>                 d0 = octalDigit $ rem octet 8
>                 d1 = octalDigit $ rem (quot octet 8) 8
>                 d2 = octalDigit $ rem (quot octet 64) 8
>               in
>                 ( LBS.cons' 0x5c $ LBS.cons' d2 $
>                     LBS.cons' d1 $ LBS.cons' d0 accum
>                 , Just byte, 4 + size
>                 )
> 
>             isDigit :: Word8 -> Bool
>             isDigit octet = (0x30 <= octet) && (octet <= 0x39)
> 
>             octalDigit :: Word8 -> Word8
>             octalDigit octet =
>               if (0x00 <= octet) && (octet <= 0x07)
>                 then octet + 0x30
>                 else error "octalDigit: out of bounds"
> 
>           in if size >= 32767
>             then error $ "String literal exceeds size limit of 32767 bytes"
>             else case byte of
> 
>               -- nonprintable ASCII characters
>               0x0a -> backslashEsc 0x6e -- line feed
>               0x0c -> backslashEsc 0x66 -- form feed
>               0x0d -> backslashEsc 0x72 -- carriage return
>               0x08 -> backslashEsc 0x62 -- backspace
>               0x09 -> backslashEsc 0x74 -- horizontal tab
>               0x28 -> backslashEsc 0x28 -- '('
>               0x29 -> backslashEsc 0x29 -- ')'
>               0x5c -> backslashEsc 0x5c -- '\\'
> 
>               _ -> if byte < 0x08
>                 then octalEsc1 byte -- one octal digit (maybe)
>                 else if byte < 0x20
>                   then octalEsc2 byte -- two octal digits (maybe)
>                   else if byte < 0x80
>                     -- printable ASCII characters
>                     then (LBS.cons' byte accum, Just byte, 1 + size)
>                     -- high page
>                     else octalEsc3 byte -- three octal digits (definitely)
> 
>         (buf, _, size) =
>           LBS.foldr process (LBS.empty, Nothing, 0) str
> 
>       in (buf, size)

Finally, `makeFragment` serializes an arbitrary object. The serialized data
type is polymorphic over types in the `FromBytes` class; the main reason for
this is to allow us to defer the choice of bytestring builder library.

> makeFragment
>   :: forall p. ( FromBytes p ) => Obj -> (p, Int)
> makeFragment obj = case obj of
>   BoolObj p -> case p of
>     True  -> (fromLazyByteString "true",  4)
>     False -> (fromLazyByteString "false", 5)
> 
>   IntObj n ->
>     let str = showLazyByteString n
>     in if (n < -(2^31)) || (2^31-1 < n)
>       then error "Invalid integer value: outside of [-2^31, 2^31-1]"
>       else (fromLazyByteString str, fromIntegral $ LBS.length str)
> 
>   RealObj d ->
>     let str = toDecimalString d
>     in
>       if 5 < decimalSigFigs d
>         then error "Invalid real number: more than 5 fractional sig figs"
>         else if 38 <= abs (decimalMagnitude d)
>           then error "Invalid real number: abs val outside (1e-38, 1e38)"
>           else
>             ( fromLazyByteString $ LBS.fromStrict $
>                 T.encodeUtf8 $ T.pack str
>             , length str
>             )
> 
>   -- we assume no escaping has been done yet
>   LitStringObj str ->
>     let (esc, size) = escapeLiteralString str
>     in
>       ( mconcat
>         [ fromLazyByteString "("
>         , fromLazyByteString esc
>         , fromLazyByteString ")"
>         ]
>       , size + 2
>       )
> 
>   HexStringObj str ->
>     let (esc, size) = escapeHexString str
>     in
>       ( mconcat
>         [ fromLazyByteString "<"
>         , fromLazyByteString esc
>         , fromLazyByteString ">"
>         ]
>       , size + 2
>       )
> 
>   NameObj (Name str) ->
>     let (esc, size) = escapeName str
>     in
>       ( fromLazyByteString $ LBS.cons' 0x2f str
>       , 1 + size
>       )
> 
>   ArrayObj objs -> case objs of
>     [] -> (fromLazyByteString "[]", 2)
>     o:rest ->
>       let
>         (head, headSize) = makeFragment o
> 
>         process
>           :: Obj -> (p, Int)
>           -> (p, Int)
>         process obj (accum, size) =
>           let (str, size') = makeFragment obj
>           in if isStream obj
>             then error "Invalid array: streams cannot be stored in arrays"
>             else
>               ( fromLazyByteString " " <> str <> accum
>               , size' + 1 + size
>               )
> 
>         (tail, tailSize) = foldr process (mempty, 0) rest
> 
>       in
>         ( mconcat
>             [ fromLazyByteString "["
>             , head, tail
>             , fromLazyByteString "]"
>             ]
>         , headSize + tailSize + 2
>         )
> 
>   DictObj dict ->
>     let
>       process
>         :: Name -> Obj -> (p, Int)
>         -> (p, Int)
>       process name obj (accum, size) =
>         let
>           (nameStr, nameLen) = makeFragment (NameObj name)
>           (objStr, objLen) = makeFragment obj
>         in if isStream obj
>           then error "Invalid dict: streams cannot be stored in dicts"
>           else
>             ( mconcat
>                 [ accum
>                 , fromLazyByteString " "
>                 , nameStr
>                 , fromLazyByteString " "
>                 , objStr
>                 ]
>             , nameLen + objLen + 2
>             )
> 
>       (str, size) = M.foldrWithKey process (mempty, 0) dict
> 
>     in
>       ( mconcat
>           [ fromLazyByteString "<<"
>           , str
>           , fromLazyByteString " >>"
>           ]
>       , size + 5
>       )
> 
>   StreamObj dict bytes ->
>     let
>       streamSize = fromIntegral $ LBS.length bytes
>       dict' = M.insert "Length" (int streamSize) dict
> 
>       (dictStr, dictSize) = makeFragment (DictObj dict')
>     in
>       ( mconcat
>           [ dictStr
>           , fromLazyByteString "\nstream\n"
>           , fromLazyByteString bytes
>           , fromLazyByteString "\nendstream"
>           ]
>       , dictSize + streamSize + 18
>       )
> 
>   NullObj -> (fromLazyByteString "null", 4)
> 
>   IndRefObj (ObjRef n) ->
>     let
>       digits = showLazyByteString n
>       numDigits = fromIntegral $ LBS.length digits
>     in ( fromLazyByteString $ digits <> " 0 R", numDigits + 4 )



> -- helper function
> showLazyByteString
>   :: ( Show a ) => a -> LBS.ByteString
> showLazyByteString =
>   LBS.fromStrict . T.encodeUtf8 . T.pack . show
