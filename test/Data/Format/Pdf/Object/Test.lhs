> {-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
> module Data.Format.Pdf.Object.Test (
>     test_Object
> ) where

> import qualified Data.ByteString.Lazy as LBS
> import           Data.String
> import           Test.Tasty
> import           Test.Tasty.QuickCheck

> import Data.Format.Pdf.Object



> test_Object :: TestTree
> test_Object = testGroup "Object"
>   [ test_escapeLiteralString
>   , test_escapeHexString
>   , test_escapeName
>   , test_makeFragment
>   ]



> test_escapeLiteralString :: TestTree
> test_escapeLiteralString = testGroup "escapeLiteralString"
>   [ testGroup "Examples"
>     [ testGroup "No Escape"
>       [ testProperty "escapeLiteralString ''" $
>           (escapeLiteralString "") == ("", 0)
> 
>       , testProperty "escapeLiteralString 'a'" $
>           (escapeLiteralString "a") == ("a", 1)
> 
>       , testProperty "escapeLiteralString 'abc'" $
>           (escapeLiteralString "abc") == ("abc", 3)
>       ]
> 
>     , testGroup "Character Escape"
>       [ testProperty "escapeLiteralString '\\'" $
>           (escapeLiteralString "\\") == ("\\\\", 2)
> 
>       , testProperty "escapeLiteralString '\\a'" $
>           (escapeLiteralString "\\a") == ("\\\\a", 3)
> 
>       , testProperty "escapeLiteralString 'a\\'" $
>           (escapeLiteralString "a\\") == ("a\\\\", 3)
> 
>       , testProperty "escapeLiteralString '\\\\'" $
>           (escapeLiteralString "\\\\") == ("\\\\\\\\", 4)
> 
>       , testProperty "escapeLiteralString '('" $
>           (escapeLiteralString "(") == ("\\(", 2)
> 
>       , testProperty "escapeLiteralString ')'" $
>           (escapeLiteralString ")") == ("\\)", 2)
> 
>       , testProperty "escapeLiteralString '(abc)'" $
>           (escapeLiteralString "(abc)") == ("\\(abc\\)", 7)
>       ]
> 
>     , testGroup "Octal Escape"
>       [ testProperty "escapeLiteralString '\\x00'" $
>           (escapeLiteralString "\x00") == ("\\0", 2)
> 
>       , testProperty "escapeLiteralString '\\x00\\&a'" $
>           (escapeLiteralString "\x00\&a") == ("\\0a", 3)
> 
>       , testProperty "escapeLiteralString '\\x00\\&0'" $
>           (escapeLiteralString "\x00\&0") == ("\\0000", 5)
> 
>       , testProperty "escapeLiteralString 'a\\x00'" $
>           (escapeLiteralString "a\x00") == ("a\\0", 3)
> 
>       , testProperty "escapeLiteralString '\\x07'" $
>           (escapeLiteralString "\x07") == ("\\7", 2)
> 
>       , testProperty "escapeLiteralString '\\x07\\&a'" $
>           (escapeLiteralString "\x07\&a") == ("\\7a", 3)
> 
>       , testProperty "escapeLiteralString '\\x07\\&0'" $
>           (escapeLiteralString "\x07\&0") == ("\\0070", 5)
> 
>       , testProperty "escapeLiteralString 'a\\x07'" $
>           (escapeLiteralString "a\x07") == ("a\\7", 3)
> 
>       , testProperty "escapeLiteralString '\\x0b'" $
>           (escapeLiteralString "\x0b") == ("\\13", 3)
> 
>       , testProperty "escapeLiteralString '\\x0b\\&a'" $
>           (escapeLiteralString "\x0b\&a") == ("\\13a", 4)
> 
>       , testProperty "escapeLiteralString '\\x0b\\&0'" $
>           (escapeLiteralString "\x0b\&0") == ("\\0130", 5)
> 
>       , testProperty "escapeLiteralString 'a\\x0b'" $
>           (escapeLiteralString "a\x0b") == ("a\\13", 4)
> 
>       , testProperty "escapeLiteralString '\\xff'" $
>           (escapeLiteralString "\xff") == ("\\377", 4)
> 
>       , testProperty "escapeLiteralString '\\xff\\&a'" $
>           (escapeLiteralString "\xff\&a") == ("\\377a", 5)
> 
>       , testProperty "escapeLiteralString '\\xff\\&0'" $
>           (escapeLiteralString "\xff\&0") == ("\\3770", 5)
> 
>       , testProperty "escapeLiteralString 'a\\xff'" $
>           (escapeLiteralString "a\xff") == ("a\\377", 5)
>       ]
>     ]
>   ]



> test_escapeHexString :: TestTree
> test_escapeHexString = testGroup "escapeHexString"
>   [ testGroup "Examples"
>     [ testProperty "escapeHexString '\\x00'" $
>         (escapeHexString "\x00") == ("00", 2)
> 
>     , testProperty "escapeHexString '\\x00\\x01'" $
>         (escapeHexString "\x00\x01") == ("0001", 4)
> 
>     , testProperty "escapeHexString '\\xff\\x80'" $
>         (escapeHexString "\xff\x80") == ("ff80", 4)
>     ]
>   ]



> test_escapeName :: TestTree
> test_escapeName = testGroup "escapeName"
>   [ testProperty "escapeName 'a'" $
>       (escapeName "a") == ("a", 1)
> 
>   , testProperty "escapeName 'ab'" $
>       (escapeName "ab") == ("ab", 2)
> 
>   , testProperty "escapeName '#'" $
>       (escapeName "#") == ("#23", 3)
> 
>   , testProperty "escapeName '\\x1a'" $
>       (escapeName "\x1a") == ("#1a", 3)
>   ]



> test_makeFragment :: TestTree
> test_makeFragment = testGroup "makeFragment"
>   [ testGroup "Examples"
>     [ testGroup "Bool"
>       [ testProperty "Bool: true" $
>           (==)
>             (makeFragment $ bool True)
>             ("true" :: LBS.ByteString, 4)
> 
>       , testProperty "Bool: false" $
>           (==)
>             (makeFragment $ bool False)
>             ("false" :: LBS.ByteString, 5)
>       ]
> 
>     , testGroup "Integer"
>       [ testProperty "Int: 0" $
>           (==)
>             (makeFragment $ int 0)
>             ("0" :: LBS.ByteString, 1)
> 
>       , testProperty "Int: 1337" $
>           (==)
>             (makeFragment $ int 1337)
>             ("1337" :: LBS.ByteString, 4)
> 
>       , testProperty "Int: -27" $
>           (==)
>             (makeFragment $ int (-27))
>             ("-27" :: LBS.ByteString, 3)
>       ]
> 
>     , testGroup "Real"
>       [ testProperty "Real: 0." $
>           (==)
>             (makeFragment $ real 0 0)
>             ("0." :: LBS.ByteString, 2)
>       ]
> 
>     , testGroup "Literal String"
>       [ testProperty "LitString: ''" $
>           (==)
>             (makeFragment $ litString "")
>             ("()" :: LBS.ByteString, 2)
> 
>       , testProperty "LitString: 'a'" $
>           (==)
>             (makeFragment $ litString "a")
>             ("(a)" :: LBS.ByteString, 3)
> 
>       , testProperty "LitString: '\'" $
>           (==)
>             (makeFragment $ litString "\\")
>             ("(\\\\)" :: LBS.ByteString, 4)
>       ]
> 
>     , testGroup "Hex String"
>       [ testProperty "HexString: ''" $
>           (==)
>             (makeFragment $ hexString "")
>             ("<>" :: LBS.ByteString, 2)
>       ]
> 
>     , testGroup "Name"
>       [ testProperty "Name: ''" $
>           (==)
>             (makeFragment $ name "")
>             ("/" :: LBS.ByteString, 1)
> 
>       , testProperty "Name: 'a'" $
>           (==)
>             (makeFragment $ name "a")
>             ("/a" :: LBS.ByteString, 2)
>       ]
> 
>     , testGroup "Array"
>       [ testProperty "Array: []" $
>           (==)
>             (makeFragment $ array [])
>             ("[]" :: LBS.ByteString, 2)
> 
>       , testProperty "Array: [0 1]" $
>           (==)
>             (makeFragment $ array [int 0, int 1])
>             ("[0 1]" :: LBS.ByteString, 5)
>       ]
> 
>     , testGroup "Dictionary"
>       [ testProperty "Dictionary: << >>" $
>           (==)
>             (makeFragment $ dict' [])
>             ("<< >>" :: LBS.ByteString, 5)
> 
>       , testProperty "Dictionary: << /a 0 >>" $
>           (==)
>             (makeFragment $ dict' [("a", int 0)])
>             ("<< /a 0 >>" :: LBS.ByteString, 10)
>       ]
> 
>     , testGroup "Stream"
>       [ testProperty "Stream: a" $
>           (==)
>             (makeFragment $ stream' [] "a")
>             ("<< /Length 1 >>\nstream\na\nendstream" :: LBS.ByteString, 34)
>       ]
>     ]
>   ]

 > 
 >      {-   , testGroup "Properties"
 >     [ testProperty "length is accurate" $
 >          \(str :: LBS.ByteString) ->
 >           let (esc, size) = escapeLiteralString str
 >           in (LBS.length esc) == (fromIntegral size)
 >     ] -}
