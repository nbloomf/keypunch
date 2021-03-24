> {-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
> module Data.Format.Pdf.Decimal.Test (
>     test_Decimal
> ) where

> import qualified Data.ByteString.Lazy as LBS
> import           Data.String
> import           Test.Tasty
> import           Test.Tasty.QuickCheck

> import Data.Format.Pdf.Decimal

> test_Decimal :: TestTree
> test_Decimal = testGroup "Decimal"
>   [ testGroup "Examples: toDecimalString"
>     [ testProperty "toDecimalString 0 0" $
>         (toDecimalString (Decimal 0 0)) == "0."
> 
>     , testProperty "toDecimalString 1 0" $
>         (toDecimalString (Decimal 1 0)) == "1."
> 
>     , testProperty "toDecimalString (-1) 0" $
>         (toDecimalString (Decimal (-1) 0)) == "-1."
> 
>     , testProperty "toDecimalString 0 1" $
>         (toDecimalString (Decimal 0 1)) == "0."
> 
>     , testProperty "toDecimalString 1 1" $
>         (toDecimalString (Decimal 1 1)) == "10."
> 
>     , testProperty "toDecimalString 1 2" $
>         (toDecimalString (Decimal 1 2)) == "100."
> 
>     , testProperty "toDecimalString 0 (-1)" $
>         (toDecimalString (Decimal 0 (-1))) == "0."
> 
>     , testProperty "toDecimalString 1 (-1)" $
>         (toDecimalString (Decimal 1 (-1))) == "0.1"
> 
>     , testProperty "toDecimalString 1 (-2)" $
>         (toDecimalString (Decimal 1 (-2))) == "0.01"
> 
>     , testProperty "toDecimalString (-1) (-1)" $
>         (toDecimalString (Decimal (-1) (-1))) == "-0.1"
> 
>     , testProperty "toDecimalString (-1) (-2)" $
>         (toDecimalString (Decimal (-1) (-2))) == "-0.01"
> 
>     , testProperty "toDecimalString 10 0" $
>         (toDecimalString (Decimal 10 0)) == "10."
> 
>     , testProperty "toDecimalString 10 (-1)" $
>         (toDecimalString (Decimal 10 (-1))) == "1."
>     ]
> 
>   , testGroup "Examples: fromString"
>     [ testProperty "fromString '0.0'" $
>         (fromString "0.0") == (Decimal 0 0)
> 
>     , testProperty "fromString '-0.0'" $
>         (fromString "-0.0") == (Decimal 0 0)
> 
>     , testProperty "fromString '1.0'" $
>         (fromString "1.0") == (Decimal 1 0)
> 
>     , testProperty "fromString '10.0'" $
>         (fromString "10.0") == (Decimal 1 1)
> 
>     , testProperty "fromString '0.1'" $
>         (fromString "0.1") == (Decimal 1 (-1))
> 
>     , testProperty "fromString '-1.0'" $
>         (fromString "-1.0") == (Decimal (-1) 0)
> 
>     , testProperty "fromString '.0'" $
>         (fromString ".0") == (Decimal 0 0)
> 
>     , testProperty "fromString '0.'" $
>         (fromString "0.") == (Decimal 0 0)
> 
>     , testProperty "fromString '.1'" $
>         (fromString ".1") == (Decimal 1 (-1))
> 
>     , testProperty "fromString '1.'" $
>         (fromString "1.") == (Decimal 1 0)
> 
>     , testProperty "fromString '01.'" $
>         (fromString "01.") == (Decimal 1 0)
>     ]
> 
>   , testGroup "decimalSigFigs"
>     [ testProperty "decimalSigFigs (Decimal 1 0)" $
>         (decimalSigFigs (Decimal 1 0)) == 0
> 
>     , testProperty "decimalSigFigs (Decimal 1 (-1))" $
>         (decimalSigFigs (Decimal 1 (-1))) == 1
> 
>     , testProperty "decimalSigFigs (Decimal 1 (-2))" $
>         (decimalSigFigs (Decimal 1 (-2))) == 1
> 
>     , testProperty "decimalSigFigs (Decimal 11 (-1))" $
>         (decimalSigFigs (Decimal 11 (-1))) == 1
> 
>     , testProperty "decimalSigFigs (Decimal 11 (-2))" $
>         (decimalSigFigs (Decimal 11 (-2))) == 2
> 
>     , testProperty "decimalSigFigs (Decimal 11 (-3))" $
>         (decimalSigFigs (Decimal 11 (-3))) == 2
> 
>     , testProperty "decimalSigFigs (Decimal 101 (-1))" $
>         (decimalSigFigs (Decimal 101 (-1))) == 1
> 
>     , testProperty "decimalSigFigs (Decimal 101 (-2))" $
>         (decimalSigFigs (Decimal 101 (-2))) == 2
> 
>     , testProperty "decimalSigFigs (Decimal 101 (-3))" $
>         (decimalSigFigs (Decimal 101 (-3))) == 3
>     ]
> 
>   , testGroup "decimalMagnitude"
>     [ testProperty "decimalMagnitude (Decimal 1 0)" $
>         (decimalMagnitude (Decimal 1 0)) == 0
> 
>     , testProperty "decimalMagnitude (Decimal 1 1)" $
>         (decimalMagnitude (Decimal 1 1)) == 1
> 
>     , testProperty "decimalMagnitude (Decimal 1 (-1))" $
>         (decimalMagnitude (Decimal 1 (-1))) == (-1)
> 
>     , testProperty "decimalMagnitude (Decimal 1 2)" $
>         (decimalMagnitude (Decimal 1 2)) == 2
> 
>     , testProperty "decimalMagnitude (Decimal 2 0)" $
>         (decimalMagnitude (Decimal 2 0)) == 0
> 
>     , testProperty "decimalMagnitude (Decimal 2 1)" $
>         (decimalMagnitude (Decimal 2 1)) == 1
> 
>     , testProperty "decimalMagnitude (Decimal 2 (-1))" $
>         (decimalMagnitude (Decimal 2 (-1))) == (-1)
> 
>     , testProperty "decimalMagnitude (Decimal 2 2)" $
>         (decimalMagnitude (Decimal 2 2)) == 2
> 
>     , testProperty "decimalMagnitude (Decimal 11 0)" $
>         (decimalMagnitude (Decimal 11 0)) == 1
> 
>     , testProperty "decimalMagnitude (Decimal 11 1)" $
>         (decimalMagnitude (Decimal 11 1)) == 2
> 
>     , testProperty "decimalMagnitude (Decimal 11 2)" $
>         (decimalMagnitude (Decimal 11 2)) == 3
> 
>     , testProperty "decimalMagnitude (Decimal 11 (-1))" $
>         (decimalMagnitude (Decimal 11 (-1))) == 0
>     ]
> 
>   , testGroup "Properties"
>     -- fromString . toDecimalString == normalizeDecimal
>     [ testProperty "fromString . toDecimalString == id" $
>         \(d :: Decimal) ->
>           (==) (normalizeDecimal d) (fromString $ toDecimalString d)
> 
>     -- always has a decimal point
>     , testProperty "elem '.' (toDecimalString d) == True" $
>         \(d :: Decimal) ->
>           elem '.' (toDecimalString d)
> 
>     -- when d == 0 the output has a radix point with no decimal digits
>     , testProperty "toDecimalString n 0 == show n <> \".\"" $
>         \(n :: Int) ->
>           (==)
>             (toDecimalString (Decimal n 0))
>             (show n <> ".")
> 
>     -- when d > 0 the least significant digit is 0
>     , testProperty "d > 0 ==> last (init (toDecimalString n d)) == '0'" $
>         \(n :: Int) (Positive d :: Positive Int) ->
>           (==)
>             (last (init (toDecimalString (Decimal n d))))
>             ('0')
>     ]
>   ]
