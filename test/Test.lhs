> {-# LANGUAGE
>     OverloadedStrings
>   , ScopedTypeVariables
> #-}
> 
> module Main where
> 
> import qualified Data.ByteString.Lazy as LBS
> import           Data.String
> import           System.Environment
> import           Test.Tasty
> import           Test.Tasty.Ingredients.Basic
> import           Test.Tasty.QuickCheck
> import           Test.Tasty.Runners

> import Data.Format.Pdf

> import Data.Format.Pdf.Decimal.Test
> import Data.Format.Pdf.Object.Test



> main :: IO ()
> main = do
>   setEnv "TASTY_HIDE_SUCCESSES" "TRUE"
>   defaultMain $
>     localOption (NumThreads 6) $
>     localOption (QuickCheckTests 10000) $
>     testGroup "Data.Format.Pdf"
>       [ test_Decimal
>       , test_Object
>       ]
