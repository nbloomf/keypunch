> {-# LANGUAGE OverloadedStrings #-}
> module Main where

> import Data.Format.Pdf

> main :: IO ()
> main = do
>   example000
>   example001



> example000 :: IO ()
> example000 = do
>   let path = "examples/example000-empty-document.pdf"
>   writeLazyByteStringPdf path id $ do
>     withCatalog $ do
>       withPages $ do
>         addPage $ do
>           return ()



> example001 :: IO ()
> example001 = do
>   let path = "examples/example001-hello-world.pdf"
>   writeLazyByteStringPdf path id $ do
> 
>     fontRef <- addObj $ dict'
>       [ ( "Type",     name "Font" )
>       , ( "Subtype",  name "Type1" )
>       , ( "BaseFont", name "Helvetica" )
>       ]
> 
>     withCatalog $ do
>       withPages $ do
>         fontName <- addFont fontRef
>         addPage $ do
>           ref <- draw $ do
>             write $ do
>               setFont fontName 12
>               advanceAndIndent 288 720
>               showText "Hello World!"
>           addContent ref
