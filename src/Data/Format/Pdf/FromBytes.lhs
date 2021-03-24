> module Data.Format.Pdf.FromBytes (
>     FromBytes(..)
> ) where

> import qualified Data.ByteString as SBS
> import qualified Data.ByteString.Builder as BS
> import qualified Data.ByteString.Lazy as LBS
> import           System.IO



> class ( Monoid p ) => FromBytes p where
>   fromLazyByteString :: LBS.ByteString -> p
>   printToConsole :: p -> IO ()
>   writeToFile :: FilePath -> p -> IO ()



> instance FromBytes SBS.ByteString where
>   fromLazyByteString = LBS.toStrict
>   printToConsole = SBS.putStr
>   writeToFile = SBS.writeFile
> 
> instance FromBytes LBS.ByteString where
>   fromLazyByteString = id
>   printToConsole = LBS.putStr
>   writeToFile = LBS.writeFile
> 
> instance FromBytes BS.Builder where
>   fromLazyByteString = BS.lazyByteString
> 
>   printToConsole p = do
>     hSetBinaryMode stdout True
>     hSetBuffering stdout (BlockBuffering Nothing)
>     BS.hPutBuilder stdout p
> 
>   writeToFile path p = do
>     withFile path WriteMode $ \h -> do
>       hSetBinaryMode h True
>       hSetBuffering h (BlockBuffering Nothing)
>       BS.hPutBuilder h p
