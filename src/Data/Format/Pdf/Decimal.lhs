> module Data.Format.Pdf.Decimal where

> import qualified Data.ByteString.Lazy as LBS
> import           Data.String
> import           Test.QuickCheck



> -- we represent decimals in the form n*10^k
> -- where n and k are arbitrary integers.
> data Decimal
>   = Decimal Int Int
>   deriving (Eq, Show)

> -- strip multiples of 10 from the integer part
> normalizeDecimal :: Decimal -> Decimal
> normalizeDecimal x@(Decimal n d) =
>   if n == 0
>     then Decimal 0 0
>     else if 0 /= (n `rem` 10)
>       then x
>       else normalizeDecimal $
>         Decimal (n `div` 10) (d+1)

> -- number of sig figs right of radix point
> -- we assume input is normalized
> decimalSigFigs :: Decimal -> Int
> decimalSigFigs (Decimal n d) = if d >= 0
>   then 0
>   else
>     let
>       -- digital log (number of digits)
>       dl = if n == 1
>         then 1
>         else ceiling $ logBase 10 $ fromIntegral n
>     in min dl (negate d)

need to detect if abs n is in the range

> -- index position of the most significant digit
> -- we assume input is normalized
> decimalMagnitude :: Decimal -> Int
> decimalMagnitude (Decimal n d) = if n == 0
>   then 0
>   else
>     let
>       dl = if n == 1
>         then 0
>         else floor $ logBase 10 $ fromIntegral n
>     in dl + d





> instance Arbitrary Decimal where
>   arbitrary = Decimal <$> arbitrary <*> arbitrary
> 
>   shrink (Decimal n d) = concat
>     [ [ Decimal n  d' | d' <- shrink d ]
>     , [ Decimal n' d  | n' <- shrink n ]
>     ]



> instance IsString Decimal where
>   fromString str =
>     let
>       (sign, nonSign) = case str of
>         '-':rest -> (-1, rest)
>         _        -> (1, str)
> 
>       (hiDigits, loDigits) =
>         break (== '.') $ dropWhile (== '0') nonSign
> 
>       (digits, radix) = case loDigits of
>         '.':rest ->
>           (reverse $ hiDigits <> rest, negate $ length rest)
>         _:_ -> error $
>           "Invalid radix point in decimal string: " <> str
>         [] -> (reverse hiDigits, 0)
> 
>       isDigit c = elem c "0123456789"
> 
>       -- strip multiples of 10 from
>       -- reversed digit string
>       normalize :: [Char] -> Int -> Decimal
>       normalize as b = case as of
>         [] -> Decimal 0 0
>         c:rest -> if c == '0'
>           then if null rest
>             then Decimal 0 0
>             else normalize rest (b+1)
>           else Decimal (sign * read (reverse as)) b
> 
>     in if all isDigit digits
>       then normalize digits radix
>       else error $
>         "Invalid digits in decimal string: " <> str



> toDecimalString :: Decimal -> String
> toDecimalString x =
>   let Decimal n d = normalizeDecimal x
>   in case compare d 0 of
>     EQ -> show n <> "."
> 
>     GT -> if n == 0
>       then "0."
>       else show n <> replicate d '0' <> "."
> 
>     LT -> if n == 0
>       then "0."
>       else
>         let
>           digits = reverse $ show $ abs n
>           sign = if n >= 0 then "" else "-"
> 
>           mk :: Int -> [Char] -> [Char] -> [Char]
>           mk k as bs = if k <= 0
>             then if null as
>               then concat ["0.", bs]
>               else concat [reverse as, ".", bs]
>             else case as of
>               [] -> concat ["0.", replicate k '0', bs]
>               a:rest -> mk (k-1) rest (a:bs)
> 
>         in sign <> mk (abs d) digits []
