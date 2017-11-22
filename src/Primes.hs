module Primes where

maxN :: Int
maxN = 10000

primes :: [Int]
primes = sieve [2..maxN]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
    where noFactors = filter (not . (== 0) . (`mod` nextPrime)) rest

isPrime :: Int -> Either PrimeError Bool
isPrime n
    | n < 2 = Left InvalidValue
    | n > maxN = Left TooLarge
    | otherwise = Right (n `elem` primes)

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
    show TooLarge = "Value exceed max bound"
    show InvalidValue = "Value is not a valid candidate for prime checking"

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next:primes) = if n `mod` next == 0
                                     then next:unsafePrimeFactors (n `div` next) (next:primes)
                                     else unsafePrimeFactors n primes

primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2 = Nothing
               | n >= length primes = Nothing
               | otherwise = Just (unsafePrimeFactors n primesLessThanN)
               where primesLessThanN = filter (<= n) primes