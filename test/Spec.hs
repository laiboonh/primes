import Test.QuickCheck
import Primes
import Data.Maybe
import Data.Either
 
prop_validPrimesOnly val = if val < 2 || val > maxN
                           then isLeft result
                           else isRight result
                           where result = isPrime val

prop_primesArePrime val = case result of
                            Right True -> length divisors == 0
                            _ -> True
                          where result = isPrime val
                                divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_nonPrimesAreComposite val = case result of
                                  Right False -> length divisors > 0
                                  _ -> True
                                 where result = isPrime val
                                       divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_factorsMakeOriginal val = if result == Nothing
                               then True
                               else product (fromJust result) == val
                               where result = primeFactors val

prop_allFactorsPrime val = if result == Nothing
                           then True
                           else all (isRight) resultsPrime
                           where result = primeFactors val
                                 resultsPrime = map isPrime (fromJust result)

main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs {maxSuccess = 1000} prop_primesArePrime
  quickCheckWith stdArgs {maxSuccess = 1000} prop_nonPrimesAreComposite
  quickCheck prop_factorsMakeOriginal
  quickCheck prop_allFactorsPrime
  
