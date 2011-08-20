module Main where
import qualified Data.List as L
import qualified Data.Map as M

primesWheel::(Integral a)=>[a]
primesFrom::(Integral a)=>a->[a]->[a]
primesFrom n (f:r) = if (any (\x -> mod n x ==0) (L.takeWhile (\x-> (x*x)<=n) primesWheel))
                     then primesFrom (n+f) r
                     else n:(primesFrom (n+f) r)

primesWheel = let wheel = cycle [2,4,2,4,6,2,6,4,2,4,6,6,2,6,4,2,6,4,6,8,4,2,4,2,4,8,6,4,6,2,4,6,2,6,6,4,2,4,6,2,6,4,2,4,2,10,2,10]
         in 2:3:5:7:(primesFrom 11 wheel)

primesSieve::(Integral a)=>[a]
primesSieve = 2:3:5:7:11:(filter (\x->(all (\p-> 0/=(mod x p)) (takeWhile (\p-> p*p<=x) primesSieve))) [13..])
primes = primesSieve

primeFactorsOfNfactorial::(Integral a)=>a->[a]
primeFactorsOfNfactorial n = let numberOfFactors = \p -> sum (takeWhile (0/=) $ map (div n) (iterate (p*) p))
                                 primeFactors = takeWhile (<=n) (map fromInteger primes)
                             in map numberOfFactors primeFactors                      
modPow::(Integral a)=>a->a->a->a
modPow m x 0 = mod 1 m
modPow m x 1 = mod x m
modPow m x n = let nBy2 = div n 2
                   nRem2 = mod n 2
                   modPowXnBy2 = modPow m x nBy2
                   modPowXnRem2 = modPow m x nRem2
               in mod (modPowXnBy2*modPowXnBy2*modPowXnRem2) m  


calcNumberOfWaysToGroup::(Integral a)=>a->[(a,a)]->a
calcNumberOfWaysToGroup m multiplicities = L.foldl' (\cur (f,n) -> let r = 2*f+1
                                                                       modPowRn = modPow m r n 
                                                                   in (cur|*|modPowRn) |+| ((1 |-| modPowRn) |*| multiplicativeInverseOf2))
                                           1 multiplicities
    where x |*| y = (mod x m) * (mod y m)
          x |-| y = (mod x m) - (mod y m)
          x |+| y = (mod x m) + (mod y m)
          multiplicativeInverseOf2 = 500004

numberOfSolutionsToEquation::(Integral a)=>a->a
numberOfSolutionsToEquation n = let frequencies = map (\w@(x:xs)-> (x,(fromIntegral.length $ w))) $ L.group (primeFactorsOfNfactorial n)
                                in ((2 |*| (calcNumberOfWaysToGroup (fromIntegral m) frequencies)) |-| 1)
  where m=1000007
        x |*| y = (mod x m) * (mod y m)
        x |-| y = mod x m - mod y m
        
main = 
 do nstr<-getLine
    let n = read nstr
--    print $ take 100 primes
--    print $ "n : "++show n
--    print $ map (\w@(x:xs)-> (x,(fromIntegral.length $ w))) $ L.group (primeFactorsOfNfactorial n)
    print $ (numberOfSolutionsToEquation (n::Integer))
