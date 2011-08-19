module Main where
import qualified Data.List as L

primes::(Integral a)=>[a]

primesFrom::(Integral a)=>a->[a]->[a]
primesFrom n (f:r) = if (any (\x -> mod n x ==0) (L.takeWhile (\x-> (x*x)<=n) primes))
                     then primesFrom (n+f) r
                     else n:(primesFrom (n+f) r)

primes = let wheel = cycle [2,4,2,4,6,2,6,4,2,4,6,6,2,6,4,2,6,4,6,8,4,2,4,2,4,8,6,4,6,2,4,6,2,6,6,4,2,4,6,2,6,4,2,4,2,10,2,10]
         in [2,3,5,7] ++ primesFrom 11 wheel

primeFactorsOfNfactorial::(Integral a)=>a->[(a,a)]
primeFactorsOfNfactorial n = let numberOfFactors = \p -> (p,sum (takeWhile (0/=) $ map (div n) (iterate (p*) p)))
                                 primeFactors = takeWhile (<=n) primes   
                             in map numberOfFactors primeFactors                      

calcNumberOfWaysToGroup::(Integral a)=>a->[a]->a
calcNumberOfWaysToGroup m multiplicities = L.foldl' (\cur f->mod ((2*cur-1)*f+cur) m) 1 multiplicities
    where x |*| y = (mod x m) * (mod y m)
          x |+| y = mod (x + y) m

numberOfSolutionsToEquation::(Integral a)=>a->a
numberOfSolutionsToEquation n = let frequencies = (map snd (primeFactorsOfNfactorial n))
                                in mod ((2 * (calcNumberOfWaysToGroup (fromIntegral 1000007) frequencies)) - 1) 1000007

main = 
 do nstr<-getLine
    let n = read nstr
    print $ numberOfSolutionsToEquation (n::Integer)
