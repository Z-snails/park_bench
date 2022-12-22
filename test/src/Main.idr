module Main

import ParkBench

||| This is an intentionally bad definition of `fib` designed to be slow
fib : Nat -> Nat
fib 0 = 1
fib 1 = 1
fib (S (S k)) = fib (S k) + fib k

main : IO ()
main = do
    for_ [5,10..40] $ \x =>
        bench "fib \{show x}" fib x >>= printLn
