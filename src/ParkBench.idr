module ParkBench

import Data.String
import System.Clock

log : Double -> Double -> Double
log base x = log x / log base -- shh don't tell anyone about this

record FloatingParts a where
    constructor MkFloatingParts
    exp : a
    mant : a

%inline
floatingParts : (base : Double) -> (x : Double) -> FloatingParts Double
floatingParts base x =
    let exp = floor $ log base x
        mant = x * pow base (-exp)
     in MkFloatingParts { exp, mant }

roundToDp : (dps : Nat) -> Double -> Double
roundToDp dps x =
    let scale = pow 10 $ cast dps
     in floor (x * scale) / scale

roundToSf : (sfs : Nat) -> Double -> Double
roundToSf sfs x =
    let scale = pow 10 $ cast (sfs `minus` 1) - (floatingParts 10 x).exp
     in floor (x * scale) / scale

standardForm : (sigFigs : Nat) -> Double -> String
standardForm sigFigs x =
    let MkFloatingParts { exp, mant } = floatingParts 10 x
        mant = roundToDp (sigFigs `minus` 1) mant
     in padRight (sigFigs + 1) '0' (show mant) ++ "e" ++ show (cast {to=Integer} exp)

notQuiteStandardForm : (sigFigs : Nat) -> Double -> String
notQuiteStandardForm sigFigs x =
    let MkFloatingParts { exp, mant } = floatingParts 10 x
        mant = roundToDp (sigFigs `minus` 1) mant
     in if -1 <= exp && exp <= 3
        then
            let exp = cast {to=Integer} exp
             in show $ roundToSf sigFigs x
        else padRight (sigFigs + 1) '0' (show mant) ++ "e" ++ show (cast {to=Integer} exp)

testTime : Clock Duration
testTime = makeDuration 2 0

nanoInSec : Integer
nanoInSec = 1000000000

toNano' : Clock type -> Integer
toNano' (MkClock s ns) = s * nanoInSec + ns

fromNano' : {type : _} -> Integer -> Clock type
fromNano' nanos =
    let s = nanos `div` nanoInSec
        ns = nanos `mod` nanoInSec
     in MkClock s ns

public export
record Timed a where
    constructor MkTimed
    result : a
    description : String
    runs : Nat
    time : Clock Duration

showPlural : String -> Nat -> String
showPlural str 1 = str
showPlural str _ = str ++ "s"

getOpsPerSec : (runs : Nat) -> (time : Clock Duration) -> Double
getOpsPerSec runs time =
    let runs = cast {to = Double} runs
        timeNs = cast {to = Double} $ toNano' time
        timeSec = timeNs / cast nanoInSec
     in runs / timeSec

export
Show (Timed a) where
    show timed = """
    \{timed.description} took \{showTime 0 5 timed.time}
      \{show timed.runs} \{showPlural "run" timed.runs}
      \{notQuiteStandardForm 3 $ getOpsPerSec timed.runs timed.time} operations per second
    """

||| A black-box function to call a closure
%noinline
call : (a -> b) -> a -> IO b
call f x = pure $ f x

timeIO : String -> IO a -> IO (Timed a)
timeIO description act = do
    start <- clockTime Monotonic
    result <- act
    end <- clockTime Monotonic
    let time = timeDifference end start
    pure $ MkTimed {description, result, runs = 1, time}

callNTimes : (a -> b) -> a -> b -> Nat -> IO b
callNTimes f x acc Z = pure acc
callNTimes f x acc (S k) = do
    acc <- call f x
    callNTimes f x acc k

%inline
divide : Clock Duration -> Nat -> Clock Duration
divide c runs = fromNano' $ toNano' c `div` cast runs

export
bench : String -> (a -> b) -> a -> IO (Timed b)
bench description f x = do
    warmup <- timeIO description $ call f x
    if warmup.time > testTime
        then pure warmup
        else do
            let runs = cast {to=Nat} $ toNano' testTime `div` toNano' warmup.time
            let False = runs <= 1
                | True => pure warmup
            res <- timeIO description $ callNTimes f x warmup.result runs
            pure $ { runs := runs, time $= (`divide` runs) } res
