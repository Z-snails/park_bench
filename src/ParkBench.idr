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
    totalTime : Clock Duration
    time : Clock Duration

showPlural : String -> Nat -> String
showPlural str 1 = str
showPlural str _ = str ++ "s"

getOpsPerSec : (runs : Nat) -> (time : Clock Duration) -> Double
getOpsPerSec runs time =
    let runs = cast {to = Double} runs
        timeNs = cast {to = Double} $ toNano' time
     in (runs * cast nanoInSec) / timeNs

export
Show (Timed a) where
    show timed = """
    \{timed.description} took \{showTime 0 5 timed.time}
      \{show timed.runs} \{showPlural "run" timed.runs} (total time: \{showTime 0 5 timed.totalTime})
    """
    -- TODO: fix this
    --   \{notQuiteStandardForm 3 $ getOpsPerSec timed.runs timed.time} operations per second
    -- """

||| A black-box function to call a closure.
|||
||| This treats evaluating a closure as a side effect
%noinline
call : (a -> b) -> a -> IO b
call f x = fromPrim $ \w => MkIORes (f x) w

||| Time an `IO` action
export
timeIO : String -> IO a -> IO (Timed a)
timeIO description act = do
    start <- clockTime Monotonic
    result <- act
    end <- clockTime Monotonic
    let time = timeDifference end start
    pure $ MkTimed {description, result, runs = 1, totalTime = time, time}

repeat : IO a -> Nat -> a -> IO a
repeat act Z acc = pure acc
repeat act (S k) acc = act >>= repeat act k

%inline
divide : Clock Duration -> Nat -> Clock Duration
divide c runs = fromNano' $ toNano' c `div` cast runs

||| Default duration of each benchmark
public export
defaultTime : Clock Duration
defaultTime = makeDuration 1 0

||| Benchmark an IO operation.
|||
||| This runs the given action repeatedly, until the target time has been reached.
export
benchIO :
    {default defaultTime targetTime : Clock Duration} ->
    (description : String) ->
    (act : IO a) ->
    IO (Timed a)
benchIO description act = do
    warmup <- timeIO description act
    if warmup.time > targetTime
        then pure warmup
        else do
            let runs = cast {to=Nat} $ toNano' targetTime `div` toNano' warmup.time
            let False = runs <= 1
                | True => pure warmup
            res <- timeIO description $ repeat act runs warmup.result
            pure $ { runs := runs, time $= (`divide` runs) } res

||| Benchmark a function with a given input.
|||
||| This runs the function repeatedly until enough time has passed.
||| The default time to benchmark is 1 second, but this can be changed
||| with `targetTime`.
export
bench :
    {default defaultTime targetTime : Clock Duration} ->
    (description : String) ->
    (a -> b) ->
    a ->
    IO (Timed b)
bench description f x = benchIO description (call f x)
