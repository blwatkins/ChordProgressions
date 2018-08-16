
> module Main where
> import System.Random
> import System.IO
> import Euterpea
> import ChordGenerator
> import RandomInterpretation
> import Interpretation

> main = chordGenIO

> getInt :: String -> IO Int
> getInt str = do
>     putStr str
>     hFlush stdout
>     x <- getLine
>     return (read x)

> getBool :: String -> IO Bool
> getBool str = do
>     putStr str
>     hFlush stdout
>     x <- getLine
>     return (read x)

> getFilename :: IO String
> getFilename = do
>     putStr "Filename (Without Extension): "
>     hFlush stdout
>     x <- getLine
>     return (x ++ ".mid")

> chordGenIO :: IO ()
> chordGenIO = do
>     numPhrases <- getInt "Number of Phrases: "
>     cadenceGenSeed <- getInt "Cadence Generation Seed: "
>     let cadences = take numPhrases (infGenCadences cadenceProbs (mkStdGen cadenceGenSeed))
>     cadenceTriadSeed <- getInt "Cadence Triad Seed: "
>     let cadenceTriads = genCadenceTriads cadences cadenceRules (mkStdGen cadenceTriadSeed)
>     triadGenSeed <- getInt "Triad Generation Seed: "
>     numTriads <- getInt "Triads Per Phrase: "
>     let triadList = genPhraseTriads cadenceTriads triadRules (mkStdGen triadGenSeed) numTriads
>         triads = map reverse triadList
>     randomDurations <- getBool "Random Durations? (True/False) "
>     if randomDurations 
>     then do
>             durationSeed <- getInt "Duration Seed: "
>             let music = RandomInterpretation.interpTriadList triads (mkStdGen durationSeed)
>             filename <- getFilename
>             writeMidi filename music
>             play music
>     else do 
>             let music = Interpretation.interpTriadList triads
>             filename <- getFilename
>             writeMidi filename music
>             play music
