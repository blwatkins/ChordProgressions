-- Chord Sequence Generator
-- Test Module
-- To be used for testing new functionality

> module Test where
> import ChordGenerator
> import ChordTypes
> import System.Random
> import Interpretation
> import TriadRules
> import Euterpea

> cadences :: [Cadence]
> cadences = take 4 (infGenCadences cadenceProbs (mkStdGen 100))

> cadenceTriads :: [[Triad]]
> cadenceTriads = genCadenceTriads cadences cadenceRules (mkStdGen 101)

> cadenceTriadsShow :: [[Triad]]
> cadenceTriadsShow = map reverse cadenceTriads

> triadList :: [[Triad]]
> triadList = genPhraseTriads cadenceTriads triadRules (mkStdGen 102) 4

> triads :: [[Triad]]
> triads = map reverse triadList

> majorMusic :: Music Pitch
> majorMusic = Interpretation.interpTriadList triads True

> minorMusic :: Music Pitch
> minorMusic = Interpretation.interpTriadList triads False

> writeMajor = writeMidi "major.mid" majorMusic
> writeMinor = writeMidi "minor.mid" minorMusic
