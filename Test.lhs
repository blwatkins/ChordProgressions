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

-- Cadence Generation

> cadences :: [Cadence]
> cadences = take 4 (infGenCadences cadenceProbs (mkStdGen 100))

> cadenceTriads :: [[Triad]]
> cadenceTriads = genCadenceTriads cadences cadenceRules (mkStdGen 101)

> cadenceTriadsShow :: [[Triad]]
> cadenceTriadsShow = map reverse cadenceTriads

-- Major Progression

> majorTriadList :: [[Triad]]
> majorTriadList = genPhraseTriads cadenceTriads majorTriadRules (mkStdGen 102) 4

> majorTriads :: [[Triad]]
> majorTriads = map reverse majorTriadList

> majorMusic :: Music Pitch
> majorMusic = Interpretation.interpTriadList majorTriads True

-- Minor Progression

> minorTriadList :: [[Triad]]
> minorTriadList = genPhraseTriads cadenceTriads minorTriadRules (mkStdGen 102) 4

> minorTriads :: [[Triad]]
> minorTriads = map reverse minorTriadList

> minorMusic :: Music Pitch
> minorMusic = Interpretation.interpTriadList minorTriads False

-- Write MIDI File

> writeMajor = writeMidi "major.mid" majorMusic
> writeMinor = writeMidi "minor.mid" minorMusic
