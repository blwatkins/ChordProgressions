-- Chord Sequence Generator
-- MUI Program Module
-- Musical User Interface (MUI) takes input from the program to generate chord sequences

> {-# LANGUAGE Arrows #-}
> module Main where
> import System.Random
> import Euterpea
> import HSoM.MUI
> import HSoM
> import FRP.UISF
> import IOWidgets
> import ChordTypes
> import ChordGenerator
> import RandomInterpretation
> import Interpretation

> main = chordGenMUI

> myParams = defaultMUIParams{uiTitle="Chord Generator", uiSize=(800, 800)}

> stringToInt :: String -> Int
> stringToInt "" = 0
> stringToInt str = read str

> numPhrasesPanel :: UISF () (Int)
> numPhrasesPanel = proc _ -> do
>     numPhrases <- hiSlider 1 (0, 10) 0 -< ()
>     display -< numPhrases
>     returnA -< (numPhrases)

> cadencesPanel :: UISF () ([Cadence])
> cadencesPanel = proc _ -> do
>     label "Number of Phrases: " -< ()
>     (numPhrases) <- leftRight $ numPhrasesPanel -< ()
>     label "Cadence Seed: " -< ()
>     cadenceGenSeedStr <- textbox "" -< Nothing
>     let cadenceGenSeed = stringToInt cadenceGenSeedStr
>         cadences = take numPhrases (infGenCadences cadenceProbs (mkStdGen cadenceGenSeed))
>     label "Cadences: " -< ()
>     display -< cadences
>     returnA -< (cadences)

> cadenceTriadsPanel :: UISF ([Cadence]) ([[Triad]])
> cadenceTriadsPanel = proc (cadences) -> do
>     label "Cadence Triad Seed: " -< ()
>     cadenceTriadSeedStr <- textbox "" -< Nothing
>     let cadenceTriadSeed = stringToInt cadenceTriadSeedStr
>         cadenceTriadsOrig = genCadenceTriads cadences cadenceRules (mkStdGen cadenceTriadSeed)
>         cadenceTriads = map reverse cadenceTriadsOrig
>     label "Cadence Triads: " -< ()
>     display -< cadenceTriads
>     returnA -< cadenceTriadsOrig

> numTriadsPanel :: UISF () (Int)
> numTriadsPanel = proc _ -> do
>     numTriads <- hiSlider 1 (0, 10) 0 -< ()
>     display -< numTriads
>     returnA -< (numTriads)

> triadGenerationPanel :: UISF ([[Triad]]) ([[Triad]])
> triadGenerationPanel = proc (cadenceTriads) -> do
>     label "Number of Triads: " -< ()
>     (numTriads) <- leftRight $ numTriadsPanel -< ()
>     label "Triad Seed: " -< ()
>     triadGenSeedStr <- textbox "" -< Nothing
>     let triadGenSeed = stringToInt triadGenSeedStr
>         triadList = genPhraseTriads cadenceTriads triadRules (mkStdGen triadGenSeed) numTriads
>         triads = map reverse triadList
>     label "Triads: " -< ()
>     display -< triads
>     returnA -< triads

> randomnessPanel :: UISF () (Bool, Int)
> randomnessPanel = proc _ -> do
>     random <- checkbox "Random Durations" False -< ()
>     label "Random Durations Seed: " -< ()
>     randomSeedStr <- textbox "" -< Nothing
>     let randomSeed = stringToInt randomSeedStr
>     returnA -< (random, randomSeed)

> pitchClassList :: [String]
> pitchClassList = ["C", "Cs", "D", "Ds", "E", "F", "Fs", "G", "Gs", "A", "As", "B"]

> octavePanel :: UISF () Int
> octavePanel = proc _ -> do
>     octave <- hiSlider 1 (1, 7) 4 -< ()
>     display -< octave
>     returnA -< octave  

> transpositionPanel :: UISF () (Int, Int)
> transpositionPanel = proc _ -> do
>     label "Key: " -< ()
>     pitchClassIndex <- leftRight $ radio pitchClassList 0 -< ()
>     label "Octave: " -< ()
>     octave <- leftRight $ octavePanel -< ()
>     returnA -< (pitchClassIndex, octave)

> interpMusic :: [[Triad]] -> Bool -> Int -> Music Pitch
> interpMusic triads random randomSeed =
>     if random then RandomInterpretation.interpTriadList triads (mkStdGen randomSeed)
>     else Interpretation.interpTriadList triads

> transposeMusic :: Music Pitch -> Int -> Int -> Music Pitch
> transposeMusic music pitchClassIndex octave =
>     let transposeValue = pitchClassIndex + ((octave - 4) * 12)
>     in transpose transposeValue music

> outputMIDIFile :: UISF (SEvent (FilePath, Music Pitch)) ()
> outputMIDIFile = ioWidget1 output where
>     output (filePath, music) = do
>         writeMidi filePath music

> chordGenMUI :: IO ()
> chordGenMUI = runMUI myParams $ proc _ -> do
>     devId <- selectOutput -< ()
>     (cadences) <- title "Cadence Generation" $ cadencesPanel -< ()
>     (cadenceTriads) <- title "Cadence Triad Generation" $ cadenceTriadsPanel -< cadences
>     (triads) <- title "Triad Generation" $ triadGenerationPanel -< cadenceTriads
>     (random, randomSeed) <- title "Randomness" $ leftRight $ randomnessPanel -< ()
>     (pitchClassIndex, octave) <- title "Transposition" $ transpositionPanel -< ()
>     let music = interpMusic triads random randomSeed
>         musicTransposed = transposeMusic music pitchClassIndex octave
>     label "Filename: " -< ()
>     filename <- textbox "" -< Nothing
>     output <- edge <<< button "Output to Midi" -< ()
>     outputMIDIFile -< fmap (const (filename, musicTransposed)) output
>     play <- edge <<< button "Play" -< ()
>     let messages = musicToMsgs' defParams musicTransposed
>         bufferOp = if play == Nothing then NoBOp else AppendToBuffer messages
>     midiOutB -< (devId, bufferOp)
>     returnA -< ()
