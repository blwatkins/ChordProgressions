-- Chord Sequence Generator
-- Interpretation Module
-- Interprets triad symbols (see ChordTypes.lhs) into music

> module Interpretation (interpTriadList) where
> import System.Random
> import Euterpea
> import ChordTypes
> import TriadInterpretation

> interpTriadList :: [[Triad]] -> Bool -> Music Pitch
> interpTriadList [] _ = rest 0
> interpTriadList [t] isMajor = interpLastTriads t isMajor
> interpTriadList (tl : tls) isMajor = 
>     let m = interpPhraseTriads tl isMajor
>         ms = interpTriadList tls isMajor
>     in m :+: ms

> interpPhraseTriads :: [Triad] -> Bool -> Music Pitch
> interpPhraseTriads [] _ = rest 0
> interpPhraseTriads [t1, t2] isMajor = 
>     let c1 = genChord t1 CadenceChord isMajor
>         c2  = genChord t2 CadenceChord isMajor
>     in c1 :+: c2
> interpPhraseTriads (t : ts) isMajor =
>     let c = genChord t PhraseChord isMajor
>         chords = interpPhraseTriads ts isMajor
>     in c :+: chords

> interpLastTriads :: [Triad] -> Bool -> Music Pitch
> interpLastTriads [] _ = rest 0
> interpLastTriads [t1, t2] isMajor = 
>     let c1 = genChord t1 LastChord isMajor
>         c2 = genChord t2 LastChord isMajor
>     in c1 :+: c2
> interpLastTriads (t : ts) isMajor =
>     let c = genChord t PhraseChord isMajor
>         chords = interpLastTriads ts isMajor
>     in c :+: chords

> genChord :: Triad -> ChordType -> Bool -> Music Pitch
> genChord t PhraseChord isMajor = 
>     let pitches = map (+60) (chordPitches t isMajor)
>         ms = map (\x -> Prim (Note qn (pitch x))) pitches
>     in chord ms
> genChord t CadenceChord isMajor = 
>     let pitches = map (+60) (chordPitches t isMajor)
>         ms = map (\x -> Prim (Note hn (pitch x))) pitches
>     in chord ms
> genChord t LastChord isMajor =
>     let pitches = map (+60) (chordPitches t isMajor)
>         ms = map (\x -> Prim (Note wn (pitch x))) pitches
>     in chord ms
