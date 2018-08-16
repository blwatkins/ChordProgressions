
> module Interpretation (interpTriadList) where
> import System.Random
> import Euterpea
> import ChordTypes
> import TriadInterpretation

> interpTriadList :: [[Triad]] -> Music Pitch
> interpTriadList [] = rest 0
> interpTriadList [t] = interpLastTriads t
> interpTriadList (tl : tls) = 
>     let m = interpPhraseTriads tl
>         ms = interpTriadList tls
>     in m :+: ms

> interpPhraseTriads :: [Triad] -> Music Pitch
> interpPhraseTriads [] = rest 0
> interpPhraseTriads [t1, t2] = 
>     let c1 = genChord t1 CadenceChord
>         c2  = genChord t2 CadenceChord 
>     in c1 :+: c2
> interpPhraseTriads (t : ts) =
>     let c = genChord t PhraseChord
>         chords = interpPhraseTriads ts
>     in c :+: chords

> interpLastTriads :: [Triad] -> Music Pitch
> interpLastTriads [] = rest 0
> interpLastTriads [t1, t2] = 
>     let c1 = genChord t1 LastChord
>         c2 = genChord t2 LastChord
>     in c1 :+: c2
> interpLastTriads (t : ts) =
>     let c = genChord t PhraseChord
>         chords = interpLastTriads ts
>     in c :+: chords

> genChord :: Triad -> ChordType -> Music Pitch
> genChord t PhraseChord = 
>     let pitches = map (+60) (chordPitches t)
>         ms = map (\x -> Prim (Note qn (pitch x))) pitches
>     in chord ms
> genChord t CadenceChord = 
>     let pitches = map (+60) (chordPitches t)
>         ms = map (\x -> Prim (Note hn (pitch x))) pitches
>     in chord ms
> genChord t LastChord =
>     let pitches = map (+60) (chordPitches t)
>         ms = map (\x -> Prim (Note wn (pitch x))) pitches
>     in chord ms
