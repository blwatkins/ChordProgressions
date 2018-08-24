-- Chord Sequence Generator
-- RandomInterpretation Module
-- Interprets triad symbols (see ChordTypes.lhs) into music
-- Allows triads to be interpreted with a random quarter note or eigth note duration

> module RandomInterpretation (interpTriadList) where
> import System.Random
> import Euterpea
> import ChordTypes
> import TriadInterpretation

> interpTriadList :: [[Triad]] -> StdGen -> Music Pitch
> interpTriadList [] g = rest 0
> interpTriadList [t] g = 
>     let (m, g2) = interpLastTriads t g
>     in m
> interpTriadList (tl : tls) g = 
>     let (m, g2) = interpPhraseTriads tl g
>         ms = interpTriadList tls g2
>     in m :+: ms

> interpPhraseTriads :: [Triad] -> StdGen -> (Music Pitch, StdGen)
> interpPhraseTriads [] g = (rest 0, g)
> interpPhraseTriads [t1, t2] g = 
>     let (c1, g2) = genChord t1 CadenceChord g
>         (c2, g3) = genChord t2 CadenceChord g2
>     in (c1 :+: c2, g3)
> interpPhraseTriads (t : ts) g =
>     let (c, g2) = genChord t PhraseChord g
>         (chords, g3) = interpPhraseTriads ts g2
>     in (c :+: chords, g3)

> interpLastTriads :: [Triad] -> StdGen -> (Music Pitch, StdGen)
> interpLastTriads [] g = (rest 0, g)
> interpLastTriads [t1, t2] g = 
>     let (c1, g2) = genChord t1 LastChord g
>         (c2, g3) = genChord t2 LastChord g2
>     in (c1 :+: c2, g3)
> interpLastTriads (t : ts) g =
>     let (c, g2) = genChord t PhraseChord g
>         (chords, g3) = interpLastTriads ts g2
>     in (c :+: chords, g3)

> genChord :: Triad -> ChordType -> StdGen -> (Music Pitch, StdGen)
> genChord t PhraseChord g = 
>     let pitches = map (+60) (chordPitches t)
>         (dur, g2) = chooseDur g
>         ms = map (\x -> Prim (Note dur (pitch x))) pitches
>     in (chord ms, g2)
> genChord t CadenceChord g = 
>     let pitches = map (+60) (chordPitches t)
>         ms = map (\x -> Prim (Note hn (pitch x))) pitches
>     in (chord ms, g)
> genChord t LastChord g =
>     let pitches = map (+60) (chordPitches t)
>         ms = map (\x -> Prim (Note wn (pitch x))) pitches
>     in (chord ms, g)

> chooseDur :: StdGen -> (Dur, StdGen)
> chooseDur g = 
>     let (num, g2) = randomR (0, 1) g
>         durs = [en, qn]
>     in (durs !! num, g2)
