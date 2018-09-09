-- Chord Sequence Generator
-- RandomInterpretation Module
-- Interprets triad symbols (see ChordTypes.lhs) into music
-- Allows triads to be interpreted with a random quarter note or eigth note duration

> module RandomInterpretation (interpTriadList) where
> import System.Random
> import Euterpea
> import ChordTypes
> import TriadInterpretation

> interpTriadList :: [[Triad]] -> Bool -> StdGen  -> Music Pitch
> interpTriadList [] _ _ = rest 0
> interpTriadList [t] isMajor g = 
>     let (m, g2) = interpLastTriads t isMajor g
>     in m
> interpTriadList (tl : tls) isMajor g = 
>     let (m, g2) = interpPhraseTriads tl isMajor g
>         ms = interpTriadList tls isMajor g2
>     in m :+: ms

> interpPhraseTriads :: [Triad] -> Bool -> StdGen -> (Music Pitch, StdGen)
> interpPhraseTriads [] _ g = (rest 0, g)
> interpPhraseTriads [t1, t2] isMajor g = 
>     let (c1, g2) = genChord t1 CadenceChord isMajor g
>         (c2, g3) = genChord t2 CadenceChord isMajor g2
>     in (c1 :+: c2, g3)
> interpPhraseTriads (t : ts) isMajor g =
>     let (c, g2) = genChord t PhraseChord isMajor g
>         (chords, g3) = interpPhraseTriads ts isMajor g2
>     in (c :+: chords, g3)

> interpLastTriads :: [Triad] -> Bool -> StdGen -> (Music Pitch, StdGen)
> interpLastTriads [] _ g = (rest 0, g)
> interpLastTriads [t1, t2] isMajor g = 
>     let (c1, g2) = genChord t1 LastChord isMajor g
>         (c2, g3) = genChord t2 LastChord isMajor g2
>     in (c1 :+: c2, g3)
> interpLastTriads (t : ts) isMajor g =
>     let (c, g2) = genChord t PhraseChord isMajor g
>         (chords, g3) = interpLastTriads ts isMajor g2
>     in (c :+: chords, g3)

> genChord :: Triad -> ChordType -> Bool -> StdGen -> (Music Pitch, StdGen)
> genChord t PhraseChord isMajor g = 
>     let pitches = map (+60) (chordPitches t isMajor)
>         (dur, g2) = chooseDur g
>         ms = map (\x -> Prim (Note dur (pitch x))) pitches
>     in (chord ms, g2)
> genChord t CadenceChord isMajor g = 
>     let pitches = map (+60) (chordPitches t isMajor)
>         ms = map (\x -> Prim (Note hn (pitch x))) pitches
>     in (chord ms, g)
> genChord t LastChord isMajor g =
>     let pitches = map (+60) (chordPitches t isMajor)
>         ms = map (\x -> Prim (Note wn (pitch x))) pitches
>     in (chord ms, g)

> chooseDur :: StdGen -> (Dur, StdGen)
> chooseDur g = 
>     let (num, g2) = randomR (0, 1) g
>         durs = [en, qn]
>     in (durs !! num, g2)
