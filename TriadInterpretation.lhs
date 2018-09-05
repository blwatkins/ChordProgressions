-- Chord Sequence Generator
-- TriadInterpretation Module
-- Interprets triad symbols (see ChordTypes.lhs) as a triad of absolute pitches
-- Pitches are transposed later in the interpretation process

> module TriadInterpretation (chordPitches) where
> import ChordTypes
> import Euterpea

> chordPitches :: Triad -> Bool -> [AbsPitch]
> chordPitches t isMajor = 
>     let inversion =
>             if isMajor then majorInv
>             else minorInv   
>         rule = filter (\(tn, ps) -> t == tn) inversion
>         pitches = snd (rule !! 0)
>     in pitches

> majorInv :: [(Triad, [AbsPitch])]
> majorInv = [
>     (I,   [0, 4, 7]),
>     (II,  [2, 5, 9]),
>     (III, [(-1), 4, 7]),
>     (IV,  [0, 5, 9]),
>     (V,   [(-1), 2, 7]),
>     (VI,  [(-3), 0, 4]),
>     (VII, [(-1), 2, 5]) ]

> minorInv :: [(Triad, [AbsPitch])]
> minorInv = [
>     (I,   [0, 3, 7]),
>     (II,  [2, 5, 8]),
>     (III, [(-2), 3, 7]),
>     (IV,  [0, 5, 8]),
>     (V,   [(-2), 2, 7]),
>     (VI,  [0, 3, 8]),
>     (VII, [(-2), 2, 5]) ]
