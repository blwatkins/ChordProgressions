-- Chord Sequence Generator
-- TriadInterpretation Module
-- Interprets triad symbols (see ChordTypes.lhs) as a triad of absolute pitches
-- Pitches are transposed later in the interpretation process

> module TriadInterpretation (chordPitches) where
> import ChordTypes
> import Euterpea

> chordPitches :: Triad -> [AbsPitch]
> chordPitches t = 
>     let rule = filter (\(tn, ps) -> t == tn) majorInv
>         pitches = snd (rule !! 0)
>     in pitches

> majorInv :: [(Triad, [AbsPitch])]
> majorInv = [
>     (I, [0, 4, 7]),
>     (II, [2, 5, 9]),
>     (III, [(-1), 4, 7]),
>     (IV, [0, 5, 9]),
>     (V, [(-1), 2, 7]),
>     (VI, [(-3), 0, 4]),
>     (VII, [(-1), 2, 5]) ]
