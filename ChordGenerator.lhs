-- Chord Sequence Generator
-- ChordGenerator Module
-- Cadence rules, cadence generation, and triad generation

> module ChordGenerator where
> import System.Random
> import Rule
> import Probability
> import ChordTypes
> import TriadRules

> cadenceProbs :: [Rule Cadence]
> cadenceProbs = [
>     (0.25, Authentic),
>     (0.25, Plagal),
>     (0.25, Half), 
>     (0.25, Deceptive) ]

> infGenCadences :: [Rule Cadence] -> StdGen -> [Cadence]
> infGenCadences rules g =
>     let (r, g2) = randomR (0.0, 1.0 :: Double) g
>         c = choose rules r
>     in [c] ++ infGenCadences rules g2

=====================================================================

> cadenceRules :: [Rule (Cadence, [Triad])]
> cadenceRules = [
>     (0.50, (Authentic, [I, V])),
>     (0.50, (Authentic, [I, VII])),
>     (1.00, (Plagal, [I, IV])),
>     (0.35, (Half, [V, I])),
>     (0.15, (Half, [V, II])),
>     (0.15, (Half, [V, III])),
>     (0.15, (Half, [V, IV])),
>     (0.15, (Half, [V, VI])),
>     (0.05, (Half, [V, VII])),
>     (0.20, (Deceptive, [II, V])),
>     (0.20, (Deceptive, [III, V])),
>     (0.20, (Deceptive, [IV, V])),
>     (0.30, (Deceptive, [VI, V])),
>     (0.10, (Deceptive, [VII, V])) ]

> genCadenceTriads :: [Cadence] -> [Rule (Cadence, [Triad])] -> StdGen -> [[Triad]]
> genCadenceTriads [] _ _ = []
> genCadenceTriads (c : cs) rules g = 
>     let cRules = filter (\(p, r) -> c == (fst r)) rules
>         (r, g2) = randomR (0.0, 1.0 :: Double) g
>         rule = choose cRules r
>         triads = snd rule
>     in (triads :  genCadenceTriads cs rules g2)

=====================================================================

> genPhraseTriads :: [[Triad]] -> [Rule (Triad, [Triad])] -> StdGen -> Int -> [[Triad]]
> genPhraseTriads [] _ _ _ = []
> genPhraseTriads (tl : tls) rules g num = 
>     let (tlTriads, g2) = genTriads tl rules g num
>         tlsTriads = genPhraseTriads tls rules g2 num
>     in tlTriads : tlsTriads

> genTriads :: [Triad] -> [Rule (Triad, [Triad])] -> StdGen -> Int -> ([Triad], StdGen)
> genTriads [] _ g _ = ([], g)
> genTriads triads rules g num =
>     let (newTriads, g2) = genNextTriad triads rules g
>     in if num > 0 then genTriads newTriads rules g2 (num - 1) else (triads, g2)

> genNextTriad :: [Triad] -> [Rule (Triad, [Triad])] -> StdGen -> ([Triad], StdGen)
> genNextTriad [] _ g = ([], g)
> genNextTriad [t] rules g = 
>     let tRules = filter (\(p, r) -> t == (fst r)) rules
>         (r, g2) = randomR (0.0, 1.0 :: Double) g
>         rule = choose tRules r
>         triads = snd rule
>     in (triads, g2)
> genNextTriad (t : ts) rules g = 
>     let (triads, g2) = genNextTriad ts rules g
>     in (t : triads, g2)
