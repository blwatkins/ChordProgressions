-- Chord Sequence Generator
-- ChordGenerator Module
-- Rule type, cadence rules, triad rules, and triad generation

> module ChordGenerator where
> import System.Random
> import Probability
> import ChordTypes

=====================================================================

> type Rule a = (Prob, a)

=====================================================================

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

-- missing III, VII

> triadRules :: [Rule (Triad, [Triad])]
> triadRules = [
>     (0.95, (I, [I, V])),
>     (0.05, (I, [I, VII])),
>     (0.40, (II, [II, VI])),
>     (0.40, (II, [II, V])),
>     (0.10, (II, [II, VII])),
>     (0.10, (II, [II, I])),
>     (0.90, (III, [III, I])),
>     (0.10, (III, [III, VII])),
>     (0.40, (IV, [IV, VI])),
>     (0.40, (IV, [IV, V])),
>     (0.10, (IV, [IV, VII])),
>     (0.10, (IV, [IV, I])),
>     (0.45, (V, [V, II])),
>     (0.45, (V, [V, IV])),
>     (0.10, (V, [V, I])),
>     (0.50, (VI, [VI, III])),
>     (0.50, (VI, [VI, I])),
>     (0.30, (VII, [VII, II])),
>     (0.30, (VII, [VII, IV])),
>     (0.30, (VII, [VII, I])) ]

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
