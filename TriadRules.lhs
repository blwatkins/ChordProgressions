-- Chord Sequence Generator
-- TriadRules Module
-- Rule type and rules for triad generation
-- Major and minor modes

> module TriadRules where
> import Probability
> import ChordTypes

=====================================================================

> type Rule a = (Prob, a)

=====================================================================

-- Triad Rules for Major Keys

> triadRules :: [Rule (Triad, [Triad])]
> triadRules = [
>     (0.90, (I, [I, V])),
>     (0.10, (I, [I, VII])),
>     (0.70, (II, [II, VI])),
>     (0.30, (II, [II, I])),
>     (0.80, (III, [III, I])),
>     (0.20, (III, [III, VII])),
>     (0.70, (IV, [IV, VI])),
>     (0.30, (IV, [IV, I])),
>     (0.50, (V, [V, II])),
>     (0.40, (V, [V, IV])),
>     (0.10, (V, [V, I])),
>     (0.50, (VI, [VI, III])),
>     (0.50, (VI, [VI, I])),
>     (0.30, (VII, [VII, II])),
>     (0.50, (VII, [VII, IV])),
>     (0.20, (VII, [VII, I])) ]