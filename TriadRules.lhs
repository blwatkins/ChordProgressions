-- Chord Sequence Generator
-- TriadRules Module
-- Rule type and rules for triad generation
-- Major and minor modes

> module TriadRules where
> import ChordTypes
> import Rule

> majorTriadRules :: [Rule (Triad, [Triad])]
> majorTriadRules = [
>     (0.90, (I,   [I, V])),
>     (0.10, (I,   [I, VII])),
>     (0.70, (II,  [II, VI])),
>     (0.30, (II,  [II, I])),
>     (0.70, (III, [III, I])),
>     (0.30, (III, [III, VII])),
>     (0.70, (IV,  [IV, VI])),
>     (0.30, (IV,  [IV, I])),
>     (0.50, (V,   [V, II])),
>     (0.40, (V,   [V, IV])),
>     (0.10, (V,   [V, I])),
>     (0.50, (VI,  [VI, III])),
>     (0.50, (VI,  [VI, I])),
>     (0.40, (VII, [VII, II])),
>     (0.50, (VII, [VII, IV])),
>     (0.10, (VII, [VII, I])) ]

> minorTriadRules :: [Rule (Triad, [Triad])]
> minorTriadRules = [
>     (0.50, (I,   [I, V])),
>     (0.50, (I,   [I, VII])),
>     (0.70, (II,  [II, VI])),
>     (0.30, (II,  [II, I])),
>     (0.50, (III, [III, I])),
>     (0.50, (III, [III, VII])),
>     (0.70, (IV,  [IV, VI])),
>     (0.30, (IV,  [IV, I])),
>     (0.10, (V,   [V, II])),
>     (0.60, (V,   [V, IV])),
>     (0.30, (V,   [V, I])),
>     (0.50, (VI,  [VI, III])),
>     (0.50, (VI,  [VI, I])),
>     (0.10, (VII, [VII, II])),
>     (0.60, (VII, [VII, IV])),
>     (0.30, (VII, [VII, I])) ]
