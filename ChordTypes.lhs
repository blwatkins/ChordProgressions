-- Chord Sequence Generator
-- ChordTypes Module

> module ChordTypes where

> data Cadence = 
>     Authentic | Plagal | Half | Deceptive
>     deriving (Show, Eq)

> data Triad = 
>     I | II | III | IV | V | VI | VII
>     deriving (Show, Eq)

> data ChordType =
>     PhraseChord | CadenceChord | LastChord
