-- Chord Sequence Generator
-- Probability Module

> module Probability where

> type Prob = Double

> choose :: [(Prob, a)] -> Double -> a
> choose [] r = error "Nothing to choose from"
> choose [(p, x)] r = x
> choose ((p, x) : pxs) r = 
>     if p <= 0 then choose pxs r else
>     if r <= p then x else choose pxs (r - p)