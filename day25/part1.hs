#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base ^>= 4.14.1.0
             , arithmoi ^>= 0.11.0.1
-}

import Control.Arrow ((&&&))
import Math.NumberTheory.Powers.Modular (powModInt)

type EncryptionKey = Int
type LoopSize = Int
type PubKey = Int
type SubjectNum = Int

findLoopSize :: SubjectNum -> PubKey -> LoopSize
findLoopSize subjectNum pubKey = go 1
  where
    go ctr
      | powModInt subjectNum ctr 20201227 == pubKey = ctr
      | otherwise = go (ctr+1)

transform :: SubjectNum -> LoopSize -> EncryptionKey
transform subjectNum loopSize = powModInt subjectNum loopSize 20201227

main :: IO ()
main = interact $ show . uncurry transform . (head . tail &&& findLoopSize 7 . head) . map read . lines
