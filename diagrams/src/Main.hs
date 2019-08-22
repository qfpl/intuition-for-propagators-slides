{-# language RecursiveDo #-}

module Main where

import Data.Functor (void)
import Data.List (intercalate)
import Data.Propagator.Diagram
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.Foldable (for_)
import Numeric.Natural (Natural)
import System.Process (system)

diagram :: Reveal String
diagram = mdo

  lL <- switch sThree "" "3"
  lR <- switch sSeven "" "7"

  always $ cell "inL" lL
  always $ cell "inR" lR

  sProp <- slide
  lProp <- switch sAddition "" "+"
  reveal sProp $ propagator "add" lProp

  sEdges <- slide
  reveal sEdges $ edge "inL" "add"
  reveal sEdges $ edge "inR" "add"

  sOutput <- slide
  lOut <- switch sTen "" "10"
  reveal sOutput $ cell "out" lOut
  reveal sOutput $ edge "add" "out"

  sAddition <- slide
  sThree <- slide
  sSeven <- slide
  sTen <- slide
  pure ()

outputs :: [(Natural,DotGraph String)]
outputs =
  imap (\i a -> (i, digraph_ (show i) a)) $ runReveal True diagram
  where
    imap :: (Natural -> a -> b) -> [a] -> [b]
    imap f = fmap (uncurry f) . zip [0..]

main :: IO ()
main =
  for_ outputs $ \(i,g) -> do
    let fndot = "example" <> show i <> ".dot"
        fnpdf = "example" <> show i <> ".pdf"
        space = " "
    dotFile fndot g
    void $ system $ intercalate space
      [ "dot"
      , fndot
      , "-Tpdf"
      , "-o"
      , fnpdf
      ]
    void $ system $ intercalate space
      [ "firefox"
      , fnpdf
      ]
