{-# language BlockArguments #-}
{-# language OverloadedLists #-}
{-# language RecursiveDo #-}

module Main where

import Control.Lens.Indexed (FunctorWithIndex (imap), ifor_)
import Data.Functor (void)
import Data.List (intercalate)
import Data.Propagator.Diagram
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.Foldable (for_)
import Numeric.Natural (Natural)
import System.Process (system)

type Name = String

introCell :: Reveal String
introCell = mdo
  lMessage <- switchLabels [(sMessage1,"\"Hello\""),(sMessage2,"\"Compose\"")]
  always $ cell "in" lMessage
  sMessage1 <- slide
  sMessage2 <- slide
  pure ()

introProp :: Reveal String
introProp = mdo
  always $ propagator "add" "+"

  lLeft <- switchLabels [(sLeft1, "3"), (sLeft2, "5")]
  lRight <- switchLabel sRight "7"
  lOut <- switchLabels [(sOut1, "10"), (sOut2, "12")]

  sCells <- slide
  reveal sCells $ cell "inL" lLeft
  reveal sCells $ cell "inR" lRight
  reveal sCells $ cell "out" lOut
  reveal sCells $ edge "inL" "add"
  reveal sCells $ edge "inR" "add"
  reveal sCells $ edge "add" "out"

  sLeft1 <- slide
  sRight <- slide
  sOut1 <- slide
  sLeft2 <- slide
  sOut2 <- slide

  pure ()

simpleAdder :: Reveal String
simpleAdder = mdo

  lL <- switch sThree "" "3"

  always $ cell "inL" lL
  sThree <- slide

  sProp <- slide
  lProp <- switch sAddition "" "+"
  reveal sProp $ propagator "add" lProp

  sEdge <- slide
  reveal sEdge $ edge "inL" "add"

  sCell2 <- slide
  lR <- switch sSeven "" "7"
  reveal sCell2 $ cell "inR" lR
  reveal sCell2 $ edge "inR" "add"

  sAddition <- slide

  sOutput <- slide
  lOut <- switch sTen "" "10"
  reveal sOutput $ cell "out" lOut
  reveal sOutput $ edge "add" "out"

  sSeven <- slide
  sTen <- slide
  pure ()

render :: Reveal s -> [DotGraph s]
render diagram =
  (\i a -> digraph_ (show i) a) `imap` runReveal True diagram

build :: Name -> Reveal String -> IO ()
build name diagram = ifor_ (render diagram) \ i frame -> do
  let
    fndot = name <> show i <> ".dot"
    fnpdf = name <> show i <> ".pdf"
    space = " "
  dotFile fndot frame
  void $ system $ intercalate space
    [ "dot"
    , fndot
    , "-Tpdf"
    , "-o"
    , fnpdf
    ]

diagrams :: [(Name, Reveal String)]
diagrams =
  [ ("intro-cell", introCell)
  , ("intro-prop", introProp)
  , ("intro-adder", simpleAdder)
  ]

main :: IO ()
main = for_ diagrams (uncurry build)
