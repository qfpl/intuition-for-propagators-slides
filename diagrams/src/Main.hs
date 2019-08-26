{-# language BlockArguments #-}
{-# language OverloadedLists #-}
{-# language RecursiveDo #-}

module Main where

import Control.Lens.Indexed (FunctorWithIndex (imap), ifor_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Functor (void)
import Data.List (intercalate)
import Data.Propagator.Diagram
import Data.GraphViz.Attributes (rank)
import Data.GraphViz.Attributes.Complete (Attribute (Weight, MinLen), Number (Int), RankType (SameRank))
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

introAdder :: Reveal String
introAdder = mdo

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

introToUpper :: Reveal String
introToUpper = mdo
  lIn <- switchLabel sWrite "\'q\'"
  lOut <- switchLabel sRun "\'Q\'"

  always $ cell "in" lIn
  
  sOut <- slide
  reveal sOut $ cell "out" lOut

  sProp <- slide
  reveal sProp $ propagator "prop" "toUpper"
  reveal sProp $ edge "in" "prop"
  reveal sProp $ edge "prop" "out"

  sWrite <- slide
  sRun <- slide

  pure ()

buildAdder :: Reveal String
buildAdder = mdo
  lInL <- switchLabels [(sInL1, "4"), (sInL2, "5")]
  lInR <- switchLabel sInR "3"
  lOut <- switchLabel sOut "8"

  always $ cell "inL" lInL
  always $ cell "inR" lInR
  always $ cell "out" lOut

  sProp <- slide
  reveal sProp $ propagator "add" "+"
  reveal sProp $ edge "inL" "add"
  reveal sProp $ edge "inR" "add"
  reveal sProp $ edge "add" "out"

  sInL1 <- slide
  sInR  <- slide
  sInL2 <- slide
  sOut  <- slide

  pure ()

biAdder :: Reveal String
biAdder = mdo

  bunch 0 $ do
    lift $ graphAttrs [rank SameRank]
    always $ cell "inL" ""
    always $ cell "inR" ""

  bunch 1 $ do
    lift $ graphAttrs [rank SameRank]
    always $ propagator "sub1" "-"
    always $ propagator "add" "+"
    always $ propagator "sub2" "-"

  bunch 2 $ do
    lift $ graphAttrs [rank SameRank]
    always $ cell "out" ""

  always $ attrs [Weight (Int 50)] $ edge "inL" "add"
  always $ attrs [Weight (Int 50)] $ edge "inR" "add"
  always $ attrs [Weight (Int 50)] $ edge "add" "out"

  always $ edge "out" "sub1"
  always $ edge "inL" "sub1"
  always $ edge "sub1" "inR"

  always $ edge "out" "sub2"
  always $ edge "inR" "sub2"
  always $ edge "sub2" "inL"

  never $ attrs [Weight (Int 0), MinLen 2] $ edge "inL" "out"
  never $ attrs [Weight (Int 0), MinLen 2] $ edge "inR" "out"

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
  , ("intro-toUpper", introToUpper)
  , ("intro-adder", introAdder)
  , ("build-adder", buildAdder)
  , ("bidirectional-adder", biAdder)
  ]

main :: IO ()
main = for_ diagrams (uncurry build)
