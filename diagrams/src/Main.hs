{-# language BlockArguments #-}
{-# language OverloadedLists #-}
{-# language RecursiveDo #-}

module Main where

import Control.Lens.Indexed (FunctorWithIndex (imap), ifor_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Functor (void)
import Data.List (intercalate)
import Data.Propagator.Diagram
import Data.GraphViz.Attributes (DirType (NoDir), Shape (PlainText), rank, shape, toLabel)
import Data.GraphViz.Attributes.Complete (Attribute (Dir, MinLen, RankDir, Weight), Number (Int), RankType (SameRank), RankDir (FromTop))
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

  lInL <- switchLabels [(sInL1, "4"), (sInL2, "3"), (sBlank, ""), (sInL3, "17")]
  lInR <- switchLabels [(sInR1, "5"), (sBlank, ""), (sInR2, "13")]
  lOut <- switchLabels [(sOut1, "9"), (sOut2, "8"), (sBlank, ""), (sOut3, "30")]

  bunch 0 $ do
    lift $ graphAttrs [rank SameRank]
    always $ cell "inL" lInL
    always $ cell "inR" lInR

  bunch 1 $ do
    lift $ graphAttrs [rank SameRank]
    reveal sMulti $ propagator "sub1" "-"
    always $ propagator "add" "+"
    reveal sMulti $ propagator "sub2" "-"

  bunch 2 $ do
    lift $ graphAttrs [rank SameRank]
    always $ cell "out" lOut

  always $ attrs [Weight (Int 50)] $ edge "inL" "add"
  always $ attrs [Weight (Int 50)] $ edge "inR" "add"
  always $ attrs [Weight (Int 50)] $ edge "add" "out"

  reveal sMulti $ edge "out" "sub1"
  reveal sMulti $ edge "inL" "sub1"
  reveal sMulti $ edge "sub1" "inR"

  reveal sMulti $ edge "out" "sub2"
  reveal sMulti $ edge "inR" "sub2"
  reveal sMulti $ edge "sub2" "inL"

  never $ attrs [Weight (Int 0), MinLen 2] $ edge "inL" "out"
  never $ attrs [Weight (Int 0), MinLen 2] $ edge "inR" "out"

  sInL1 <- slide
  sInR1 <- slide
  sOut1 <- slide
  sInL2 <- slide
  sOut2 <- slide
  sBlank <- slide
  sMulti <- slide
  sOut3 <- slide
  sInR2 <- slide
  sInL3 <- slide

  pure ()

oscillator :: Reveal String
oscillator = mdo

  lFst <- switchLabels [(sInput, "True"), (sOscillate1, "False"), (sOscillate4, "True")]
  lSnd <- switchLabels [(sProp1, "False"), (sOscillate2, "True"), (sOscillate5, "False")]
  lThd <- switchLabels [(sProp2, "True"), (sOscillate3, "False")]

  always $ cell "fst" lFst
  always $ propagator "not1" "not"
  always $ propagator "not3" "not"
  always $ cell "snd" lSnd
  always $ propagator "not2" "not"
  always $ cell "thd" lThd
  always $ edge "snd" "not2"
  always $ edge "not2" "thd"
  always $ edge "thd" "not3"
  always $ edge "fst" "not1"
  always $ edge "not1" "snd"
  always $ edge "not3" "fst"

  sInput <- slide
  sProp1 <- slide
  sProp2 <- slide
  sOscillate1 <- slide
  sOscillate2 <- slide
  sOscillate3 <- slide
  sOscillate4 <- slide
  sOscillate5 <- slide

  pure ()

oscillatorFixed :: Reveal String
oscillatorFixed = mdo

  lFst <- switchLabels [(sStart, "None"), (sInput, "Written True"), (sOscillate1, "TooMany")]
  lSnd <- switchLabels [(sStart, "None"), (sProp1, "Written False"), (sOscillate2, "TooMany")]
  lThd <- switchLabels [(sStart, "None"), (sProp2, "Written True"), (sOscillate3, "TooMany")]

  always $ cell "fst" lFst
  always $ propagator "not1" "not"
  always $ propagator "not3" "not"
  always $ cell "snd" lSnd
  always $ propagator "not2" "not"
  always $ cell "thd" lThd
  always $ edge "snd" "not2"
  always $ edge "not2" "thd"
  always $ edge "thd" "not3"
  always $ edge "fst" "not1"
  always $ edge "not1" "snd"
  always $ edge "not3" "fst"

  sStart <- slide
  sInput <- slide
  sProp1 <- slide
  sProp2 <- slide
  sOscillate1 <- slide
  sOscillate2 <- slide
  sOscillate3 <- slide

  pure ()
  
writeOnceBool :: Reveal String
writeOnceBool = lift $ do
  graphAttrs [RankDir FromTop]
  let textNode name val = node name [toLabel val, shape PlainText]

  textNode "top" "TooMany"
  textNode "true" "Written True  "
  textNode "false" "  Written False"
  textNode "bottom" "None"

  edge "top" "true" [Dir NoDir]
  edge "top" "false" [Dir NoDir]
  edge "true" "bottom" [Dir NoDir]
  edge "false" "bottom" [Dir NoDir]

------

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
  , ("oscillator", oscillator)
  , ("oscillator-fixed", oscillatorFixed)
  , ("writeonce-bool", writeOnceBool)
  ]

main :: IO ()
main = do
  for_ diagrams (uncurry build)
  dodgyFixups


----------------------------------------
-- Abandon all hope ye who enter here --
----------------------------------------

dodgyFixups :: IO ()
dodgyFixups = do
  system $ "sed -i -e 's/not1\\ \\[label\\=not\\,shape\\=square\\]\\;/\\{rank\\=same\\;\\ not1\\ \\[label\\=not\\,shape\\=square\\]\\;/' oscillator*.dot"
  system $ "sed -i -e 's/not3\\ \\[label\\=not\\,shape\\=square\\]\\;/not3\\ \\[label\\=not\\,shape\\=square\\]\\;\\}\\{rank\\=same\\;/' oscillator*.dot"
  system $ "sed -i -e 's/not2\\ \\->\\ thd\\;/not2\\ \\->\\ thd\\;\\}/' oscillator*.dot"

  let
    make name n =
      let
        fndot = name <> show n <> ".dot"
        fnpdf = name <> show n <> ".pdf"
        space = " "
      in
        void $ system $ intercalate space
          [ "dot"
          , fndot
          , "-Tpdf"
          , "-o"
          , fnpdf
          ]

  let bads  = [0..8] :: [Int]
  for_ bads  $ make "oscillator"
  let goods = [1..7] :: [Int]
  for_ goods $ make "oscillator-fixed" 
