module InnerEar.Widgets.Bars where

import Reflex
import Reflex.Dom
import Data.Map
import Reflex.Dom.Contrib.Widgets.Svg
import Control.Monad
import Data.Maybe (isJust)

import InnerEar.Widgets.Utility
import InnerEar.Widgets.Labels
import InnerEar.Types.Score

-- a svg with dynamic CSS and height
dynSvgCss :: MonadWidget t m => Dynamic t String -> Dynamic t Float -> m ()
dynSvgCss cssClass height = do
  cssClass' <- mapDyn (singleton "class") cssClass
  height' <- mapDyn (singleton "height" . show) height
  attrs <- mconcatDyn [cssClass', height']
  svgDynAttr "svg" attrs $ return ()

--Dynamic "rect" element
rect :: MonadWidget t m => Dynamic t Int -> Dynamic t Int -> Dynamic t Float -> Dynamic t Float -> Dynamic t String -> Dynamic t String -> m ()
rect posX posY width height style transform= do
  posX' <- mapDyn (singleton "x" . show) posX
  posY' <- mapDyn (singleton "y" . show) posY
  width' <- mapDyn (singleton "width" . show) width
  height' <- mapDyn (singleton "height" . show) height
  style' <- mapDyn (singleton "style") style
  transform' <- mapDyn (singleton "transform") transform
  m <- mconcatDyn [posX', posY', width', height', style', transform']
  svgDynAttr "rect" m $ return()

--Dynamic "rect" element with CSS style
rectDynCSS :: MonadWidget t m => Dynamic t Int -> Dynamic t Int -> Dynamic t Double -> Dynamic t Double -> Dynamic t String -> Dynamic t String -> m ()
rectDynCSS posX posY width height transform cssClass = do
    cssClass' <- mapDyn (singleton "class") cssClass
    posX' <- mapDyn (singleton "x" . show) posX
    posY' <- mapDyn (singleton "y" . show) posY
    height' <- mapDyn (singleton "height" . show) height
    width' <- mapDyn (singleton "width" . show) width
    transform' <- mapDyn (singleton "transform") transform
    m <- mconcatDyn [posX', posY', width',height', cssClass', transform']
    svgDynAttr "rect" m $ return ()

--A dynamic bar
drawBar' :: MonadWidget t m =>  Dynamic t Float -> m ()
drawBar' x  = do
    let m = fromList [("width","200px"),("height","200px"), ("viewBox", "0 0 300 200")]
    svgAttr "svg" m $ do
       let posX = constDyn  20 -- $ negate 100
       let posY = constDyn  50 -- $ negate 200
       let w = constDyn 50
       h <- mapDyn (*5) x
       let t = constDyn "rotate(0)"
       let s = constDyn "fill:green;stroke-width:5"
       rect posX posY w h s t

--A dynamic bar with css style and in-line attributes
drawBarCSS :: MonadWidget t m =>  Dynamic t Double -> Dynamic t Double -> m ()
drawBarCSS x y = do
    svgClass "svg" "svgForBars" $ do
      let posX = constDyn $ negate 30
      let posY = constDyn $ negate 200
      h <- mapDyn (*100) x
      w <- mapDyn (*1) y
      let c = constDyn "test"
      let t = constDyn "rotate (180)"
      rectDynCSS posX posY w h t c

--A dynamic bar with css style and in-line attributes
drawBarCSS'' :: MonadWidget t m =>  Dynamic t Double -> Dynamic t Double -> m ()
drawBarCSS'' x y = do
    let class' = constDyn (singleton "class" "svgForBars")
    height' <- mapDyn (* 200) x
    height'' <- mapDyn (singleton "height" . show) height'
    attrs <- mconcatDyn [class', height'']
    svgDynAttr "svg" attrs $ do
       let posX = constDyn $ negate 30
       let posY = constDyn $ negate 200
       h <- mapDyn (*200) x
       w <- mapDyn (*1) y
       let c = constDyn "test"
       let t = constDyn "rotate (180)"
       rectDynCSS posX posY w h t c

--A dynamic bar with a label, a button and CSS style
labelBarButton :: MonadWidget t m => String ->  Dynamic t String -> Dynamic t Double -> m (Event t ())
labelBarButton label buttonString barHeight = do
   labelsForBars label
   let barWidth = constDyn 30
   drawBarCSS barHeight barWidth
   question <- dynButton buttonString -- m (Event t ())
   return (question)

--A dynamic bar with a label, maybe percentage, maybe button label and maybe height
dynLabelBarButton :: MonadWidget t m => String ->  Dynamic t (Maybe Int) ->  Dynamic t (Maybe String) -> Dynamic t (Maybe Double) -> m (Event t ())
dynLabelBarButton label p buttonString barHeight = elClass "div" "barWrapper" $ do
    labelsForBars label
    let barWidth = constDyn 30
    boolBar <- mapDyn (maybe False (const True)) barHeight
    barHeight' <- mapDyn (maybe 0.0 id) barHeight
    flippableDyn (text " ") (drawBarCSS barHeight' barWidth) boolBar
    boolPercentage <- mapDyn (maybe False (const True)) p
    p' <- mapDyn (maybe 0 id) p -- Dynamic t Int
    flippableDyn (text " ") (dynPercentage p') boolPercentage
    el "div" $ do
      buttonString' <- mapDyn (maybe " " id) buttonString
      boolButton <- mapDyn (maybe False (const True)) buttonString --Dynamic t bool
      let emptyString = constDyn " "
      flippableDynE (dynButton emptyString) (dynButton buttonString') boolButton

performanceBar :: MonadWidget t m => Dynamic t Float -> Dynamic t String -> Dynamic t Int -> m ()
performanceBar percentage label count =  do
  dynPercentageFloat (constDyn "percentageClass") percentage
  dynLabelForBar (constDyn "dynLabelForBarClass") label
  dynCount (constDyn "dynCountClass") count

  --A dynamic bar with css style and in-line attribute
dynBarCSS :: MonadWidget t m =>  Dynamic t Double -> Dynamic t Double -> m ()
dynBarCSS percent barWidth = do
      svgClass "svg" "svgBarContainer" $ do
        let posX = constDyn $ negate 30 -- Dynamic t Int
        let posY = constDyn $ negate 200  --Dynamic t Int
        barHeight' <- mapDyn (*200) percent -- m (Dynamic t Float)
        barWidth' <- mapDyn (*1) barWidth -- m (Dynamic t Float)
        let c = constDyn "bars" --Dynamic t String
        let t = constDyn "rotate (180)" --Dynamic t String
        rectDynCSS posX posY barWidth' barHeight' t c  -- m ()

dynBarCSS' :: MonadWidget t m =>  Dynamic t Double -> Dynamic t Float -> m ()
dynBarCSS' percent barWidth =  do
    let class' = constDyn (singleton "class" "svgBarContainer")
    svgHeight <- mapDyn (* 100) percent
    svgHeight' <- mapDyn (singleton "height" . show) svgHeight
    let rotate' = constDyn (singleton "transform" "rotate (180)")
    attrs <- mconcatDyn [class', svgHeight', rotate']
    svgDynAttr "svg" attrs $ return ()

-- A small fainted line to use in performance graphs
faintedLineCSS :: MonadWidget t m => String -> m ()
faintedLineCSS s = svgClass "svg" s $ return ()

--A fainted Y axis
faintedYaxis :: MonadWidget t m => String -> m ()
faintedYaxis s =  svgClass "svg" s $ return ()

-- A fainted X axis
faintedXaxis :: MonadWidget t m => String -> m ()
faintedXaxis s = svgClass "svg" s $ return ()

-- A small fainted line to use in performance graphs
faintedLineToAdjustGraph :: MonadWidget t m => String -> m ()
faintedLineToAdjustGraph c = svgClass "svg" c $ return ()

segmentedXaxis :: MonadWidget t m => String -> m ()
segmentedXaxis s = svgClass "svg" s $ return ()

-- A dynamic bar for (Maybe Score)
scoreBar :: MonadWidget t m => String -> Dynamic t (Maybe Score) -> m ()
scoreBar key score  = elClass "div" "scoreBarWrapper" $ do
  bool <-  mapDyn (maybe False (const True)) score
  score' <-  mapDyn (maybe (Score 0 0 0) id) score -- Dynamic t Int
  percent <- mapDyn asPercent score'
  let b = dynScoreLabel (constDyn "scoreLabel") percent >> dynBarCSS' percent (constDyn 100)
  flippableDyn (return ()) b bool
  let b2 = emptyScoreLabel >> faintedLineCSS "svgFaintedLine" >> dynBarCSS' percent (constDyn 100)
  flippableDyn b2 (return ()) bool
  faintedLineToAdjustGraph "faintedLineToAdjustGraph"
  xLabel "xLabel" key
  mapDyn questionsAsked score' >>= dynCountLabel (constDyn "countLabel")

scoreBar' :: MonadWidget t m => String -> Dynamic t (Maybe Score) -> m ()
scoreBar' key score  = elClass "div" "scoreBarWrapperFiveBand" $ do
    bool <-  mapDyn (maybe False (const True)) score
    score' <-  mapDyn (maybe (Score 0 0 0) id) score -- Dynamic t Int
    percent <- mapDyn asPercent score'
    let b = dynScoreLabel (constDyn "scoreLabel") percent >> dynBarCSS' percent (constDyn 100)
    flippableDyn (return ()) b bool
    let b2 = emptyScoreLabel >> faintedLineCSS "svgFaintedLine" >> dynBarCSS' percent (constDyn 100)
    flippableDyn b2 (return ()) bool
    faintedLineToAdjustGraph "faintedLineToAdjustGraph"
    xLabel "xLabel" key
    mapDyn questionsAsked score' >>= dynCountLabel (constDyn "countLabel")
