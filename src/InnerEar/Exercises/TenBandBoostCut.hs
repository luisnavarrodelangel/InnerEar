{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.TenBandBoostCut (tenBandBoostCutExercise) where

import Reflex
import Reflex.Dom
import Data.Map
import Text.JSON
import Text.JSON.Generic

import InnerEar.Widgets.SpecEval
import InnerEar.Types.Data
import InnerEar.Types.Score
import Reflex.Synth.Synth
import Reflex.Synth.Types
import InnerEar.Types.Exercise
import InnerEar.Types.ExerciseId
import InnerEar.Types.Frequency
import InnerEar.Exercises.MultipleChoice
import InnerEar.Widgets.Config
import InnerEar.Widgets.UserMedia
import InnerEar.Widgets.Utility (radioWidget, safeDropdown)

data FrequencyBand = AllBands | HighBands  | Mid8Bands | MidBands | LowBands deriving (Eq,Data,Typeable, Read)

instance Show FrequencyBand where
  show (AllBands) = "Entire spectrum"
  show (HighBands) = "High Bands"
  show (MidBands) = "Mid Bands"
  show (Mid8Bands) = "Mid 8 Bands"
  show (LowBands) = "Low Bands"


-- necessary so things are displayed in the config dropdown in the right order
instance Ord FrequencyBand where
  compare AllBands AllBands = EQ
  compare AllBands _ = LT
  compare HighBands AllBands = GT
  compare HighBands HighBands = EQ
  compare HighBands MidBands = LT
  compare HighBands Mid8Bands = LT
  compare HighBands LowBands = LT
  compare MidBands AllBands = GT
  compare MidBands HighBands = GT
  compare MidBands MidBands = EQ
  compare MidBands Mid8Bands = LT
  compare MidBands LowBands = LT
  compare Mid8Bands AllBands =GT
  compare Mid8Bands HighBands = GT
  compare Mid8Bands MidBands = GT
  compare Mid8Bands Mid8Bands = EQ
  compare Mid8Bands LowBands = LT
  compare LowBands LowBands = EQ
  compare LowBands _ = GT


frequencyBands::[FrequencyBand]
frequencyBands = [AllBands, HighBands, Mid8Bands, MidBands, LowBands]

boostAmounts::[Double]
boostAmounts = [-10,-6,-3,-2,-1,1,2,3,6,10]

type Config = (FrequencyBand, Double) -- FrequencyBand and Boost/Cut amount

type Answer = Frequency

answers :: [Answer]
answers = [
  F 31 "31", F 63 "63", F 125 "125", F 250 "250", F 500 "500",
  F 1000 "1k", F 2000 "2k", F 4000 "4k", F 8000 "8k", F 16000 "16k"]

renderAnswer :: Config -> Source -> Maybe Answer -> Sound
renderAnswer (_,boost) s f = case f of
  (Just freq) -> GainSound (FilteredSound s $ Filter Peaking (freqAsDouble freq) 1.4 boost) (-10)
  Nothing -> GainSound (Sound s) (-10)

convertBands :: FrequencyBand -> [Answer]
convertBands AllBands = answers
convertBands HighBands = drop 5 answers
convertBands MidBands = take 5 $ drop 3 $ answers
convertBands Mid8Bands = take 8 $ drop 1 $ answers
convertBands LowBands = take 5 answers

generateQ :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQ config _ = randomMultipleChoiceQuestion (convertBands $ fst config)

tenBandsConfigWidget::MonadWidget t m => Config -> m (Dynamic t Config,  Dynamic t Source,  Event t (Maybe a)) -- dyn config, source, and event maybe answer for playing reference sound (config widget
tenBandsConfigWidget c =  elClass "div" "configWidget" $ do
  config <- elClass "div" "radioConfigWidget" $ do
    text "Spectrum Range: "
    (bands,_) <- safeDropdown (fst c) (fromList $ fmap (\x->(x,show x)) frequencyBands) (constDyn empty) never
    (boost,_) <- radioWidget (fromList $ fmap (\x->(show x ++ " dB", x)) boostAmounts) (Just $ snd c)
    combineDyn (\x y-> (x, maybe (snd c) id y ) ) bands  boost
  let sources = (fromList $ zip [0::Int,1] $ fmap (\(x,y)-> (x, ((flip NodeSource) (Just 2) . BufferNode $ File y))) [("Pink noise","pinknoise.wav"),("White noise","whitenoise.wav")])
  (source,playReference) <- sourceWidget'' ("tenBandsExercise") sources 0
  return (config, source, Nothing <$ playReference)

tenBandBoostCutExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
tenBandBoostCutExercise = multipleChoiceExercise
  3
  answers
  (return ())
  tenBandsConfigWidget
  renderAnswer
  TenBandBoostCut
  (AllBands, -10)
  (displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers)
  generateQ
