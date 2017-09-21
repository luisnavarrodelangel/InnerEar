{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.Compression (compressionExercise) where

import Reflex
import Reflex.Dom
import Data.Map
import Text.JSON
import Text.JSON.Generic

import Reflex.Synth.Types
import InnerEar.Exercises.MultipleChoice
import InnerEar.Types.ExerciseId
import InnerEar.Types.Exercise
import InnerEar.Types.Score
import InnerEar.Widgets.Config
import InnerEar.Widgets.SpecEval
import InnerEar.Types.Data (Datum)

type Config = Double -- representing compression ratio, i.e. 2 = 2:1 compression ratio

configs :: [Config]
configs = [20,10,5,2,1.5]

configMap:: Map String Config
configMap = fromList $ fmap (\x-> (show x++":1", x)) configs

data Answer = Answer Bool deriving (Eq,Ord,Data,Typeable)

instance Show Answer where
  show (Answer True) = "Compressed"
  show (Answer False) = "Not Compressed"

answers = [Answer False,Answer True]

renderAnswer :: Config -> b -> Maybe Answer -> Sound
renderAnswer r b (Just (Answer True)) = NoSound -- should be source (b) compressed at threshold -20 dB with ratio r, then down -10 dB post-compression
renderAnswer _ b (Just (Answer False)) = NoSound -- should just be source (b) down -10 dB
renderAnswer _ b Nothing = NoSound -- should be the same as false answer above
-- note also: the user MUST provide a sound file (or we might provide some standard ones) - synthetic sources won't work for this

configurationWidget :: MonadWidget t m => Config -> m (Event t Config)
configurationWidget i = radioConfigWidget "" msg configs i
  where msg = "Please choose the compression ratio to be used for this exercise:"

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers

generateQ :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion [Answer False,Answer True]

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text ""
  elClass "div" "instructionsText" $ text ""

compressionExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
compressionExercise = multipleChoiceExercise
  1
  answers
  instructions
  (sineSourceConfig "compressionExercise" configMap)
  renderAnswer
  Compression
  (20)
  configurationWidget
  displayEval
  generateQ
  (Just "Please write a reflection here...")
