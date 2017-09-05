{-# LANGUAGE RecursiveDo #-}

module InnerEar.Exercises.MultipleChoice where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common
import Data.Map
import Control.Monad (zipWithM)
import Data.List (findIndices,partition,elemIndex)
import Data.Maybe (fromJust)
import System.Random

import InnerEar.Types.ExerciseId
import InnerEar.Types.Data
import InnerEar.Types.Exercise
import InnerEar.Types.ExerciseNavigation
import InnerEar.Types.Score
import InnerEar.Types.Utility
import InnerEar.Widgets.Utility
import InnerEar.Widgets.UserMedia
import InnerEar.Widgets.AnswerButton
import Reflex.Synth.Types

-- | This module introduces a function to generate multiple choice exercises.
-- Most specifically, it abstracts over the requirement to provide a widget
-- to present questions, requiring instead that a fixed list of possible
-- answers be provided, together with a pure function that converts an answer
-- value to a sound.

multipleChoiceExercise :: (MonadWidget t m, Show a, Eq a, Ord a)
  => Int -- maximum number of tries to allow
  -> [a]
  -> m (Dynamic t b) -- b represents something which affects sound production independently of configuration
  -> (c -> b -> a -> Sound)
  -> ExerciseId
  -> c
  -> (c -> m (Event t c))
  -> (Dynamic t (Map a Score) -> m ())
  -> (c -> [Datum c [a] a (Map a Score)] -> IO ([a],a))
  -> Maybe Reflection
  -> Exercise t m c [a] a (Map a Score)

multipleChoiceExercise maxTries answers bWidget render i c cw de g r = Exercise {
  exerciseId = i,
  defaultConfig = c,
  configWidget = cw,
  defaultEvaluation = empty,
  displayEvaluation = de,
  generateQuestion = g,
  questionWidget = multipleChoiceQuestionWidget maxTries answers bWidget render de,
  reflectiveQuestion = r
}



multipleChoiceQuestionWidget :: (MonadWidget t m, Show a, Eq a, Ord a)
  => Int -- maximum number of tries
  -> [a] -- fixed list of potential answers
  -> m (Dynamic t b) -- b represents something which affects sound production independently of configuration
  -> (c -> b -> a -> Sound) -- function to produce a sound from an answer
  -> (Dynamic t (Map a Score) -> m ())
  -> c
  -> Map a Score
  -> Event t ([a],a)
  -> m (Event t (Datum c [a] a (Map a Score)),Event t Sound,Event t ExerciseNavigation)

multipleChoiceQuestionWidget maxTries answers bWidget render eWidget config initialEval newQuestion = elClass "div" "exerciseWrapper" $ mdo

  let initialState = initialMultipleChoiceState answers maxTries
  let newQuestion' = fmap newQuestionMultipleChoiceState newQuestion
  questionHeard0 <- holdDyn False $ leftmost [False <$ newQuestion,True <$ playQuestion]
  let questionHeard = nubDyn questionHeard0
  let questionHeard' = fmap (const onceQuestionHeard) $ ffilter (==True) $ updated questionHeard
  let answerPressed' = fmap answerSelected answerPressed
  let stateChanges = leftmost [newQuestion',questionHeard', answerPressed']
  multipleChoiceState <- foldDyn ($) initialState stateChanges
  modes <- mapDyn answerButtonModes multipleChoiceState
  modes' <- mapM (\x-> mapDyn (!!x) modes) [0,1..9]
  scores <- mapDyn scoreMap multipleChoiceState

  -- user interface (buttons, etc)
  (playReference,playQuestion,nextQuestion) <- elClass "div" "playReferenceOrQuestion" $ do
    x <- buttonDynCss "Listen to Reference Sound" (constDyn "buttonWrapper")
    y <- buttonDynCss "Listen to Question" (constDyn "buttonWrapper")
    newQuestionVisible <- holdDyn False $ leftmost [False <$ newQuestion,True <$ y]
    z <- visibleWhen newQuestionVisible $ (InQuestion <$) <$> buttonDynCss "New Question" (constDyn "buttonWrapper")
    return (x,y,z)
  answerPressed <- elClass "div" "answerButtonWrapper" $ -- m (Event t a)
    leftmost <$> zipWithM (\f m -> answerButton (show f) m f) answers modes'
  b <- elClass "div" "bottomRow" $ do
    x <- mapDyn scoreMap multipleChoiceState
    elClass "div" "evaluationInQuestion" $ eWidget x
    elClass "div" "userMediaWidgetInQuestion" $ bWidget

  -- generate sounds to be playedW
  answer <- holdDyn Nothing $ fmap (Just . snd) newQuestion
  let questionSound = fmapMaybe id $ tagDyn answer playQuestion
  let referenceSound = Sound (NodeSource (BufferNode $ File "pinknoise.wav") 2.0) <$ playReference
  let soundsToRender = leftmost [questionSound,answerPressed]
  let renderedSounds = attachDynWith (render config) b soundsToRender
  let playSounds = leftmost [renderedSounds,referenceSound]

  -- generate navigation events
  onToReflect <- (InReflect <$) <$> buttonDynCss "Reflect" (constDyn "buttonWrapper")
  let navEvents = leftmost [nextQuestion,onToReflect]

  return (fmap Evaluation $ updated scores, playSounds,navEvents)


debugDisplay :: (MonadWidget t m, Show a ) => String -> Dynamic t a -> m ()
debugDisplay x d = el "div" $ text x >> display d

randomMultipleChoiceQuestion :: [a] -> IO ([a],a)
randomMultipleChoiceQuestion possibilities = do
  let n = length possibilities
  x <- getStdRandom ((randomR (0,n-1))::StdGen -> (Int,StdGen))
  return (possibilities,possibilities!!x)

radioConfigWidget :: (MonadWidget t m, Eq a, Show a) => String -> [a] -> a -> m (Event t a)
radioConfigWidget msg possibilities i = do
  let radioButtonMap =  zip [0::Int,1..] possibilities
  let iVal = maybe 0 id $ elemIndex i possibilities
  elClass "div" "configText" $ text msg
  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ fmap (\(x,y)->(x,show y)) radioButtonMap)
           (WidgetConfig {_widgetConfig_initialValue= Just iVal
                         ,_widgetConfig_setValue = never
                         ,_widgetConfig_attributes = constDyn empty})
  dynConfig <- holdDyn i $ fmap (\x-> maybe i id $ Data.Map.lookup (maybe 0 id x) (fromList radioButtonMap)) (_hwidget_change radioWidget)
  b <- button "Begin Exercise"
  return $ tagDyn dynConfig b

trivialBWidget :: MonadWidget t m => m (Dynamic t ())
trivialBWidget = holdDyn () $ never


data MultipleChoiceMode = ListenMode | AnswerMode | ExploreMode deriving (Eq)

data MultipleChoiceState a = MultipleChoiceState {
  mode :: MultipleChoiceMode,
  correctAnswer :: a,
  allAnswers :: [a],
  possibleAnswers :: [a],
  answerButtonModes :: [AnswerButtonMode],
  attemptsRemainingDefault :: Int,
  attemptsRemaining :: Int,
  scoreMap :: Map a Score
  }

-- initialMultipleChoiceState provides a useful initial configuration of
-- the MultipleChoiceState for the time before a new question has been
-- generated.

initialMultipleChoiceState :: [a] -> Int -> MultipleChoiceState a
initialMultipleChoiceState xs n = MultipleChoiceState {
  mode = ListenMode,
  correctAnswer = xs!!0,
  allAnswers = xs,
  possibleAnswers = xs,
  answerButtonModes = NotPossible <$ xs,
  attemptsRemainingDefault = n,
  attemptsRemaining = n,
  scoreMap = empty
  }

-- When a multiple choice question is generated, all of the buttons are
-- not possible, pending the user listening to the correct answer.

newQuestionMultipleChoiceState :: ([a],a) -> MultipleChoiceState a -> MultipleChoiceState a
newQuestionMultipleChoiceState (xs,x) s = s {
  mode = ListenMode,
  correctAnswer = x,
  possibleAnswers = xs,
  answerButtonModes = NotPossible <$ allAnswers s,
  attemptsRemaining = attemptsRemainingDefault s
  }

-- Once the user has listened to the correct answer at least once, all
-- of the buttons that represent possible answers become possible and the
-- mode changes to AnswerMode (the only mode in which answers are processed)

onceQuestionHeard :: Eq a => MultipleChoiceState a -> MultipleChoiceState a
onceQuestionHeard s = s { mode = AnswerMode, answerButtonModes = m }
  where m = fmap f $ fmap (flip elem $ possibleAnswers s) $ allAnswers s
        f True = Possible
        f False = NotPossible

-- When answers are selected they are ignored if mode is ListenMode or ExploreMode
-- Otherwise (i.e. AnswerMode) the state is updated in different ways depending
-- on whether the answer is correct or incorrect, and

answerSelected :: (Eq a,Ord a) => a -> MultipleChoiceState a -> MultipleChoiceState a
answerSelected _ s | mode s == ListenMode = s
answerSelected _ s | mode s == ExploreMode = s

answerSelected a s | a == correctAnswer s = toExploreMode $ s {
  answerButtonModes = replaceAtSameIndex a (allAnswers s) Correct (answerButtonModes s),
  scoreMap = markCorrect a $ scoreMap s
  }

answerSelected a s | a /= correctAnswer s && attemptsRemaining s > 0 = s {
  answerButtonModes = replaceAtSameIndex a (allAnswers s) IncorrectDisactivated (answerButtonModes s),
  attemptsRemaining = attemptsRemaining s - 1,
  scoreMap = markIncorrect a (correctAnswer s) $ scoreMap s
  }

answerSelected a s | a /= correctAnswer s && attemptsRemaining s == 0 = toExploreMode $ s {
  answerButtonModes = replaceAtSameIndex a (allAnswers s) IncorrectActivated (answerButtonModes s),
  scoreMap = markIncorrect a (correctAnswer s) $ scoreMap s
  }

toExploreMode :: MultipleChoiceState a -> MultipleChoiceState a
toExploreMode s = s {
  mode = ExploreMode,
  answerButtonModes = fmap f $ answerButtonModes s
  }
  where
    f NotPossible = NotPossible
    f Possible = Possible
    f IncorrectDisactivated = IncorrectActivated
    f IncorrectActivated = IncorrectActivated
    f Correct = Correct
    f CorrectMissed = CorrectMissed
