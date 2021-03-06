module InnerEar.Widgets.UserList where

import Reflex
import Reflex.Dom
import Data.Maybe
import Data.Map

import InnerEar.Types.Request
import InnerEar.Types.Response
import InnerEar.Types.User
import InnerEar.Types.Handle
import InnerEar.Widgets.Utility


userListWidget :: MonadWidget t m
  => Event t [Response] -> Dynamic t (Maybe Role) -> m (Event t Request,Event t (Maybe Handle))
userListWidget responses currentRole = elClass "div" "excerciseWrapper" $ do

  -- after the widget is built it requests info on all users from the server, but only if role is Administrator
  postBuild <- getPostBuild
  isAdministrator <- mapDyn (== (Just Administrator)) currentRole
  let getUserList = GetUserList <$ gate (current isAdministrator) postBuild

  -- select any and all server responses that are UserData to display a clickable map of all users
  let userEvents = fmap (catMaybes . fmap responseToUser) responses
  userMap <- foldDyn (\xs m -> Prelude.foldl (\m' (h,u) -> insert h u m') m xs) Data.Map.empty userEvents
  userList <- mapDyn elems userMap
  userNavs <- simpleList userList $ \u -> do
    userHandle <- mapDyn handle u
    userRole <- mapDyn (show . role) u
    divClass "navButton" $ do
      dynText userHandle
      text " ("
      dynText userRole
      text ") "
      (fmap Just . tagDyn userHandle) <$> button "UserPage"
  userNavs' <- mapDyn leftmost userNavs
  let userNavs'' = switchPromptlyDyn userNavs'

  -- widget asks to be closed when back button is pressed, or anytime role is not Administrator
  backButton <- (Nothing <$) <$> button "Back"
  let notAdminPostBuild = (Nothing <$) $ ffilter (/= (Just Administrator)) $ tagDyn currentRole postBuild
  let notAdminLater = (Nothing <$) $ ffilter (/= (Just Administrator)) $ updated currentRole
  let nav = leftmost [userNavs'',backButton,notAdminPostBuild,notAdminLater]
  return (getUserList,nav)

responseToUser :: Response -> Maybe (String,User)
responseToUser (UserData x@(User h _ _)) = Just (h,x)
responseToUser _ = Nothing
