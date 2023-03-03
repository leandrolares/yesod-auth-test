{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Application where

import           Data.Default (def)
import           Data.Text (Text)
import           Network.HTTP.Client.Conduit (Manager, newManager)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.GoogleEmail2

-- Replace with Google client ID.
clientId :: Text
clientId =
  "528934951729-hu2lckmvljm8g8fqen9ohq7pcpnh80pm.apps.googleusercontent.com"

-- Replace with Google secret ID.
clientSecret :: Text
clientSecret = "CxR_qveDNnKMH8H-fwfbDC2B"

data App = App { httpManager :: Manager }

mkYesod "App" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
  -- Note: In order to log in with BrowserID, you must correctly
  -- set your hostname here.
  approot = ApprootStatic "http://localhost:3000"

instance YesodAuth App where
  type AuthId App = Text

  authenticate = return . Authenticated . credsIdent

  loginDest _ = HomeR

  logoutDest _ = HomeR

  authPlugins _ = [authGoogleEmail clientId clientSecret]

  -- The default maybeAuthId assumes a Persistent database. We're going for a
  -- simpler AuthId, so we'll just do a direct lookup in the session.
  maybeAuthId = lookupSession "_ID"

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
  maid <- maybeAuthId
  defaultLayout
    [whamlet|
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]

runApp :: IO ()
runApp = do
  man <- newManager
  warp 3000 $ App man