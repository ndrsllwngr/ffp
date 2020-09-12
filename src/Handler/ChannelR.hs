module Handler.ChannelR(getChannelR,broadcast) where

import           Game.Game
import           Game.StateUtil
import           Import
import           Marshalling
import           Control.Lens
import Data.Text
import Control.Concurrent.Chan as Chan
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Data.Aeson.Encode (encode)

getChannelR :: Text -> Handler ()
getChannelR gameIdText = do
  app <- getYesod
  let tGames = games app
  let gameId_ = Import.unpack gameIdText
  gameStateMaybe <- liftIO $ getGameById tGames gameId_

  case gameStateMaybe of
        -- If a game was found in the in-memory state return it
        Just gameState -> do
                  channel_ <- liftIO $ Chan.dupChan (gameState ^. channel)
                  sendWaiApplication $ eventSourceAppChan channel_
        Nothing -> return ()
                  
broadcast :: Chan ServerEvent -> GameStateEntity -> Handler ()
broadcast chan gse = do
      liftIO $ Chan.writeChan chan $ serverEvent $ return $ fromString message
                        where message = show $ encode gse
                              serverEvent = ServerEvent Nothing Nothing
