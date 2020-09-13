module Handler.ChannelR(getChannelR,broadcast) where

import           Game.Game
import           Util.StateUtil
import           Import
import           Control.Lens
import           Control.Concurrent.Chan as Chan
import           Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import           Data.Aeson (encode)
import           Data.ByteString.Lazy.Char8 as Char8

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
                        where message = Char8.unpack $ encode gse
                              serverEvent = ServerEvent Nothing Nothing
