module Handler.ChannelR (getChannelR, broadcast) where

import Control.Concurrent.Chan as Chan
import Control.Lens
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 as Char8
import Game.Game
import Import
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Util.StateUtil

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
broadcast channel_ gse = do
  liftIO $ Chan.writeChan channel_ $ serverEvent $ return $ fromString message
  where
    message = Char8.unpack $ encode gse
    serverEvent = ServerEvent Nothing Nothing
