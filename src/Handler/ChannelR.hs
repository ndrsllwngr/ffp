module Handler.ChannelR where

import           Game.Game
import           Game.StateUtil
import           Game.Util
import           Import
import           Marshalling
import           Control.Lens
import Control.Concurrent.Chan
import Network.Wai.EventSource (ServerEvent (..), eventSourceApp)

getChannelR :: Int -> Handler ()
getChannelR gameIdText = do
  app <- getYesod
  let tGames = games app
  let gameId_ = unpack gameIdText
  gameStateMaybe <- liftIO $ getGameById tGames gameId_
  case gameStateMaybe of
        -- If a game was found in the in-memory state return it
        Just gameState -> do
                  -- tfoo
                  channel <- liftIO $ Control.Concurrent.Chan.dupChan $ channel game
                  request  <- waiRequest
                  response  <- lift $ eventSourceApp channel request
                  -- updateGame id game
                  sendWaiResponse response
        
broadcast :: Int -> Handler ()
broadcast gameId = do
  app <- getYesod
  let tGames = games app
  game <- getGameById tGames gameId
  liftIO $ Control.Concurrent.Chan.writeChan (channel game) $ serverEvent $ return $ fromString message
  where message = "hi"
        serverEvent = ServerEvent Nothing Nothing