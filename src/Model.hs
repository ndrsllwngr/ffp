{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Model where

import           ClassyPrelude.Yesod
import           Database.Persist.MongoDB   hiding (master)
import           Database.Persist.Quasi
import           Language.Haskell.TH.Syntax

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
-- share [mkPersist sqlSettings, mkMigrate "migrateAll"]
--    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")
let mongoSettings = mkPersistSettings (ConT ''MongoContext)
  in share [mkPersist mongoSettings]
    $(persistFileWith upperCaseSettings "config/models.persistentmodels")
-- in share [mkPersist mongoSettings] -- mkMigrate "migrateAll"
--    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")
