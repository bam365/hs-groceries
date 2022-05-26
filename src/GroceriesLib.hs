module GroceriesLib 
    ( BeamSqliteGroceriesRepoR (..)
    , GroceriesRepo (..)
    , GroceryT (..)
    , Grocery (..)
    , mkGroceryId
    ) where

import Control.Monad.IO.Class (MonadIO)
import Database.Beam
import Database.Beam.Sqlite
--import Database.Beam.Query ((==.))
import Database.SQLite.Simple (Connection)

import Data.Text (Text)
import Data.Int (Int32)


data GroceryT f 
    = Grocery
    { _groceryId :: Columnar f Int32
    , _groceryName :: Columnar f Text
    , _groceryGot :: Columnar f Bool
    } deriving (Generic, Beamable)

type Grocery = GroceryT Identity
deriving instance Show Grocery
deriving instance Eq Grocery

instance Table GroceryT where
    data PrimaryKey GroceryT  f =  GroceryId (Columnar f Int32)
        deriving (Generic, Beamable)
    primaryKey = GroceryId . _groceryId


type GroceryId = PrimaryKey GroceryT Identity

-- This should not be necessary...
mkGroceryId :: Int32 -> GroceryId
mkGroceryId = GroceryId

data GroceriesDb f 
    = GroceriesDb
    { _groceries :: f (TableEntity GroceryT)
    } deriving (Generic, Database be)

class GroceriesRepo r where
    getAllGroceries :: r -> IO [Grocery]
    insertGrocery :: r -> Text -> IO ()
    setGot :: r -> GroceryId -> Bool -> IO (Maybe Grocery)
    clearGotten :: r -> IO ()


data BeamSqliteGroceriesRepo 
    = BeamSqliteGroceriesRepo 
    { bsgrConn :: Connection
    , bsgrDb :: DatabaseSettings Sqlite GroceriesDb
    }

data BeamSqliteGroceriesRepoR =
    BeamSqliteGroceriesRepoR 
    { bsgrRun :: forall a. SqliteM a -> IO a
    , bsgrDb2 :: DatabaseSettings Sqlite GroceriesDb
    }

instance GroceriesRepo BeamSqliteGroceriesRepoR where
    getAllGroceries repo = bsgrRun repo $
        runSelectReturningList $ select $ all_ (_groceries (bsgrDb2 repo))

    insertGrocery repo gName = bsgrRun repo $ runInsert $
        insert (_groceries (bsgrDb2 repo)) $ insertExpressions [Grocery default_ (val_ gName) (val_ False)]

    setGot repo groceryId got = bsgrRun repo $ do
        result <- runSelectReturningOne $ lookup_ (_groceries (bsgrDb2 repo)) groceryId
        case result of
            (Just grocery) -> do 
                let grocery' = grocery { _groceryGot = got }
                result2 <- runUpdate $ save (_groceries (bsgrDb2 repo)) grocery'
                return $ Just grocery'
            Nothing        -> return Nothing

    clearGotten repo = bsgrRun repo $ 
        runDelete $ delete (_groceries (bsgrDb2 repo))  (\g -> _groceryGot g)
