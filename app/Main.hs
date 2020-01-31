{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Main where

import qualified Control.Concurrent.STM               as STM
import qualified Control.Monad.Reader                 as MR
import qualified Data.Text.Lazy                       as Text
import           Eventful
import qualified Eventful.Store.Memory                as MemoryStore
import qualified Eventful.UUID                        as UUID
import qualified Events.Counter                       as Counter
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Web.Scotty.Trans                     as Scotty

-- let's try to setup the store ...

-- now let's try to share that state properly
newtype WebMonad a= WebMonad {runWebMonad :: MR.ReaderT (STM.TVar(MemoryStore.EventMap Counter.CounterEvent)) IO a}
    deriving (Applicative, Functor, Monad, MR.MonadIO, MR.MonadReader (STM.TVar (MemoryStore.EventMap Counter.CounterEvent)))

webM:: MR.MonadTrans t => WebMonad a -> t WebMonad a
webM = MR.lift

main :: IO ()
main = do
    sync <- MemoryStore.eventMapTVar
    let runActionToIO m = MR.runReaderT (runWebMonad m) sync
    Scotty.scottyT 31337 runActionToIO scottyApp

scottyApp:: Scotty.ScottyT Text.Text WebMonad ()
scottyApp = do
    Scotty.middleware logStdoutDev

    Scotty.get "/api/status" $ Scotty.text "all good so far"

    Scotty.get "/api/counter/:id/reset" $ do
        counterId <- Scotty.param "id"
        tvar <- webM $ MR.ask
        let
            writer = MemoryStore.tvarEventStoreWriter $ tvar
            reader = MemoryStore.tvarEventStoreReader tvar
            uuid = UUID.uuidFromInteger $ read $ Text.unpack counterId
        _ <- MR.liftIO $ STM.atomically $ Eventful.storeEvents writer Eventful.AnyVersion uuid [Counter.CounterReset]
        -- now get the current state of the counter and send it in the response
        events <- MR.liftIO $ STM.atomically  $ Eventful.getEvents reader $ Eventful.allEvents uuid
        let state = Counter.latestCounterState $ map streamEventEvent events
        Scotty.text $ mconcat ["reseting counter #" , counterId, "\n counter is now: " , Text.pack $ show$ Counter.unpack  state]

    Scotty.get "/api/counter/:id/addSome/:some" $ do
        counterId <- Scotty.param "id"
        some <- Scotty.param "some"
        tvar <- webM $ MR.ask
        let
            writer = MemoryStore.tvarEventStoreWriter $ tvar
            reader = MemoryStore.tvarEventStoreReader $ tvar
            uuid = UUID.uuidFromInteger $ read $ Text.unpack counterId
            inc = read $ Text.unpack  some
        _ <- MR.liftIO $ STM.atomically $ Eventful.storeEvents writer Eventful.AnyVersion uuid [Counter.CounterIncreased inc]
        events <- MR.liftIO $ STM.atomically  $ Eventful.getEvents reader $ Eventful.allEvents uuid
        let state = Counter.latestCounterState $ map streamEventEvent events
        Scotty.text $ mconcat ["added ", Text.pack $ show inc, " to the counter #" , counterId, "\n counter is now: " , Text.pack $ show $ Counter.unpack state]
