{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Action where

import Control.Monad.Error hiding (when)
import Control.Monad.Trans.Control
import Control.Applicative (Applicative, (<$>))

import Data.Text (Text)
import System.Posix.Unistd (sleep)
import Data.Default (Default (def))

import AWS
import Filters
import Request
import Request.RequestSpotInstances
import qualified Request.DescribeSpotInstanceRequests as DSIR
import qualified Request.DescribeInstances as DI
import qualified Request.DescribeSnapshots as DS


newtype Action m a = Action { runAction :: m a }
                   deriving (Functor, Applicative, Monad, MonadIO)

--runAction = runErrorT . unAction

wait :: (MonadIO m) => Action m a -> (a -> Bool) -> Action m a
wait a pred = do
  v <- a
  if pred v then return v else
    (do
      liftIO $ sleep 5
      wait a pred)

when :: (MonadIO m) => Action m a -> (a -> Bool) -> Action m b -> Action m b
when a1 pred a2 = do
  wait a1 pred
  a2

action :: (ActionParam a) => a -> Filter b -> Action IO b
action = action'

action' :: (ActionParam a, MonadIO m) => a -> Filter b -> Action m b
action' p f = liftIO $ runWithFilter param f
  where param = actionParam p

descInstance = action (def :: DI.DescribeInstances) instanceId

reqSpot = action (getDefault "0.01" "ami-ee1da9ef" M1_small) spotInstanceRequestId
spotState id = action (def { DSIR.spotInstanceRequestId = Just [id] }) state
descSpot id = action (def { DSIR.spotInstanceRequestId = Just [id] }) instanceId

requestSpot = do
  rqId <- reqSpot
  when (spotState rqId) (== "active") (descSpot rqId)

descSnapshots = action (def :: DS.DescribeSnapshots) (findContents "snapshotId")
