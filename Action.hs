{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Action
       (
         runAction
       , descInstance
       )
       where

import ClassyPrelude

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

until :: (MonadIO m) => Action m a -> (a -> Bool) -> Action m a
until a pred = do
  v <- a
  if pred v then return v else
    do
      liftIO $ sleep 5
      until a pred

when :: (MonadIO m) => Action m a -> (a -> Bool) -> Action m b -> Action m b
when a1 pred a2 = until a1 pred >> a2

action :: (ActionParam a) => a -> Filter b -> Action IO b
action = action'

action' :: (ActionParam a, MonadIO m) => a -> Filter b -> Action m b
action' p f = liftIO $ runWithFilter param f
  where param = actionParam p

descInstance = action (def :: DI.DescribeInstances) instanceId
descInstance' = action (def {DI.instanceId = Just ["abc"]}) instanceId

reqSpot = action (getDefault "0.01" "ami-204ff921" M1_small) spotInstanceRequestId
spotState id = action (def { DSIR.spotInstanceRequestId = Just [id] }) state
descSpot id = action (def { DSIR.spotInstanceRequestId = Just [id] }) instanceId

requestSpot = do
  rqId <- reqSpot
  when (spotState rqId) (== "active") (descSpot rqId)

descSnapshots = action (def :: DS.DescribeSnapshots) (findContents "snapshotId")

descSnapshots' = action (def {DS.owner = Just ["abc"]}) (findContents "snapshotId")