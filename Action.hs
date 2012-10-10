{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}
module Action
       (
         runAction
       , descInstance
       )
       where

import ClassyPrelude hiding (until)

import System.Posix.Unistd (sleep)
import Data.Default (Default (def))
import Control.Monad.Free

import AWS
import Filters
import Request
import qualified Request.RequestSpotInstances as RSI
import qualified Request.DescribeSpotInstanceRequests as DSIR
import qualified Request.DescribeInstances as DI
import qualified Request.DescribeSnapshots as DS

data ActionF a next
  = DescribeInstances DI.DescribeInstances (Filter a) (a -> next)
  | DescribeSnapshots DS.DescribeSnapshots (Filter a) (a -> next)
  | RequestSpotInstances RSI.RequestSpotInstances (Filter a) (a -> next)
  | DescribeSpotInstanceRequests DSIR.DescribeSpotInstanceRequests (Filter a) (a -> next)
  | Wait next
  | Exit
  deriving (Functor)

type Action a = Free (ActionF a)

describeInstances :: DI.DescribeInstances -> Filter a -> Action a a
describeInstances p fil = liftF $ DescribeInstances p fil id

describeSnapshots :: DS.DescribeSnapshots -> Filter a -> Action a a
describeSnapshots p fil = liftF $ DescribeSnapshots p fil id

requestSpotInstances :: RSI.RequestSpotInstances -> Filter a -> Action a a
requestSpotInstances p fil = liftF $ RequestSpotInstances p fil id

describeSpotInstanceRequests :: DSIR.DescribeSpotInstanceRequests -> Filter a -> Action a a
describeSpotInstanceRequests p fil = liftF $ DescribeSpotInstanceRequests p fil id

wait :: Action a ()
wait = liftF $ Wait ()

exit :: Action a r
exit = liftF Exit

runAction :: (Show r, Show a) => Action a r -> IO ()
runAction (Pure r) = print r
runAction (Free (DescribeInstances p fil f)) =  runAction' p fil f
runAction (Free (DescribeSnapshots p fil f)) =  runAction' p fil f
runAction (Free (RequestSpotInstances p fil f))  =  runAction' p fil f
runAction (Free (DescribeSpotInstanceRequests p fil f)) = runAction' p fil f
runAction (Free (Wait t)) = sleep 10 >> runAction t
runAction (Free Exit) = print "done"

runAction' p filter f = runActionAws p filter >>= runAction . f

runActionAws p filter = runWithFilter param filter
  where param = actionParam p

showProgram :: (Show r) => Action Text r -> [Text] -> Text
showProgram (Pure r) xt = 
  "return " ++ show r ++ "\n"
showProgram (Free (DescribeInstances p fil f)) xt =
  showProgram' "DescribeInstances\n" f xt
showProgram (Free (DescribeSnapshots p fil f)) xt = 
  showProgram' "DescribeSnapshots\n" f xt
showProgram (Free (RequestSpotInstances p fil f)) xt =
  showProgram' "RequestSpotInstances\n" f xt
showProgram (Free (DescribeSpotInstanceRequests p fil f)) xt =
  showProgram' "DescribeSpotInstanceRequests\n" f xt
showProgram (Free (Wait r)) t =
  "wait...\n " ++ showProgram r t
showProgram (Free Exit) t =
  "exit\n "

showProgram' :: (Show r) => Text -> (Text -> Action Text r) -> [Text] -> Text
showProgram' log f [] = log
showProgram' log f (x:xt) = log ++ showProgram (f x) xt

until :: Action a b -> (b -> Bool) -> Action a b
until a pred = do
  v <- a
  if pred v then return v else
    do wait
       until a pred

when :: Action a b -> (b -> Bool) -> Action a c -> Action a c
when a1 pred a2 = until a1 pred >> a2

---test
descInstance = describeInstances (def :: DI.DescribeInstances) instanceId

reqspot = do
  rqId <- requestSpotInstances (RSI.getDefault "1.01" "ami-249a2525" RSI.M1_small) spotInstanceRequestId
  when (describeSpotInstanceRequests (def { DSIR.spotInstanceRequestId = Just [rqId] }) state)
       (== "active")
       (describeSpotInstanceRequests (def { DSIR.spotInstanceRequestId = Just [rqId] }) instanceId)

-- showProgram reqspot $ replicate 10 "" ++ ["active","ami-0001"]