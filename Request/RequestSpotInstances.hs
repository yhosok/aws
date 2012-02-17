{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Request.RequestSpotInstances where

import Data.Text (Text,pack)
import Data.Char (toLower)
import Control.Applicative
import Data.Maybe (catMaybes)
import Data.Default (Default (def))
import Data.Time (UTCTime)

import Request
import Request.RequestTH
import Request.RequestDefParser
import Util

$(mkDefsD [request|
RequestSpotInstances
  SpotPrice String Yes
  InstanceCount Integer No
  Type_ Data Type No
  ValidFrom DateTime No
  ValidUntil DateTime No
  Subnet String No
  LaunchGroup String No
  AvailabilityZoneGroup String No
  PlacementGroupName String No
  LaunchSpecification Data LaunchSpecification Yes

LaunchSpecification
  ImageId String Yes
  KeyName String No
  SecurityGroupId List String No
  SecurityGroup List String No
  UserData String No
  AddressingType String No
  InstanceType Data InstanceType Yes
  PlacementAvailabilityZone String No
  KernelId String No
  RamdiskId String No
  BlockDeviceMapping List Data BlockDeviceMapping No
  Monitoring Data Monitoring No

BlockDeviceMapping
  DeviceName String No
  VirtualName String No
  Ebs Data Ebs No

Ebs
  SnapshotId String No
  VolumeSize Integer No
  NoDevice Boolean No
  DeleteOnTermination Boolean No

Monitoring
  Enabled String No
|])

data Type = OneTime | Persistent deriving (Eq,Show)

data InstanceType = M1_small 
                  | M1_large 
                  | M1_xlarge 
                  | C1_medium 
                  | C1_xlarge 
                  | M2_xlarge 
                  | M2_2xlarge 
                  | M2_4xlarge 
                  | T1_micro
                  deriving (Eq,Show)

instance ToParam InstanceType where
  toParam it =  [("", Just $ val it)]
    where val = pack . map rep . show
          rep c | c == '_' = '.'
                | otherwise = toLower c

instance ToParam Type where
  toParam t = [("",Just $ conv t) ]
    where conv OneTime = "one-time"
          conv Persistent = "persistent"

instance Default InstanceType where
  def = M1_small

getDefault price image instype = 
  def { spotPrice = price
      , launchSpecification = 
        def { imageId = image
            , instanceType = instype
            }
      }

test = 
  def { launchSpecification = 
          def { blockDeviceMapping = 
                   Just [ def { deviceName = Just "test", ebs = Just def } ]
              }
      }