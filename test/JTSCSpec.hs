{-# LANGUAGE OverloadedStrings #-}
module JTSCSpec (spec) where

import qualified Data.ByteString.Lazy as BSL
import qualified Text.Parsec          as P

import Test.Hspec

import JTSC

spec :: Spec
spec = do
  describe "cutCommonNamePart" $ do
    it "cuts udm01 from oxin8-test-env-udm01" $ do
      cutCommonNamePart "oxin8-test-env-udm01" `shouldBe` "udm01"
    it "cuts test01 from oxin8-test-env-member-test01" $ do
      cutCommonNamePart "oxin8-test-env-member-test01" `shouldBe` "test01"
  describe "pMachineInformation succeeds" $ do
    it "Parses example1" $ do
      P.parse pMachineInformation "" example1 `shouldBe` Right (MachineInformation "oxin8-test-env-udm01" "10.210.79.67")
  describe "pMachines" $ do
    it "Parses example2" $ do
      let expected = [ MachineInformation "oxin8-test-env-udm01" "10.210.79.67"
                     , MachineInformation "oxin8-test-env-usp01" "10.210.214.221"
                     , MachineInformation "oxin8-test-env-ups01" "10.210.64.156"
                     , MachineInformation "oxin8-test-env-member-pco01" "10.210.149.171"
                     , MachineInformation "oxin8-test-env-member-upw01" "10.210.203.189"
                     , MachineInformation "oxin8-test-env-member-upg01" "10.210.166.29"
                     , MachineInformation "oxin8-test-env-member-test01" "10.210.239.208"
                     , MachineInformation "oxin8-test-env-member-adm01" "10.210.240.239"
                     ]
      P.parse pMachines "" example2 `shouldBe` Right expected
    it "Parses example3" $ do
      let expected = [ MachineInformation "oxin8-test-env-member-pco01" "10.210.205.43"
                     , MachineInformation "oxin8-test-env-member-upw01" "10.210.37.192"
                     , MachineInformation "oxin8-test-env-member-upg01" "10.210.250.161"
                     ]
      P.parse pMachines "" example3 `shouldBe` Right expected
    it "Parses example for older environments" $ do
      let expected = [ MachineInformation "oxin8-test-env-udm01" "10.210.91.142"
                     , MachineInformation "oxin8-test-env-usp01" "10.210.189.64"
                     , MachineInformation "oxin8-test-env-ups01" "10.210.174.205"
                     , MachineInformation "oxin8-test-env-member-pco01" "10.210.173.166"
                     , MachineInformation "oxin8-test-env-member-upw01" "10.210.57.134"
                     , MachineInformation "oxin8-test-env-member-upg01" "10.210.242.193"
                     , MachineInformation "oxin8-test-env-member-test01" "10.210.219.47"
                     , MachineInformation "oxin8-test-env-member-adm01" "10.210.3.224"
                     ]
      P.parse pMachines "" example4 `shouldBe` Right expected
    it "Parses example5" $ do
      let expected = [ MachineInformation "ox-newmail-repo01" "10.207.121.182"]
      P.parse pMachines "" example5 `shouldBe` Right expected


example1 :: BSL.ByteString
example1 =
  "[oxin8-test-env-udm01] Starting VM\n\
  \done (IP: 10.210.79.67, ID: i-0a551ba5b802d245d, AMI Name: Univention Corporate Server (UCS) 4.4 (official image) rev. 1, AMI ID: ami-04e4ed4e7bb6e8610)\n\
  \"

example2 :: BSL.ByteString
example2 =
  "[oxin8-test-env-udm01] Starting VM\n\
  \done (IP: 10.210.79.67, ID: i-0a551ba5b802d245d, AMI Name: Univention Corporate Server (UCS) 4.4 (official image) rev. 1, AMI ID: ami-04e4ed4e7bb6e8610)\n\
  \[oxin8-test-env-usp01] Starting VM\n\
  \done (IP: 10.210.214.221, ID: i-0599dfc9952665990, AMI Name: Univention Corporate Server (UCS) 4.4 (official image) rev. 1, AMI ID: ami-04e4ed4e7bb6e8610)\n\
  \[oxin8-test-env-ups01] Starting VM\n\
  \done (IP: 10.210.64.156, ID: i-0f539abe91ccce853, AMI Name: Univention Corporate Server (UCS) 4.4 (official image) rev. 1, AMI ID: ami-04e4ed4e7bb6e8610)\n\
  \[oxin8-test-env-member-pco01] Starting VM\n\
  \done (IP: 10.210.149.171, ID: i-07ed4f27223f38b42, AMI Name: Univention Corporate Server (UCS) 4.4 (official image) rev. 1, AMI ID: ami-04e4ed4e7bb6e8610)\n\
  \[oxin8-test-env-member-upw01] Starting VM\n\
  \done (IP: 10.210.203.189, ID: i-03c7db022295e2821, AMI Name: Univention Corporate Server (UCS) 4.4 (official image) rev. 1, AMI ID: ami-04e4ed4e7bb6e8610)\n\
  \[oxin8-test-env-member-upg01] Starting VM\n\
  \done (IP: 10.210.166.29, ID: i-0e09b5278c3957ad2, AMI Name: Univention Corporate Server (UCS) 4.4 (official image) rev. 1, AMI ID: ami-04e4ed4e7bb6e8610)\n\
  \[oxin8-test-env-member-test01] Starting VM\n\
  \done (IP: 10.210.239.208, ID: i-041e68fd74b60ef76, AMI Name: Univention Corporate Server (UCS) 4.4 (official image) rev. 1, AMI ID: ami-04e4ed4e7bb6e8610)\n\
  \[oxin8-test-env-member-adm01] Starting VM\n\
  \done (IP: 10.210.240.239, ID: i-00b0c5f1005b833c8, AMI Name: Univention Corporate Server (UCS) 4.4 (official image) rev. 1, AMI ID: ami-04e4ed4e7bb6e8610)\n\
  \"

-- This happens when copy pasting from the Jenkins console output.
example3 :: BSL.ByteString
example3 =
  "[oxin8-test-env-member-pco01] Starting VM\n\
  \done (IP: 10.210.205.43, ID: i-0205d68c2a684272c, AMI Name: Univention Corporate Server (UCS) 4.4 (official image) rev. 1, AMI ID: ami-04e4ed4e7bb6e8610)\n\
  \\n\
  \[oxin8-test-env-member-upw01] Starting VM\n\
  \\n\
  \done (IP: 10.210.37.192, ID: i-0dbb2115582ebb398, AMI Name: Univention Corporate Server (UCS) 4.4 (official image) rev. 1, AMI ID: ami-04e4ed4e7bb6e8610)\n\
  \\n\
  \[oxin8-test-env-member-upg01] Starting VM\n\
  \\n\
  \done (IP: 10.210.250.161, ID: i-05e97dc00aa55b898, AMI Name: Univention Corporate Server (UCS) 4.4 (official image) rev. 1, AMI ID: ami-04e4ed4e7bb6e8610)\n\
  \\n\
  \"


-- Example for old environments.
example4 :: BSL.ByteString
example4 =
  "Starting VM [oxin8-test-env-udm01]\n\
  \done (IP: 10.210.91.142, ID: i-09344038e464507df)\n\
  \Starting VM [oxin8-test-env-usp01]\n\
  \done (IP: 10.210.189.64, ID: i-003dea6beb0d4e825)\n\
  \Starting VM [oxin8-test-env-ups01]\n\
  \done (IP: 10.210.174.205, ID: i-0d93daf32e345ca13)\n\
  \Starting VM [oxin8-test-env-member-pco01]\n\
  \done (IP: 10.210.173.166, ID: i-02338acd9e9ba8bbb)\n\
  \Starting VM [oxin8-test-env-member-upw01]\n\
  \done (IP: 10.210.57.134, ID: i-0f2520fc295b913eb)\n\
  \Starting VM [oxin8-test-env-member-upg01]\n\
  \done (IP: 10.210.242.193, ID: i-019527599784172ce)\n\
  \Starting VM [oxin8-test-env-member-test01]\n\
  \done (IP: 10.210.219.47, ID: i-0bb56ed76912d3bfc)\n\
  \Starting VM [oxin8-test-env-member-adm01]\n\
  \done (IP: 10.210.3.224, ID: i-0ce6cb0a5211495da)\n\
  \"

-- Example for a VM started on tross.
example5 :: BSL.ByteString
example5 =
 "[ox-newmail-repo01] Starting VM: done (VM: build_ox-newmail-repo01-r640-small-dev-thomas-2, Template: 4.4-7+e829 generic-unsafe)\n\
 \[ox-newmail-repo01] Detecting VNC display: done (VNC: tross.knut.univention.de:37)\n\
 \[ox-newmail-repo01] Detecting IPv6 address: done (MAC=52:54:00:4e:d0:57  IPv6=fe80::5054:00ff:fe4e:d057%eth0)\n\
 \[ox-newmail-repo01] Requesting IPv4 address: done (MAC=52:54:00:4e:d0:57  IPv4=10.207.121.182)\n\
 \"
