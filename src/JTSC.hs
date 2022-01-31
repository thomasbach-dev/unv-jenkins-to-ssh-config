{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module JTSC where

import qualified Data.ByteString.Lazy as BSL
import qualified Text.Parsec          as P

import Data.List                   (intercalate)
import Data.Maybe                  (catMaybes)
import Network.Connection          (TLSSettings (TLSSettingsSimple))
import Network.HTTP.Client         (Request, Response, httpLbs, responseBody)
import Network.HTTP.Client.TLS     (mkManagerSettings, newTlsManagerWith)
import Text.Parsec.ByteString.Lazy (Parser)

import JTSC.Config

type MachineName = String
type IPAddress = String
type ConfigEntry = String

data MachineInformation = MachineInformation
  { miName :: MachineName
  , miIP   :: IPAddress
  } deriving (Eq, Show)


fetchFromJenkinsAndParse :: Settings
                         -> IO (Either P.ParseError [ConfigEntry])
fetchFromJenkinsAndParse settings@Settings{..} = do
  resp <- fetchFromJenkins request
  let parsed = P.parse pMachines "" (responseBody resp)
  return (fmap (map (genConfigEntry settings)) parsed)

fetchFromJenkins :: Request -> IO (Response BSL.ByteString)
fetchFromJenkins req = do
  mgr <- newTlsManagerWith managerSettings
  httpLbs req mgr
  where
    managerSettings = mkManagerSettings (TLSSettingsSimple  True False False)
                                        Nothing

genConfigEntry :: Settings -> MachineInformation -> ConfigEntry
genConfigEntry settings@Settings{..} (MachineInformation name ip) =
  maybeAppendIdentityFile $
      intercalate "\n" [ "Host " ++ shortenName settings name
                       , "  User root"
                       , "  Hostname " ++ ip
                       , "  CheckHostIP no"
                       , "  StrictHostKeyChecking off"
                       , "  UserKnownHostsFile /tmp/unv_known_hosts"
                       , "  ServerAliveInterval 15"
                       ]
  where
    maybeAppendIdentityFile =
        maybe id (\s -> (++ "\n  IdentityFile " ++ s)) identityFile

shortenName :: Settings -> MachineName -> MachineName
shortenName settings = prefixMachineName settings . cutCommonNamePart

cutCommonNamePart :: MachineName -> MachineName
cutCommonNamePart = reverse . takeWhile (/= '-') . reverse

prefixMachineName :: Settings -> MachineName -> MachineName
prefixMachineName Settings{..} = (prefix ++)

pMachines :: Parser [MachineInformation]
pMachines = catMaybes <$> P.many (     P.try (Just <$> pMachineInformation)
                                 P.<|> P.try (Just <$> pMachineInformationOldEnv)
                                 P.<|> P.try (Just <$> pMachineInformationTross)
                                 P.<|> (Nothing <$ P.anyChar)
                                 )

pMachineInformation :: Parser MachineInformation
pMachineInformation = do
  _ <- P.char '['
  name <- P.manyTill P.anyChar (P.char ']')
  _ <- P.string " Starting VM" *> P.many1 P.newline
  _ <- P.manyTill P.anyChar (P.try (P.string "done (IP: "))
  ip <- pIPAddress
  _ <- P.manyTill P.anyChar P.newline
  _ <- P.many P.newline
  return (MachineInformation name ip)

pMachineInformationTross :: Parser MachineInformation
pMachineInformationTross = do
  _ <- P.char '['
  name <- P.manyTill P.anyChar (P.char ']')
  _ <- P.string " Requesting IPv4 address: done"
  _ <- P.manyTill P.anyChar (P.try (P.string "IPv4="))
  ip <- pIPAddress
  _ <- P.manyTill P.anyChar P.newline
  _ <- P.many P.newline
  return (MachineInformation name ip)

pMachineInformationOldEnv :: Parser MachineInformation
pMachineInformationOldEnv = do
  _ <- P.string "Starting VM ["
  name <- P.manyTill P.anyChar (P.char ']')
  _ <- P.newline
  _ <- P.string "done (IP: "
  ip <- pIPAddress
  _ <- P.manyTill P.anyChar P.newline
  _ <- P.many P.newline
  return (MachineInformation name ip)

pIPAddress :: Parser IPAddress
pIPAddress = do
  field1 <- pNum
  _ <- pDot
  field2 <- pNum
  _ <- pDot
  field3 <- pNum
  _ <- pDot
  field4 <- pNum
  return $ intercalate "." [field1, field2, field3, field4]

pNum :: Parser String
pNum = P.many1 P.digit

pDot :: Parser Char
pDot = P.char '.'
