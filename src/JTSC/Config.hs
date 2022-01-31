{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module JTSC.Config where

import qualified Data.HashMap.Strict as HM

import Control.Applicative (optional)
import Control.Monad.Catch (Exception, MonadThrow (throwM))
import Data.Aeson.Types    (prependFailure, typeMismatch, (.:?))
import Data.List           (intercalate)
import Data.Maybe          (fromMaybe)
import Data.Yaml           (FromJSON (parseJSON), Value (Object), decodeFileThrow, (.:))
import Network.HTTP.Client (Request, parseRequest)
import Options.Applicative
    (Parser, ParserInfo, execParser, fullDesc, help, helper, info, long, metavar, option, progDesc,
    short, str, switch, (<**>))
import System.Environment  (lookupEnv)

newtype JTSCException = ConfigException String
                      deriving (Show)

instance Exception JTSCException

data Settings = Settings
  { schema       :: String
  , request      :: Request
  , sshConfig    :: Maybe FilePath
  , identityFile :: Maybe FilePath
  , prefix       :: String
  , append       :: Bool
  } deriving (Show)

getSettings :: IO Settings
getSettings = do flags' <- execParser flags
                 env' <- lookupEnv jtscConfigFileVar
                 cfg <- getConfiguration flags' env'
                 combineToSettings flags' env' cfg

combineToSettings :: MonadThrow m => Flags -> Maybe FilePath -> Configuration -> m Settings
combineToSettings Flags{..} _ Configuration{..} =
    do pathSelector <- case (fPathSelector, cPathSelector) of
                         (Just s, _) -> return s
                         (_, Just s) -> return s
                         _ -> throwM (ConfigException "Could not find a path-selector in the configuration!")
       let prefix = fromMaybe defaultPrefix fPrefix
           defaultPrefix =  pathSelector <> "-" <> fromMaybe "latest" fJobNumber <> "-"
       path <- case HM.lookup pathSelector cPathMap of
                   Just base -> return (intercalate "/" [base, jobNum', "consoleText"])
                   Nothing -> throwM (ConfigException
                                         "Could not find wanted path-selector in path-map!")
       req <- parseRequest (schema' ++ "://" ++ cHostname ++ port' ++ path)
       return (Settings schema' req cSshConfig cIdentityFile prefix fAppend)
  where
    schema' = fromMaybe "https" cSchema
    jobNum' = fromMaybe "lastCompletedBuild" fJobNumber
    port' = maybe "" ((':':) . show) cPort

jtscConfigFileVar :: String
jtscConfigFileVar = "JTSC_CONFIG_FILE"

-- | Command line flags.
data Flags = Flags
  { fConfigFile   :: Maybe FilePath
  , fPathSelector :: Maybe String
  , fJobNumber    :: Maybe String
  , fPrefix       :: Maybe String
  , fAppend       :: Bool
  } deriving (Eq, Show)

flags :: ParserInfo Flags
flags = info (flagsParser <**> helper)
             (fullDesc
             <> progDesc "Generates a SSH configuration file from a Jenkins job.")

flagsParser :: Parser Flags
flagsParser =
  Flags <$> optional (option str (long "config-file"
                                    <> short 'c'
                                    <> metavar "FILE"
                                    <> help "The path to the configuration to read."))
        <*> optional (option str (long "path-selector"
                                    <> short 's'
                                    <> metavar "SEL"
                                    <> help "The path to choose from the path map defined in the configuration."))
        <*> optional (option str (long "job-num"
                                    <> short 'n'
                                    <> metavar "NUM"
                                    <> help "The job number to fetch from Jenkins. (Defautlts to 'lastCompletedBuild'.)"))
        <*> optional (option str (long "prefix"
                                    <> short 'p'
                                    <> metavar "STR"
                                    <> help ("Prefix to put in front of host name."
                                            <> " The default is a concatenation of the path-selector and the job-num.")))
        <*> switch (long "append"
                      <> short 'a'
                      <> help "Append to config instead of overwriting.")

-- | Configurtion file.
data Configuration = Configuration
  { cPathMap      :: HM.HashMap String String
  , cPathSelector :: Maybe String
  , cHostname     :: String
  , cPort         :: Maybe Int
  , cSchema       :: Maybe String
  , cSshConfig    :: Maybe FilePath
  , cIdentityFile :: Maybe FilePath
  } deriving (Eq, Show)

instance FromJSON Configuration where
    parseJSON (Object v) = Configuration <$> v .:  "path-map"
                                         <*> v .:? "path-selector"
                                         <*> v .:  "hostname"
                                         <*> v .:? "port"
                                         <*> v .:? "schema"
                                         <*> v .:? "ssh-config"
                                         <*> v .:? "identity-file"
    parseJSON invalid = prependFailure "parsing Configuration failed, "
                                       (typeMismatch "Object" invalid)

getConfiguration :: Flags -> Maybe FilePath -> IO Configuration
getConfiguration Flags{..} mConfigFile =
     case (fConfigFile, mConfigFile) of
         (Just f, _)      -> decodeFileThrow f
         (_     , Just f) -> decodeFileThrow f
         _                -> throwM (ConfigException "No configuration file found to read!")
