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
    (Parser, ParserInfo, command, execParser, fullDesc, help, helper, info, long, metavar, option,
    progDesc, short, str, subparser, switch, (<**>))
import System.Environment  (lookupEnv)

newtype JTSCException = ConfigException String
                      deriving (Show)

instance Exception JTSCException

data Settings = SCommandRun RunSettings
  deriving (Show)

data RunSettings = RunSettings
  { rsSchema       :: String
  , rsRequest      :: Request
  , rsSshConfig    :: Maybe FilePath
  , rsIdentityFile :: Maybe FilePath
  , rsPrefix       :: String
  , rsAppend       :: Bool
  } deriving (Show)

getSettings :: IO Settings
getSettings = do
  cliOpts@CliOptions{..} <- execParser cliOptions
  env' <- lookupEnv jtscConfigFileVar
  cfg <- getConfiguration cliOpts env'
  case coCommand of
    CliCommandRun flags -> SCommandRun <$> combineToRunSettings flags env' cfg

combineToRunSettings :: MonadThrow m => RunFlags -> Maybe FilePath -> Configuration -> m RunSettings
combineToRunSettings RunFlags{..} _ Configuration{..} = do
  pathSelector <- case (rfPathSelector, cPathSelector) of
                    (Just s, _) -> return s
                    (_, Just s) -> return s
                    _ -> throwM (ConfigException "Could not find a path-selector in the configuration!")
  let prefix = fromMaybe defaultPrefix rfPrefix
      defaultPrefix = pathSelector <> "-" <> fromMaybe "latest" rfJobNumber <> "-"
  path <- case HM.lookup pathSelector cPathMap of
              Just base -> return (intercalate "/" [base, jobNum', "consoleText"])
              Nothing -> throwM (ConfigException
                                    "Could not find wanted path-selector in path-map!")
  req <- parseRequest (schema' ++ "://" ++ cHostname ++ port' ++ path)
  pure $ RunSettings schema' req cSshConfig cIdentityFile prefix rfAppend
  where
    schema' = fromMaybe "https" cSchema
    jobNum' = fromMaybe "lastCompletedBuild" rfJobNumber
    port' = maybe "" ((':':) . show) cPort

jtscConfigFileVar :: String
jtscConfigFileVar = "JTSC_CONFIG_FILE"

-- * Command line options.
data CliOptions = CliOptions
  { coConfigFile :: Maybe FilePath
  , coCommand    :: CliCommand
  } deriving (Eq, Show)

cliOptions :: ParserInfo CliOptions
cliOptions = info (cliOptionsParser <**> helper)
                  (fullDesc
                  <> progDesc "Maintain SSH configurations generated from Jenkins")

cliOptionsParser :: Parser CliOptions
cliOptionsParser = CliOptions
  <$> (optional . option str $
         long "config-file"
         <> short 'c'
         <> metavar "FILE"
         <> help "The path to the configuration to read.")
  <*> cliCommandParser

data CliCommand = CliCommandRun RunFlags
                deriving (Eq, Show)

cliCommandParser :: Parser CliCommand
cliCommandParser = subparser $
  command "run" (CliCommandRun <$> runFlags)

data RunFlags = RunFlags
  { rfPathSelector :: Maybe String
  , rfJobNumber    :: Maybe String
  , rfPrefix       :: Maybe String
  , rfAppend       :: Bool
  } deriving (Eq, Show)

runFlags :: ParserInfo RunFlags
runFlags = info (runFlagsParser <**> helper)
                (fullDesc
                <> progDesc "Generates a SSH configuration file from a Jenkins job.")

runFlagsParser :: Parser RunFlags
runFlagsParser = RunFlags
  <$> (optional . option str $
         long "path-selector"
         <> short 's'
         <> metavar "SEL"
         <> help "The path to choose from the path map defined in the configuration.")
  <*> (optional . option str $
         long "job-num"
         <> short 'n'
         <> metavar "NUM"
         <> help "The job number to fetch from Jenkins. (Defautlts to 'lastCompletedBuild'.)")
  <*> (optional . option str $
         long "prefix"
         <> short 'p'
         <> metavar "STR"
         <> help ("Prefix to put in front of host name."
                 <> " The default is a concatenation of the path-selector and the job-num."))
  <*> switch
        (long "append"
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

getConfiguration :: CliOptions -> Maybe FilePath -> IO Configuration
getConfiguration CliOptions{..} mConfigFile =
     case (coConfigFile, mConfigFile) of
         (Just f, _)      -> decodeFileThrow f
         (_     , Just f) -> decodeFileThrow f
         _                -> throwM (ConfigException "No configuration file found to read!")
