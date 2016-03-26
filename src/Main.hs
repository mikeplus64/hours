{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Data.Thyme.Time
import           Data.Yaml
import           GHC.Generics          (Generic)
import           System.Directory      (doesFileExist)
import           System.Environment    (getArgs)
import           Orphans ()

data Shift = Shift
  { start    :: !LocalTime
  , end      :: !LocalTime
  , duration :: !NominalDiffTime
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Timesheet = Timesheet
  { filename     :: !String
  , timezone     :: !TimeZone
  , timeWorked   :: !NominalDiffTime
  , registered   :: !LocalTime
  , currentShift :: !(Maybe UTCTime)
  , shifts       :: ![Shift]
  } deriving (Show, Generic, ToJSON, FromJSON)

type Time = (TimeZone, LocalTime)

timesheet :: IO Timesheet
timesheet = do
  tz    <- getCurrentTimeZone
  time  <- getCurrentTime
  local <- pure (utcToLocalTime tz time)
  day   <- pure (localDay local)
  let n = tsName day tz ++ ".yaml"
  exists <- doesFileExist n
  if exists
    then fromMaybe (error "Could not read timesheet") <$> readTimesheet n
    else
      return Timesheet
        { filename     = n
        , timezone     = tz
        , timeWorked   = 0
        , registered   = local
        , currentShift = Nothing
        , shifts       = []
        }

tsName :: Day -> TimeZone -> String
tsName day tz =
  shows day ("-" ++ show tz)

saveTimesheet :: Timesheet -> IO ()
saveTimesheet ts = do
  encodeFile (filename ts) ts
  B.putStrLn =<< B.readFile (filename ts)

readTimesheet :: FilePath -> IO (Maybe Timesheet)
readTimesheet = decodeFile

shift :: TimeZone -> UTCTime -> UTCTime -> Shift
shift tz st fin =
  Shift (utcToLocalTime tz st) (utcToLocalTime tz fin) (diffUTCTime fin st)

stopWork :: UTCTime -> Timesheet -> Either String Timesheet
stopWork now ts
  | Just st <- currentShift ts =
    Right ts { currentShift = Nothing
             , timeWorked   = timeWorked ts + diffUTCTime now st
             , shifts       = shifts ts ++ [shift (timezone ts) st now]
             }
  | otherwise = Left "Nothing to stop."

startWork :: UTCTime -> Timesheet -> Either String Timesheet
startWork now ts
  | Nothing <- currentShift ts = Right ts {currentShift = Just now}
  | otherwise                  = Left "Already working?"

main :: IO ()
main = do
  args <- getArgs
  now  <- getCurrentTime
  ts   <- timesheet
  case args of
    ["start"] -> either error saveTimesheet (startWork now ts)
    ["stop"]  -> either error saveTimesheet (stopWork now ts)
    _         -> error "Usage: timesheet start|stop"

