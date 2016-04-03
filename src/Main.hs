{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where
import qualified Data.ByteString.Char8 as B
import           Data.Fixed            (E2, Fixed)
import           Data.Foldable         (fold)
import           Data.List             (isSuffixOf)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text             as Text
import           Data.Text.Lazy        (Text)
import qualified Data.Text.Lazy.IO     as T
import           Data.Thyme.Time
import qualified Data.Yaml             as Yaml
import           GHC.Generics          (Generic)
import           Orphans               ()
import           System.Directory      (doesFileExist, getDirectoryContents)
import           System.Environment    (getArgs)
import qualified Text.EDE              as EDE
import qualified Text.EDE.Filters      as EDE
import Paths_hours
data Shift = Shift
  { start    :: !LocalTime
  , end      :: !LocalTime
  , duration :: !NominalDiffTime
  , info     :: !(Maybe [String])
  } deriving (Eq, Ord, Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

data Timesheet = Timesheet
  { filename     :: !String
  , timezone     :: !TimeZone
  , timeWorked   :: !NominalDiffTime
  , registered   :: !LocalTime
  , currentShift :: !(Maybe UTCTime)
  , currentInfo  :: !(Maybe [String])
  , shifts       :: ![Shift]
  } deriving (Show, Generic, Yaml.ToJSON, Yaml.FromJSON)

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
        , currentInfo  = Nothing
        , shifts       = []
        }

tsName :: Day -> TimeZone -> String
tsName day tz =
  shows day ("-" ++ show tz)

saveTimesheet :: Timesheet -> IO ()
saveTimesheet ts = do
  Yaml.encodeFile (filename ts) ts
  B.putStrLn =<< B.readFile (filename ts)

readTimesheet :: FilePath -> IO (Maybe Timesheet)
readTimesheet = Yaml.decodeFile

shift :: Maybe [String] -> TimeZone -> UTCTime -> UTCTime -> Shift
shift nfo tz st fin =
  Shift { start    = utcToLocalTime tz st
        , end      = utcToLocalTime tz fin
        , duration = diffUTCTime fin st
        , info     = nfo
        }

stopWork :: UTCTime -> Timesheet -> Either String Timesheet
stopWork now ts
  | Just st <- currentShift ts =
    Right ts { currentShift = Nothing
             , currentInfo  = Nothing
             , timeWorked   = timeWorked ts + diffUTCTime now st
             , shifts       = shifts ts ++ [shift (currentInfo ts) (timezone ts) st now]
             }
  | otherwise = Left "Nothing to stop."

startWork :: UTCTime -> Timesheet -> Either String Timesheet
startWork now ts
  | Nothing <- currentShift ts = Right ts {currentShift = Just now}
  | otherwise                  = Left "Already working?"

addInfo :: Timesheet -> [String] -> Either String Timesheet
addInfo ts infoLines
  | Just _ <- currentShift ts = Right ts {currentInfo = currentInfo ts <> Just infoLines }
  | otherwise                 = Left "Not currently working."

toHours :: Text.Text -> Fixed E2
toHours = (/ (60*60)) . fromRational . toSeconds' . (read :: String -> NominalDiffTime) . Text.unpack

truncTime :: Text.Text -> Text.Text
truncTime =
  Text.takeWhile (/= '.')
  . Text.tail
  . Text.dropWhile (\a -> a /= ' ')

reportTimesheet :: Rational -> Timesheet -> IO ()
reportTimesheet rate ts = do
  tpl_ <- EDE.parseFile =<< getDataFileName "report.org.tpl"
  tpl <- case tpl_ of
    EDE.Success tpl -> pure tpl
    EDE.Failure doc -> print doc >> return undefined

  let h = toSeconds' (timeWorked ts) / (60*60)
  let m = EDE.renderWith
        [("unlines", EDE.quote "unlines" 0 Text.unlines)
        ,("hours", EDE.quote "hours" 0 (\a -> Text.pack $! show (toHours a) ++ "h"))
        ,("time", EDE.quote "time" 0 truncTime)
        ]
        tpl $ EDE.fromPairs
        [ "timesheet" EDE..= Yaml.toJSON ts
        , "rate"      EDE..= Yaml.toJSON (fromRational rate :: Fixed E2)
        , "hours"     EDE..= Yaml.toJSON (fromRational h :: Fixed E2)
        , "owed"      EDE..= Yaml.toJSON (fromRational (h*rate) :: Fixed E2)
        ]
  case m of
    EDE.Success m' -> T.putStrLn m'
    _ -> print m

showf :: Rational -> String
showf t = show (fromRational t :: Fixed E2)

main :: IO ()
main = do
  args <- getArgs
  now  <- getCurrentTime
  ts   <- timesheet
  case args of
    ["start"] -> either error saveTimesheet (startWork now ts)
    ["stop"]  -> either error saveTimesheet (stopWork now ts)

    ["info","add"] -> do
      nfo <- getContents
      either error saveTimesheet (addInfo ts [nfo])

    ["info","get"] ->
      mapM_ putStrLn (fold (currentInfo ts))

    "report":args' -> do
      files  <- getDirectoryContents "."
      sheets <- catMaybes <$> mapM readTimesheet (filter (".yaml" `isSuffixOf`) files)

      let
        rate =
          case args' of
            [rate'] -> toRational (read rate' :: Fixed E2)
            _       -> 1

      mapM_ (reportTimesheet rate) sheets

    _ -> error "Usage: timesheet start|stop|calc [n]"

