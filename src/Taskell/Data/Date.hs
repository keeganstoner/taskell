module Taskell.Data.Date
    ( Day
    , Deadline(..)
    , Due(..)
    , timeToText
    , timeToDisplay
    , timeToDisplayFormatted
    , timeToOutput
    , timeToOutputLocal
    , textToTime
    , inputToTime
    , isoToTime
    , deadline
    ) where

import ClassyPrelude

import Control.Monad.Fail (MonadFail)

import Data.Time.LocalTime (ZonedTime (ZonedTime))
import Data.Time.Zones     (TZ, localTimeToUTCTZ, timeZoneForUTCTime, utcToLocalTimeTZ)

import Data.Time.Calendar (diffDays)
import Data.Time.Format   (FormatTime, ParseTime, iso8601DateFormat)

import Taskell.Data.Date.RelativeParser (parseRelative)
import Taskell.Data.Date.Types          (Deadline (..), Due (..))

import Data.Time.Calendar (addDays, toGregorian, fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.LocalTime (LocalTime, localDay, utcToLocalTime, utc)

import qualified Data.Text as T -- If not already imported, for using pack



-- formats
dateFormat :: String
dateFormat = "%Y-%m-%d"

timeDisplayFormat :: String
timeDisplayFormat = "%Y-%m-%d %H:%M"

timeFormat :: String
timeFormat = "%Y-%m-%d %H:%M %Z"

isoFormat :: String
isoFormat = iso8601DateFormat (Just "%H:%M:%S%Q%Z")

-- utility functions
utcToZonedTime :: TZ -> UTCTime -> ZonedTime
utcToZonedTime tz time = ZonedTime (utcToLocalTimeTZ tz time) (timeZoneForUTCTime tz time)

appendYear :: (FormatTime t, FormatTime s) => String -> t -> s -> String
appendYear txt t1 t2 =
    if format "%Y" t1 == format "%Y" t2
        then txt
        else txt <> " %Y"

-- output
format :: (FormatTime t) => String -> t -> Text
format fmt = pack . formatTime defaultTimeLocale fmt

timeToText :: TZ -> UTCTime -> Due -> Text
timeToText _ now (DueDate day) = format fmt day
  where
    fmt = appendYear "%d-%b" now day
timeToText tz now (DueTime time) = format fmt local
  where
    local = utcToLocalTimeTZ tz time
    fmt = appendYear "%H:%M %d-%b" now local

-- timeToDisplay :: TZ -> Due -> Text
-- timeToDisplay _ (DueDate day)   = format dateFormat day -- right here maybe
-- timeToDisplay tz (DueTime time) = format timeDisplayFormat (utcToLocalTimeTZ tz time)



-- Function to display dates in a user-friendly manner
timeToDisplay :: TZ -> Due -> Text
timeToDisplay tz due = case due of
    DueDate day -> format dateFormat day
    DueTime time -> format timeDisplayFormat (utcToLocalTimeTZ tz time)

-- Function to display dates in a user-friendly manner with enhanced formatting
-- including Today, Tomorrow, and weekday display
timeToDisplayFormatted :: TZ -> UTCTime -> Due -> Text
timeToDisplayFormatted tz now due = case due of
    DueDate day ->
        let daysDiff = daysBetween now day
        in if daysDiff < 0 then
            "Overdue"
        else if daysDiff == 0 then
            "Today"
        else if daysDiff == 1 then
            "Tomorrow"
        else if daysDiff < 7 then
            T.pack (dayOfWeek day)  -- Convert String to Text
        else
            format dateFormat day
    DueTime time -> format timeDisplayFormat (utcToLocalTimeTZ tz time)
  where
    dayOfWeek date = formatTime defaultTimeLocale "%A" date
    daysBetween d1 d2 = diffDays d2 (utcTimeToDay d1)
    utcTimeToDay utcTime = localDay $ utcToLocalTime utc utcTime






timeToOutput :: Due -> Text
timeToOutput (DueDate day)  = format dateFormat day
timeToOutput (DueTime time) = format timeFormat time

timeToOutputLocal :: TZ -> Due -> Text
timeToOutputLocal _ (DueDate day)   = format dateFormat day
timeToOutputLocal tz (DueTime time) = format timeFormat (utcToZonedTime tz time)

-- input
parseT :: (Monad m, MonadFail m, ParseTime t) => String -> Text -> m t
parseT fmt txt = parseTimeM False defaultTimeLocale fmt (unpack txt)

parseDate :: Text -> Maybe Due
parseDate txt = DueDate <$> parseT dateFormat txt

(<?>) :: Maybe a -> Maybe a -> Maybe a
(<?>) Nothing b = b
(<?>) a _       = a

textToTime :: Text -> Maybe Due
textToTime txt = parseDate txt <?> (DueTime <$> parseT timeFormat txt)

inputToTime :: TZ -> UTCTime -> Text -> Maybe Due
inputToTime tz now txt =
    parseDate txt <?> (DueTime . localTimeToUTCTZ tz <$> parseT timeDisplayFormat txt) <?>
    case parseRelative now txt of
        Right due -> Just due
        Left _    -> Nothing



isoToTime :: Text -> Maybe Due
isoToTime txt = DueTime <$> parseT isoFormat txt

-- deadlines
deadline :: UTCTime -> Due -> Deadline
deadline now date
    | days < 0 = Passed
    | days == 0 = Today
    | days == 1 || (isFriday && days == 3) = Tomorrow  -- Monday is the "tomorrow" of Friday
    | days < 7 = ThisWeek
    | otherwise = Plenty
  where
    days =
        case date of
            DueTime t -> diffDays (utctDay t) (utctDay now)
            DueDate d -> diffDays d (utctDay now)
    
    -- Check if today is Friday (day of week 5)
    isFriday = let (_, _, dayOfWeek) = toWeekDate (utctDay now) in dayOfWeek == 5
