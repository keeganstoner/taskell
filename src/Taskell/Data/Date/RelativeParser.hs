module Taskell.Data.Date.RelativeParser
    ( parseRelative
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

import Data.Time.Clock (addUTCTime)

import Taskell.Data.Date.Types (Due (DueDate, DueTime))
import Taskell.Utility.Parser  (lexeme, only)

import Data.Time.Calendar (Day, addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)


-- utility functions
addP :: (Integral a) => Parser a -> UTCTime -> Parser UTCTime
addP p now = ($ now) . addUTCTime . fromIntegral . sum <$> many1 p


-- Calculate the next weekday given a start day and a target day of the week
nextWeekday :: Day -> Int -> Day
nextWeekday today targetWday = 
    let (_, _, wday) = toWeekDate today
        daysToAdd = (targetWday - wday + 7) `mod` 7
    in if daysToAdd == 0 
       then addDays 7 today 
       else addDays (fromIntegral daysToAdd) today



-- relative time parsing
minute :: Int
minute = 60

hour :: Int
hour = minute * 60

day :: Int
day = hour * 24

week :: Int
week = day * 7

timePeriodP :: Char -> Parser Int
timePeriodP c = lexeme decimal <* char c

wP :: Parser Int
wP = (* week) <$> timePeriodP 'w'

dP :: Parser Int
dP = (* day) <$> timePeriodP 'd'

hP :: Parser Int
hP = (* hour) <$> timePeriodP 'h'

mP :: Parser Int
mP = (* minute) <$> timePeriodP 'm'

-- Add day of week parsing
keywordP :: UTCTime -> Parser (Maybe Due)
keywordP now = choice
    [ "tod" *> pure (Just . DueDate $ utctDay now)
    , "tom" *> pure (Just . DueDate $ addDays 1 $ utctDay now)
    , dayOfWeekP "mon" 1 now
    , dayOfWeekP "tue" 2 now
    , dayOfWeekP "wed" 3 now
    , dayOfWeekP "thu" 4 now
    , dayOfWeekP "fri" 5 now
    , dayOfWeekP "sat" 6 now
    , dayOfWeekP "sun" 7 now
    ]

dayOfWeekP :: Text -> Int -> UTCTime -> Parser (Maybe Due)
dayOfWeekP keyword targetWday now = string keyword *> pure (Just . DueDate $ nextWeekday (addDays 0 $ utctDay now) targetWday)







sP :: Parser Int
sP = timePeriodP 's'

timeP :: UTCTime -> Parser (Maybe Due)
timeP now = only . lexeme $ Just . DueTime <$> addP (sP <|> mP <|> hP <|> dP <|> wP) now

-- relative date parsing
dateP :: UTCTime -> Parser (Maybe Due)
dateP now = only . lexeme $ Just . DueDate . utctDay <$> addP (dP <|> wP) now

-- -- relative parser
-- relativeP :: UTCTime -> Parser (Maybe Due)
-- relativeP now = dateP now <|> timeP now

relativeP :: UTCTime -> Parser (Maybe Due)
relativeP now = keywordP now <|> dateP now <|> timeP now


parseRelative :: UTCTime -> Text -> Either Text Due
parseRelative now text =
    case parseOnly (relativeP now) text of
        Right (Just due) -> Right due
        _                -> Left "Could not parse date."
