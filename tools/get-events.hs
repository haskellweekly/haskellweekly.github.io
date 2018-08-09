#!/usr/bin/env stack
-- stack --resolver lts-12.0 script

module Main
  ( main
  )
where

import qualified Control.Arrow as Arrow
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Http
import qualified System.Environment as Environment

main :: IO ()
main = do
  key <- Environment.getEnv "MEETUP_API_KEY"
  today <- Time.utctDay <$> Time.getCurrentTime
  url <- makeUrl
    "https://api.meetup.com/find/upcoming_events"
    [ ("end_date_range", formatDay $ Time.addDays 8 today)
    , ("key", key)
    , ("oder", "time")
    , ("radius", "global")
    , ("start_date_range", formatDay $ Time.addDays (-1) today)
    , ("text", "haskell")
    ]
  request <- Client.parseUrlThrow url
  manager <- Tls.newTlsManager
  response <- Client.httpLbs request manager
  payload <- either fail pure . Aeson.eitherDecode $ Client.responseBody
    response
  mapM_ printEvent
    . List.sortOn (eventLocalDate Arrow.&&& eventYesRsvpCount)
    $ payloadEvents payload

printEvent :: Event -> IO ()
printEvent event = putStrLn $ concat
  [ "- "
  , formatDate $ eventLocalDate event
  , ": ["
  , eventName event
  , "]("
  , eventLink event
  , ") hosted by ["
  , groupName $ eventGroup event
  , "](https://www.meetup.com/"
  , groupUrlname $ eventGroup event
  , "/) in ["
  , eventLocation event
  , "](https://www.openstreetmap.org/?mlat="
  , show $ eventLatitude event
  , "&mlon="
  , show $ eventLongitude event
  , ")"
  ]

makeUrl :: String -> [(String, String)] -> IO String
makeUrl url query =
  either (fail . show) (pure . Text.unpack)
    . Text.decodeUtf8'
    $ Text.encodeUtf8 (Text.pack url)
    <> Http.renderQuery True (Http.toQuery query)

formatDay :: Time.Day -> String
formatDay = Time.formatTime Time.defaultTimeLocale "%FT00:00:00"

formatDate :: Date -> String
formatDate (Date day) =
  Time.formatTime Time.defaultTimeLocale "%A, %B %-d" day

newtype Payload = Payload
  { payloadEvents :: [Event]
  }

instance Aeson.FromJSON Payload where
  parseJSON = Aeson.withObject "Payload"$ \object -> Payload
    <$> required object "events"

data Event = Event
  { eventGroup :: Group
  , eventLink :: String
  , eventLocalDate :: Date
  , eventName :: String
  , eventVenue :: Maybe Venue
  , eventYesRsvpCount :: Integer
  }

instance Aeson.FromJSON Event where
  parseJSON = Aeson.withObject "Event" $ \object -> Event
    <$> required object "group"
    <*> required object "link"
    <*> required object "local_date"
    <*> required object "name"
    <*> optional object "venue"
    <*> required object "yes_rsvp_count"

eventLocation :: Event -> String
eventLocation event =
  maybe
      (groupLocalizedLocation $ eventGroup event)
      (\venue -> List.intercalate ", " $ Maybe.catMaybes
        [ Just $ venueCity venue
        , venueState venue
        , Just $ venueLocalizedCountryName venue
        ]
      )
    $ eventVenue event

eventLatitude :: Event -> Double
eventLatitude event =
  maybe (groupLat $ eventGroup event) venueLat $ eventVenue event

eventLongitude :: Event -> Double
eventLongitude event =
  maybe (groupLon $ eventGroup event) venueLon $ eventVenue event

data Group = Group
  { groupLat :: Double
  , groupLon :: Double
  , groupName :: String
  , groupLocalizedLocation :: String
  , groupUrlname :: String
  }

instance Aeson.FromJSON Group where
  parseJSON = Aeson.withObject "Group" $ \object -> Group
    <$> required object "lat"
    <*> required object "lon"
    <*> required object "name"
    <*> required object "localized_location"
    <*> required object "urlname"

data Venue = Venue
  { venueCity :: String
  , venueLat :: Double
  , venueLocalizedCountryName :: String
  , venueLon :: Double
  , venueState :: Maybe String
  }

instance Aeson.FromJSON Venue where
  parseJSON = Aeson.withObject "Venue" $ \object -> Venue
    <$> required object "city"
    <*> required object "lat"
    <*> required object "localized_country_name"
    <*> required object "lon"
    <*> optional object "state"

required :: Aeson.FromJSON a => Aeson.Object -> String -> Aeson.Parser a
required object key = object Aeson..: Text.pack key

optional
  :: Aeson.FromJSON a => Aeson.Object -> String -> Aeson.Parser (Maybe a)
optional object key = object Aeson..:? Text.pack key

newtype Date
  = Date Time.Day
  deriving (Eq, Ord)

instance Aeson.FromJSON Date where
  parseJSON value = do
    string <- Aeson.parseJSON value
    Date <$> Time.parseTimeM False Time.defaultTimeLocale "%F" string
