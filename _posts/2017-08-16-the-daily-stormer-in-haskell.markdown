---
layout: post
author: Michael Burge
title: "The Daily Stormer, in Haskell"
started_date: 2017-08-16 02:00:00 -0700
date: 2017-08-16 10:00:00 -0700
tags:
  - haskell
---

America is under attack from outside forces that resist our control. The 13-year rolling average of killings in New Orleans alone is up compared to 20 years ago[1], where just last week a State of Emergency[2] has been called.

The Daily Stormer aims to be the final solution to the ignorance plaguing our country. White people are being run out of their homes[3], while our politicians do nothing. It's time to take this problem into our own hands.

I talk of course about hurricanes. With the declining popularity of television and radio, young people are less likely to tune in to official government-run alert systems. At any moment, they may be overrun by surprise flash floods that their older, wiser fellow citizens avoided by paying attention to the news.

This article will cover the development of software that reads a weather-reporting API, and posts automated alerts to a website and Discord bot.

## The Data

Official government sources are generally best, but they aren't the easiest to interface with. No problem: I take all of these slings and arrows gladly, so that we can have our country back.

The [National Hurricane Center](http://www.nhc.noaa.gov/aboutrss.shtml) appears to have a few dynamic "Trouble Cyclone Feeds" that we can use. Here is a complete Haskell program to download and parse them:

{% highlight haskell %}
{-# LANGUAGE ViewPatterns #-}

import Text.Feed.Import
import Text.Feed.Types
import Text.RSS.Syntax
import Network.Download

import Control.Monad

feedUrls = [
  "http://www.nhc.noaa.gov/nhc_at1.xml",
  "http://www.nhc.noaa.gov/nhc_at2.xml",
  "http://www.nhc.noaa.gov/nhc_at3.xml",
  "http://www.nhc.noaa.gov/nhc_at4.xml",
  "http://www.nhc.noaa.gov/nhc_at5.xml",
  "http://www.nhc.noaa.gov/nhc_ep1.xml",
  "http://www.nhc.noaa.gov/nhc_ep2.xml",
  "http://www.nhc.noaa.gov/nhc_ep3.xml",
  "http://www.nhc.noaa.gov/nhc_ep4.xml",
  "http://www.nhc.noaa.gov/nhc_ep5.xml"
  ]

fetchFeed :: String -> IO Feed
fetchFeed url = either error id <$> openAsFeed url

main = do
  feeds <- mapM fetchFeed feedUrls
  forM feeds $ \feed -> case feed of
    RSSFeed (rssChannel -> (rssTitle -> title)) -> print title
    _ -> error "Unexpected feed type"
{% endhighlight %}

The `case` expression is pattern-matching the `<title>` attribute from the XML file, which is all we need.

To actually run this, I initialized a new project using `stack new daily-stormer`. [Stack](https://docs.haskellstack.org/en/stable/README/) is a tool for managing Haskell projects. Then, I wrote [this commit](https://github.com/MichaelBurge/daily-stormer/commit/83c34dd13e9fb0774639860800c5e17d3ace56a0). Finally, I type `stack build` and `stack exec read-feeds` in my terminal.

Its output is:

{% highlight haskell %}
"NHC Atlantic Wallet 1 - No current storm"
"NHC Atlantic Wallet 2 - No current storm"
"NHC Atlantic Wallet 3 - Hurricane Gert (AL082017)"
"NHC Atlantic Wallet 4 - No current storm"
"NHC Atlantic Wallet 5 - No current storm"
"NHC Eastern North Pacific Wallet 1 - No current storm"
"NHC Eastern North Pacific Wallet 2 - No current storm"
"NHC Eastern North Pacific Wallet 3 - No current storm"
"NHC Eastern North Pacific Wallet 4 - No current storm"
"NHC Eastern North Pacific Wallet 5 - No current storm"
{% endhighlight %}

Here's how the program will be structured:
* On program startup, we'll read the feeds and store the alert titles in a set. We want to easily stop and start the service, so we won't generate any alerts on startup. Missed alerts before we start the service have probably already claimed their victims, anyways.
* Periodically, we'll poll the feeds and calculate a new set. If anything changes, we'll store the new set and blast any new alerts to all of our alert listener services.

## Networking

Back in the wild '90s, we wouldn't really use frameworks or libraries. We'd just sort of open a socket to the outside world, and occasionally people would send us messages. We here at the Daily Stormer long for a simpler time, but we've also learned a thing or two about letting just anyone in. We need strong borders against the outside world for security purposes, and the people managing [`warp`](https://hackage.haskell.org/package/warp-3.2.13) have built a fine type-checked wall to keep out ne'er-do-wells.

`warp` only handles HTTP. Haskell has full-blown web-frameworks too, even ones written by the same people that work on `warp`. But we're not going to use those here.

Recall that we needed components:
* To initialize some global state
* To start a timer to periodically poll for updates
* To enter a request-response loop for people visiting our website
* To send our alerts downstream to various listeners

Here's a start to an alert service, with the 3rd and 4th parts stubbed out:
{% highlight haskell %}
{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

import Text.Feed.Import
import Text.Feed.Types
import Text.RSS.Syntax
import Network.Download
import Network.HTTP.Types(status200)
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W

import Control.Concurrent(forkIO, threadDelay)
import Control.Monad
import Data.IORef
import Data.Time.Clock
import qualified Data.Set as S

data GlobalState = GlobalState {
  currentAlertsRef :: IORef (S.Set String)
  }

main :: IO ()
main = do
  st <- initialize
  startTimer st
  W.run 2000 $ \request respond -> do
    respond $ W.responseLBS status200 [] "BEWARE OF HURRICANES"

initialize :: IO GlobalState
initialize = do
  titles <- fetchTitles
  alertsRef <- newIORef titles
  return $ GlobalState { currentAlertsRef = alertsRef }

startTimer :: GlobalState -> IO ()
startTimer (currentAlertsRef -> alertsRef) = do
  threadId <- forkIO loop
  return ()
  where
    loop = do
      threadDelay $ 10 * seconds
      titles <- fetchTitles
      atomicWriteIORef alertsRef titles
      loop
    seconds = 1000000

fetchTitles :: IO (S.Set String)
fetchTitles = do
  putStrLn "Fetched!"
  return $ S.fromList []
{% endhighlight %}

At startup and every 10 seconds after, it will pretend like it's fetching status updates from the government:
{% highlight haskell %}
$ stack exec alerts-service
Fetched!
Fetched!
Fetched!
Fetched!
{% endhighlight %}

And you can curl it for some advice:
{% highlight haskell %}
$ curl localhost:2000
BEWARE OF HURRICANES
{% endhighlight %}

## Adding the Alerts

Let's change the service to actually fetch those alerts. And then we'll calculate which alerts are new.

`fetchTitles` above is where we insert the code from the first section, but let's rename it to `fetchAlerts`:
{% highlight haskell %}
feedUrls = [
  "http://www.nhc.noaa.gov/nhc_at1.xml",
  "http://www.nhc.noaa.gov/nhc_at2.xml",
  "http://www.nhc.noaa.gov/nhc_at3.xml",
  "http://www.nhc.noaa.gov/nhc_at4.xml",
  "http://www.nhc.noaa.gov/nhc_at5.xml",
  "http://www.nhc.noaa.gov/nhc_ep1.xml",
  "http://www.nhc.noaa.gov/nhc_ep2.xml",
  "http://www.nhc.noaa.gov/nhc_ep3.xml",
  "http://www.nhc.noaa.gov/nhc_ep4.xml",
  "http://www.nhc.noaa.gov/nhc_ep5.xml"
  ]

fetchAlerts :: IO (S.Set String)
fetchAlerts = do
  feeds <- rights <$> mapM openAsFeed feedUrls
  let titles = catMaybes $ flip Prelude.map feeds $ \feed -> case feed of
        RSSFeed (rssChannel -> (rssTitle -> title)) -> Just title
        _ -> Nothing
  return $ S.fromList titles
{% endhighlight %}

The original code threw an exception when it encountered a parsing error. If the URLs started returning slightly different feeds, the resulting errors could crash our program and expose us to potential government censorship, which the Daily Stormer stands against. So we instead silently drop URLs that don't parse or match our pattern.

Our service will update its alerts, but we need to calculate differences in order to warn people of new hurricanes. We'll do that in the timer:

{% highlight haskell %}
startTimer :: GlobalState -> IO ()
startTimer st@(currentAlertsRef -> alertsRef) = do
  threadId <- forkIO loop
  return ()
  where
    loop = do
      threadDelay $ 10 * seconds
      oldAlerts <- readIORef alertsRef
      updatedAlerts <- fetchAlerts
      let newAlerts = updatedAlerts `S.difference` oldAlerts
      atomicWriteIORef alertsRef updatedAlerts
      unless (S.null newAlerts) $ do
        notifyListeners st newAlerts
      loop
    seconds = 1000000

notifyListeners :: GlobalState -> S.Set String -> IO ()
notifyListeners _ newAlerts = forM_ newAlerts $ \alert -> do
  terminalListener alert
  -- INSERT OTHER LISTENERS HERE

terminalListener :: String -> IO ()
terminalListener alert = print alert
{% endhighlight %}

It's polite to poll the government servers every hour, rather than every 10 seconds. To test listeners, I recommend temporarily replacing `fetchAlerts` with this definition:
{% highlight haskell %}
fetchAlerts :: IO (S.Set String)
fetchAlerts = do
  now <- getCurrentTime
  putStrLn "Fetched!"
  return $ S.fromList [ "example", show now ]
{% endhighlight %}

Our service should now watch the government feed and tell us when there's an incoming hurricane. We should be safe, but other people need to be alerted too.

## Discord Listener

_We were going to do a Twitter bot here, but they require a mobile phone and life is too short to have one of those_

Discord is a chat application like IRC, but you can post images and browse the chat history. Since our alerting service is targeted towards kids vulnerable to sudden flash floods, this will be our most useful way to report alerts to them.

We could hand-roll the client since Discord supports webhooks, but someone's already written the [`discord-hs`](https://github.com/jano017/Discord.hs) library to do this. Here's the Discord listener:

{% highlight haskell %}
data GlobalState = GlobalState {
  currentAlertsRef :: IORef (S.Set String),
  discordChan :: Chan String
  }

-- Get these from Discord web interface
discordToken = undefined
discordChannel = undefined

initializeDiscordBot :: IO (Chan String)
initializeDiscordBot = do
  chan <- newChan
  forkIO $ do
    runBot (Bot discordToken) $ do
      with ReadyEvent $ \(Init v u _ _ _) -> do
        liftIO $ putStrLn $ "Connected to gateway " ++ show v ++ " as user " ++ show u
        loop chan
  return chan
  where
    loop chan = do
      alert <- liftIO $ readChan chan
      fetch' $ CreateMessage discordChannel (pack alert) Nothing
      loop chan

notifyListeners :: GlobalState -> S.Set String -> IO ()
notifyListeners (discordChan -> dchan) newAlerts = forM_ newAlerts $ \alert -> do
  terminalListener alert
  discordListener dchan alert

discordListener :: Chan String -> String -> IO ()
discordListener chan alert = writeChan chan alert
{% endhighlight %}

The bot needs to persistently wait in someone's Discord channel, so we fork off another thread for it(that's at least 3 threads in total now). We need to pass alerts to the bot when we find them, so we create a `Chan String` and add it to our global state.

`discord-hs` uses [`Pipes`](https://hackage.haskell.org/package/conduit) in its example code; it's another Haskell library. The problem that `pipes` solves is that we have all of these APIs and databases and web services and scrapers and files floating around. We want to hook them all together, but there's sockets and connection pools and caching layers and other complexities that need to be initialized and destroyed that get in the way. `pipes` or its competitors `conduit` and `machines` are all good choices to use for this. He's probably using `pipes` to store an HTTP session cookie.

Because `discord-hs` accesses the network, it must use `IO` underneath the hood. `liftIO` is a common way to execute an `IO` action when in a context that is similar-to-but-not-actually `IO`.

## Website

Our website should probably say more than just `"BEWARE OF HURRICANES"`. One improvement would be to display all of the alert titles:
{% highlight haskell %}
import Data.ByteString.Lazy

main :: IO ()
main = do
  st <- initialize
  startTimer st
  W.run 2000 $ \request respond -> do
    text <- renderState st
    respond $ W.responseLBS status200 [] text

renderState :: GlobalState -> IO ByteString
renderState (currentAlertsRef -> alertsRef) = do
  alerts <- readIORef alertsRef
  return $ encodeUtf8 $ TL.pack $ show alerts

{% endhighlight %}

which displays:
{% highlight haskell %}
fromList ["NHC Atlantic Wallet 1 - No current storm",
"NHC Atlantic Wallet 2 - No current storm",
"NHC Atlantic Wallet 3 - Hurricane Gert (AL082017)",
"NHC Atlantic Wallet 4 - No current storm",
"NHC Atlantic Wallet 5 - No current storm",
"NHC Eastern North Pacific Wallet 1 - No current storm",
"NHC Eastern North Pacific Wallet 2 - No current storm",
"NHC Eastern North Pacific Wallet 3 - No current storm",
"NHC Eastern North Pacific Wallet 4 - No current storm",
"NHC Eastern North Pacific Wallet 5 - No current storm"]
{% endhighlight %}

To really call it a website, though, we'll want to return HTML in our response. [`blaze-html`](https://hackage.haskell.org/package/blaze-html) is a good way to embed HTML in just about any Haskell program. Here's how to get a single unstyled table:
{% highlight haskell %}
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 (renderMarkup)

renderState :: GlobalState -> IO BSL.ByteString
renderState (currentAlertsRef -> alertsRef) = do
  alerts <- readIORef alertsRef
  return $ renderMarkup $ html $ do
    head $ do
      title "The Daily Stormer"
      -- link ! rel "stylesheet " type_ "text/css" ! href "http://assets.daily-stormer.michaelburge.us"
    body $ do
      table ! class_ "alerts" $ forM_ alerts $ \alert -> do
        tr $
          td $ toMarkup alert
  where
    title = H.title
{% endhighlight %}

You can navigate to port 2000 with your web browser to see the HTML document you'd expect.

If you wanted to build out a full website, you might try using an actual web framework at this point. I recommend using a separate asset server to host CSS, images, and other static assets because you'd otherwise have to change your code to serve them - and why do extra work?

## Conclusion

We've taken a hard look at the serious hurricane problem in the US, and presented a foundation for how one might build an alerting service to warn people of impending storms. Unfortunately, funding ran dry partway through development, so we've left the code on [Github](https://github.com/MichaelBurge/daily-stormer).

## References

* [1] https://en.wikipedia.org/wiki/Effects_of_Hurricane_Katrina_in_New_Orleans
* [2] https://www.washingtonpost.com/national/the-latest-state-of-emergency-declared-in-new-orleans/2017/08/10/5258944e-7df7-11e7-b2b1-aeba62854dfa_story.html
* [3] https://en.wikipedia.org/wiki/Demographics_of_Florida