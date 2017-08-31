---
layout: post
author: Michael Burge
title: "Roll your Own Bitcoin Exchange in Haskell"
started_date: 2017-08-25 18:51:00
date: 2017-08-31 18:51:00
tags:
  - haskell
---

A stock exchange is a complex beast, but much of it can be reduced to a single data structure: the Order Book. A Bitcoin exchange uses the same data structure when trading currency pairs of USD, BTC, or ETH. This article will show you how to:

* Design an order book that can handle limit orders and market orders
* Install automated sanity checks that run on every write to the order book, preventing hacks and implementation bugs
* Build an HTTP API that people can use to interact with the order book

We won't work with actual bitcoins or wallets, since they add a lot of complexity and risk without making the article any more interesting. Instead, we'll assume the "billing code" has already been written and focus only on the order book portion of the exchange.

## Types

So what is an order book, really?

First we'll define our orders:

{% highlight haskell %}
import Data.Tagged
import qualified Data.Map as M
import qualified Data.Sequence as Q

type UserId = Integer

type Currency = Text
type CurrencyPair = (Currency, Currency)
type Amount = Integer
type Price = Double

data LimitOrder = LimitOrder {
  _lorder_user         :: UserId,
  _lorder_fromAmount   :: Amount,
  _lorder_toAmount     :: Amount
  } deriving (Eq, Show, Generic)

data TBid
data TAsk

type BidT a = Tagged TBid a
type AskT a = Tagged TAsk a
type Bid = BidT LimitOrder
type Ask = AskT LimitOrder

data MarketOrder = MarketOrder {
  _morder_user   :: UserId,
  _morder_amount :: Amount
  } deriving (Eq, Show, Generic)

type MBid = BidT MarketOrder
type MAsk = AskT MarketOrder

data OrderBookF a = OrderBook {
  _book_fromCurrency :: Currency,
  _book_toCurrency   :: Currency,
  _book_bids         :: M.Map Price (Q.Seq (BidT a)),
  _book_asks         :: M.Map Price (Q.Seq (AskT a))
  } deriving (Eq, Show, Functor, Traversable, Foldable)
type OrderBook = OrderBookF LimitOrder
{% endhighlight %}

A couple points to note:
* We use `Seq` rather than `List` or `Vector` because we need relatively efficient insert and search.
* The higher order `OrderBookF` lets us get `Traversable`, `Functor`, and `Foldable` instances to manipulate the order book without writing any code.
* Both `Bid` and `Ask`s are `LimitOrder`s, but I want to track them separately in the type system. So I use `Tagged` to attach a `TBid` or `TAsk` tag.

The `OrderBook` by itself isn't enough, because we want to track the final amounts actually transferred between buyers and sellers. Lets add a few types for that:
{% highlight haskell %}
data SingleEntry = SingleEntry {
  _se_account  :: UserId,
  _se_currency :: Currency,
  _se_amount   :: Amount
  } deriving (Eq, Show, Generic)

data DoubleEntry = DoubleEntry {
  _de_fromAccount :: UserId,
  _de_toAccount   :: UserId,
  _de_currency    :: Currency,
  _de_amount      :: Amount
  } deriving (Eq, Show, Generic)

data TradeF a = Trade {
  _trade_from :: a,
  _trade_to   :: a
  } deriving (Eq, Show, Functor, Traversable, Foldable, Generic)
type Trade = TradeF DoubleEntry

data External
type ExternalTransfer = Tagged External SingleEntry
{% endhighlight %}

To "execute" an order causes it to turn into at least one `Trade`. There are 4 changes to users' balances whenever a `Trade` occurs for the USD/BTC pair:

* The buyer loses his USD
* The seller gains USD
* The buyer gains BTC
* The seller loses BTC

It's important to use "Double entry accounting" when handling money of any sort. By adding an entry to both sides whenever money changes hands, it's much more resistant to bookkeeping errors. The total amount of USD and BTC should be zero-sum when transferred internally.

Some accounts are external to our exchange, such as USD held in bank accounts, or BTC held in wallets. In this example, I've chosen to eliminate the external account from the exchange's view, giving us only single-entry bookkeeping for transfers outside the exchange. For accounting purposes, there should be a separate database where full double-entry financial data is held, and it should be routinely reconciled with the exchange, any bank accounts, and any wallets.

Administrators or billing code can create `SingleEntry` records when creating or destroying money supply in the exchange, while all users will create `DoubleEntry` records amongst each other.

Finally, here's the global state that the exchange is going to keep:
{% highlight haskell %}
data Exchange = Exchange {
  _exchange_external :: TVar (Q.Seq ExternalTransfer),
  _exchange_book     :: TVar OrderBook,
  _exchange_trades   :: TVar (Q.Seq Trade)
  }
{% endhighlight %}

In words: An `Exchange` is a collection of external transfers that move money in and out of the system, an order book for a single currency pair full of orders waiting to be filled, and the history of previous trades.

This exchange has no trading fees. Those would go on either the `Trade` or `External` types, depending on whether we charge people for internal or external transfers.

## Order book

What are the fundamental operations we need to use our order book? We want to:

* Add an order to the book, automatically executing trades if able.
* Cancel an order from the book
* List all outstanding orders on the order book

Once we have these, we'll wrap it in an HTTP API that can be used by scripts or a website to interact with the order book.

I'll only look at the `Bid` limit and market orders for now - the `Ask` versions are very similar. Here are the types for the functions we need:
{% highlight haskell %}
cancelBid :: Bid -> OrderBook -> OrderBook
fillBid :: Bid -> OrderBook -> ([Trade], OrderBook)
tryFillMBid :: MBid -> Balances -> OrderBook -> (Maybe MBid, [Trade], OrderBook)
listOrders :: OrderBook -> [LimitOrder]
listOrders = toList
{% endhighlight %}

Since the generated `Foldable` instance does `listOrders` for us, I went ahead and implemented it. I'll leave the implementation of `cancelBid` on [Github](https://github.com/MichaelBurge/lambda-exchange).

For `fillBid` and `tryFillMBid`, we'll write one suitably-generic function for limit orders, and say that a `MarketOrder` is a `LimitOrder` that can't be persisted in the order book, and where the user bids his entire account balance:

{% highlight haskell %}
fillBid :: Bid -> OrderBook -> ([Trade], OrderBook)
fillBid bid book = case matchBid bid book of
  (Nothing, trades, book) ->
    (trades, book)
  (Just bidRemainder, trades, book) ->
    (trades, unsafe_addBid bidRemainder book)

tryFillMBid :: MBid -> Balances -> OrderBook -> (Maybe MBid, [Trade], OrderBook)
tryFillMBid mbid bals book@OrderBook{..} =
  case M.lookup _book_toCurrency bals of
    Nothing -> (Nothing, [], book)
    Just toBalance ->
      -- A Market order is a non-persistable limit order with the user bidding his entire balance.
      let (Tagged MarketOrder{..}) = mbid
          bid = Tagged $ LimitOrder {
            _lorder_user = _morder_user,
            _lorder_fromAmount = _morder_amount,
            _lorder_toAmount = toBalance
            }
      in case matchBid bid book of
        (Nothing, trades, book) ->
          (Nothing, trades, book)
        (Just bid, trades, book) ->
          (Just $ bidToMbid bid, trades, book)

bidToMBid :: Bid -> MBid
unsafe_addBid :: Ask -> OrderBook -> OrderBook
{% endhighlight %}

Our generic `matchBid` function attempts to fill a limit order on the `OrderBook` and returns a new `Bid` for whatever couldn't be matched, any `Trade`s that were executed, and the new `OrderBook`:

{% highlight haskell %}
matchBid :: Bid -> OrderBook -> (Maybe Bid, [Trade], OrderBook)
matchBid bid book =
  let pair = _book_pair book
      loop :: (Bid, [Trade], OrderBook) -> (Maybe Bid, [Trade], OrderBook)
      loop x@(bid, trades, book) =
        case lowestAsk book of
          -- Case 1: The order book has no asks
          (Nothing, _) -> (Just bid, [], book)
          (Just lowestAsk, deletedBook) ->
            case mergeBid pair bid lowestAsk of
              -- Case 2: The bid was unable to be matched
              (Just bid, Just _, Nothing) -> (Just bid, trades, book)
              -- Case 3: The bid was partially matched; repeat the loop
              (Just bidRemainder, Nothing, Just trade) ->
                loop (bidRemainder, trade:trades, deletedBook)
              -- Case 4: The ask was partially matched; terminate the loop.
              (Nothing, Just askRemainder, Just trade) ->
                (Nothing, trade:trades, unsafe_addAsk askRemainder deletedBook)
              -- Case 5: The bid and ask exactly canceled each other out
              (Nothing, Nothing, Just trade) ->
                (Nothing, trade:trades, deletedBook)
              -- Case 6: Impossible cases
              x -> panic $ "fillBid: Unexpected case: " <> show x
  in loop (bid, [], book)

mergeBid :: CurrencyPair -> Bid -> Ask -> (Maybe Bid, Maybe Ask, Maybe Trade)

lowestAsk :: OrderBook -> (Maybe Ask, OrderBook)

unsafe_addAsk :: Ask -> OrderBook -> OrderBook
{% endhighlight %}

Here, we repeatedly find the best price `Ask` on the order book, use it to fill our `Bid`, and stop when we run out of qualifying `Ask`s.



`lowestAsk` is pretty easy since our `Map` is sorted by `price`. I'll define it and `unsafe_addAsk` in the [Github](https://github.com/MichaelBurge/lambda-exchange) repository.

`mergeBid` is probably the most complex, since it handles 3 distinct things:
* It generates new orders if either the bid or ask were partially filled
* It generates a trade if the bid crosses the ask
* It handles the calculations determining these

{% highlight haskell %}
mergeBid :: CurrencyPair -> Bid -> Ask -> (Maybe Bid, Maybe Ask, Maybe Trade)
mergeBid (fromCurrency, toCurrency) bid ask =
  let bidOrder = unTagged bid
      askOrder = unTagged ask
      n1 = _lorder_fromAmount bidOrder
      d1 = _lorder_toAmount bidOrder
      n2 = negate $ _lorder_fromAmount askOrder
      d2 = _lorder_toAmount askOrder
      buyer = _lorder_user bidOrder
      seller = _lorder_user askOrder
      fi = fromIntegral
      -- If seller rounds down, price would be below his limit.
      sellerPrice = ceiling (fi n2 / fi d2)
      -- If buyer rounds up, price would be above his limit.
      buyerPrice = floor (fi n1 / fi d1)

      unitPrice = buyerPrice
      numUnits = min n1 n2
      toAmount = ceiling (fi numUnits / fi unitPrice)
      fromTransfer = DoubleEntry {
        _de_fromAccount = seller,
        _de_toAccount   = buyer,
        _de_amount      = numUnits,
        _de_currency    = fromCurrency
        }
      toTransfer = DoubleEntry {
        _de_fromAccount = buyer,
        _de_toAccount   = seller,
        _de_amount      = toAmount,
        _de_currency    = toCurrency
        }
      trade = Trade fromTransfer toTransfer
      (mNewBid, mNewAsk) = case d1 `compare` d2 of
        -- Case 1: Buyer is done; seller still has inventory
        LT -> let newAsk = Tagged $ LimitOrder {
                    _lorder_user       = seller,
                    _lorder_fromAmount = n2 - numUnits,
                    _lorder_toAmount   = sellerPrice
                    }
              in (Nothing, Just newAsk)
        -- Case 2: Seller is out; buyer needs more
        GT -> let newBid = Tagged $ LimitOrder {
                    _lorder_user       = buyer,
                    _lorder_fromAmount = n1 - numUnits,
                    _lorder_toAmount   = buyerPrice
                    }
              in (Just newBid, Nothing)
        -- Case 3: Buyer and seller exactly traded
        EQ -> (Nothing, Nothing)
  in if buyerPrice >= sellerPrice
     -- Bid has crossed the ask, so we can generate a trade.
     then (mNewBid, mNewAsk, Just trade)
     -- Bid is less than ask, so they can't be merged.
     else (Just bid, Just ask, Nothing)
{% endhighlight %}

`mergeBid` is subtle enough that you probably don't feel comfortable trusting its implementation based on visual inspection alone. In the next section, we'll install automated sanity checks on every write so that any implementation bugs will be blocked.

## Security

How do we stop our exchange from losing money when we inevitably get hacked?

The most important things to secure are the actual Bitcoin or Ethereum wallets our exchange has. Luckily, we avoided that issue by not having any wallets.

The second most important thing is to have append-only backups that we can roll back to if we detect a hack. That's not ideal, because we still have to tell all of our customers we got hacked.

The third most important thing is to avoid losing the money in the first place.

In [my last article](http://www.michaelburge.us/2017/08/25/writing-a-formally-verified-porn-browser-in-coq.html), I showed how you can use theorem provers to formally prove that certain properties hold regarding your data structures. We can't quite do that in Haskell, but let me define a few invariants for you anyways and I'll show you a trick.

### Invariants
Here are our invariants in words:
* Users can't trade with themselves
* Users can't have negative balances
* Users can't have more money in pending trades than they have in their account.
* Users and OrderBooks can't have currencies that don't exist

And here they are in code:
* `User`s can't trade with themselves
{% highlight haskell %}
type ConsistencyCheck = Exchange -> STM Bool

consistency_noSelfTrades :: ConsistencyCheck
consistency_noSelfTrades = \exchange -> do
  trades <- readTVar $ _exchange_trades exchange
  return $ all checkTrade trades
  where
    checkDe DoubleEntry{..} = _de_fromAccount /= _de_toAccount
    checkTrade Trade{..} = checkDe _trade_from && checkDe _trade_to
{% endhighlight %}

* Users can't have negative balances
{% highlight haskell %}
consistency_noNegativeBalances :: ConsistencyCheck
consistency_noNegativeBalances = \exchange -> do
  bals <- userBalances exchange
  let checkUser (userId, balances) =
        flip all (M.toList balances) $ \(currency, balance) ->
        if balance >= 0
        then True
        else error $ "Negative balance for " <> show (userId, currency, balance)
  return $ all checkUser $ M.toList bals

userBalances :: Exchange -> STM (M.Map UserId Balances)
userBalances = undefined
{% endhighlight %}

* Users can't have more money in pending trades than they have in their account.
{% highlight haskell %}
consistency_ordersBackedByAccount :: ConsistencyCheck
consistency_ordersBackedByAccount = \exchange -> do
  usersBals <- userBalances exchange

  let checkUserBalance :: Balances -> (Currency, Amount) -> Bool
      checkUserBalance userBals (currency, bookAmount) =
        case M.lookup currency userBals of
          Nothing -> False
          Just userAmount -> userAmount >= bookAmount

  let checkUser :: (UserId, Balances) -> STM Bool
      checkUser (user, userBals) = do
        bookBals <- userBookBalances exchange user
        let currenciesPending = M.toList bookBals
        return $ all (checkUserBalance userBals) currenciesPending
  allM checkUser $ M.toList usersBals

userBookBalances :: Exchange -> UserId -> STM Balances
userBookBalances = undefined
{% endhighlight %}

* `User`s and `OrderBook`s can't have nonexistent currencies
{% highlight haskell %}
consistency_allCurrenciesExist :: ConsistencyCheck
consistency_allCurrenciesExist = \exchange -> do
  usersBals <- userBalances exchange
  bookBals <- bookBalances exchange
  let valid currency = currency `elem` allCurrencies
      checkBals bals = all valid $ M.keys bals
      usersCheck = all checkBals usersBals
      booksCheck = all valid $ M.keys bookBals
  return $ usersCheck && booksCheck

bookBalances :: Exchange -> STM Balances
bookBalances = undefined
{% endhighlight %}

As long as those functions always return true, we can have some confidence in the rest of our code.

`userBalances`, `bookBalances`, and `userBookBalances` roll up the trades and external transfers to get a final balance. I'll leave their implementation in the [Github](https://github.com/MichaelBurge/lambda-exchange) repository.

### The Trick

People often use triggers or constraints in relational databases to automatically enforce invariants. Using Haskell's Software Transactional Memory library, we can do something similar:

{% highlight haskell %}
installSanityChecks :: Exchange -> IO ()
installSanityChecks exchange =
  atomically $ mapM_ installCheck [
    (consistency_noNegativeBalances, "No negative balances"),
    (consistency_ordersBackedByAccount, "Orders must be backed by account"),
    (consistency_allCurrenciesExist, "Non-existent currency"),
    (consistency_noSelfTrades, "Users cannot trade with themselves")
    ]
  where
    installCheck (check, message) = alwaysSucceeds $ do
      b <- check exchange
      if b
        then return ()
        else error message
{% endhighlight %}

`atomically` enters a "transaction" in the `STM` sense. It's designed as an efficient way to allow multiple threads to concurrently update a shared data structure. Transactions can abort and retry if there are concurrent updates to the same variable. We can also abort if one of our sanity checks fails.

The `alwaysSucceed` function will run the sanity check once, and if it passes run it after every transaction afterwards. It will roll back the transaction with an exception if the sanity check fails with an exception.

We'll call `installSanityChecks` near the start of our program, after we initialize or load our exchange's state. Then every write will be automatically sanity-checked and rolled back with an exception. Our HTTP library `warp` will catch the exception and abort the request.

## Networking

We want 5 API endpoints:
* List orders
* Cancel an order
* Add an order
* Create money on the exchange
* List balances

Here are the request types:
{% highlight haskell %}
data Request_ListOrders = Request_ListOrders {
  _reqListOrders_user :: Maybe UserId
  } deriving (Eq, Show, Generic)

data Request_CancelOrder = Request_CancelBid {
  _reqCancelOrder_bid :: Bid
  } | Request_CancelAsk {
  _reqCancelOrder_ask :: Ask
  } deriving (Eq, Show, Generic)

data Request_AddOrder = Request_AddBid {
  _reqAddOrder_bid :: Bid
  } | Request_AddAsk {
  _reqAddOrder_ask :: Ask
  } | Request_AddMBid {
  _reqAddOrder_mbid :: MBid
  } | Request_AddMAsk {
  _reqAddOrder_mask :: MAsk
  } deriving (Eq, Show, Generic)

data Request_CreateMoney = Request_CreateMoney {
  _reqCreateMoney_singleEntry :: SingleEntry
  } deriving (Eq, Show, Generic)

data Request_ListBalances = Request_ListBalances deriving (Eq, Show, Generic)
{% endhighlight %}

And the response types:
{% highlight haskell %}
data Response_ListOrders = Response_ListOrders {
  _resListOrders_orders :: [LimitOrder]
  } deriving (Eq, Show, Generic)

data Response_CancelOrder = Response_CancelOrder deriving (Eq, Show, Generic)
data Response_AddOrder = Response_AddBid {
  _resAddOrder_trades :: [Trade]  
  } | Response_AddAsk {
  _resAddOrder_trades :: [Trade]
  } | Response_AddMBid {
  _resAddOrder_mbidRemainder :: Maybe MBid,
  _resAddOrder_trades        :: [Trade]
  } | Response_AddMAsk {
  _resAddOrder_maskRemainder :: Maybe MAsk,
  _resAddOrder_trades        :: [Trade]
  } deriving (Eq, Show, Generic)

data Response_CreateMoney = Response_CreateMoney deriving (Eq, Show, Generic)

data Response_ListBalances = Response_ListBalances {
  _resListBalances_externals :: [(UserId, Currency, Amount)],
  _resListBalances_internals :: [(UserId, Currency, Amount)],
  _resListBalances_helds     :: [(UserId, Currency, Amount)],
  _resListBalances_totalBals :: [(UserId, Currency, Amount)],
  _resListBalances_bookBals  :: [(Currency, Amount)]
  } deriving (Eq, Show, Generic)
{% endhighlight %}

Anyone that wants to interact with our exchange will end up creating a `Request` and receive an appropriate `Response`. Here is the actual entrypoint to the server:

{% highlight haskell %}
import Network.Wai as W
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status

le_port = 2345

serverMain :: IO ()
serverMain = do
  state <- initialize
  installSanityChecks state
  putStrLn $ "Listening on " ++ show le_port
  run le_port $ \req respond ->
    let ?req = req
        ?respond = respond
        ?state = state
    in do
      print req
      body <- strictRequestBody req
      case (pathInfo req, requestMethod req) of
        ("createMoney" : _, "POST") -> withParsedRequest body api_createMoney
        ("listOrders" : _,  "POST") -> withParsedRequest body api_listOrders
        ("addOrder" : _,    "POST") -> withParsedRequest body api_addOrder
        ("cancelOrder" : _, "POST") -> withParsedRequest body api_cancelOrder
        ("listBalances" : _, "POST") -> withParsedRequest body api_listBalances
        _ -> respond (W.responseLBS status404 [] "Unknown path")

initialize :: IO Exchange
initialize = Exchange <$> newTVarIO Q.empty <*> newTVarIO (newBook ("USD", "BTC")) <*> newTVarIO Q.empty

type HandlerT a = (?req :: Request, ?respond :: (Response -> IO ResponseReceived), ?state :: Exchange) => a -> IO ResponseReceived

withParsedRequest :: FromJSON a => BSL.ByteString -> HandlerT (a -> IO ResponseReceived)
withParsedRequest bs handler = case decode bs of
  Nothing -> ?respond (W.responseLBS status400 [] "Unable to parse")
  Just x -> handler x
{% endhighlight %}

And here's how to implement one of our handlers:

{% highlight haskell %}
api_listOrders :: HandlerT Request_ListOrders
api_listOrders _ = do
  let Exchange{..} = ?state
  book <- readTVarIO _exchange_book
  ?respond $ W.responseLBS status200 [] $ encode $ Response_ListOrders $ toList book
{% endhighlight %}

I've also included JSON serializers and deserializers in the [Github](https://github.com/MichaelBurge/lambda-exchange) repository.

### Testing

How do we actually end-to-end test our Bitcoin exchange?

The first step is to print one of your request values to get its corresponding JSON:
{% highlight haskell %}
print $ encode $ Request_AddBid $ Tagged $ LimitOrder {
    _lorder_user = 1
    _lorder_fromAmount = 4600,
    _lorder_toAmount = 1
    }
{% endhighlight %}

The second step is to use a shell script to send that JSON to the server:
{% highlight bash %}
cat <<'EOF' | curl localhost:2345/addOrder -XPOST -d @-
{
  "bid":{
    "toAmount":1,
    "user":1,
    "fromAmount":4600
  },
  "tag":"Request_AddBid"
}
EOF
{% endhighlight %}

At this point, you can use any combination of the 5 endpoints to change or inspect the exchange's state.

## Conclusion

We showed how you can implement a simple order book in Haskell, which can be the basis of a full-blown Bitcoin exchange. Future articles may cover:

* Writing the order book in C for efficiency, and using that in the larger Haskell program.
* Writing a program that watches for actual Bitcoin to be sent, so money can enter our exchange.
* Using unit tests to validate the order book implementation.
* Adding multiple currency pairs to the exchange.
* Adding authentication, so users won't have unrestricted access to the exchange.