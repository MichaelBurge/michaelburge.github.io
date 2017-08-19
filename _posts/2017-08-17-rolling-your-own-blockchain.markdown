---
layout: post
author: Michael Burge
title: "Rolling your Own Blockchain in Haskell"
started_date: 2017-08-17 08:52:00
date: 2017-08-17 08:52:00
tags:
  - haskell
---

Bitcoin and Ethereum provide a decentralized means of handling money, contracts, and ownership tokens. From a technical perspective, they have a lot of moving parts and provide a good way to demo a programming language.

This article will develop a simple blockchain-like data structure, to demonstrate these in Haskell:

* Writing a binary serializer and deserializer
* Using cryptographic primitives to calculate hashes
* Automatically adjusting the difficulty of a miner in response to computation time.

We'll name it Haskoin. Note that it won't have any networking or wallet security until a future article.

## What is a Blockchain?

The first step when writing any software application is always to figure out your data structures. This is true whether it's Haskell, Perl, C, or SQL. We'll put the major types and typeclass instances in their own module:

{% highlight haskell %}
{-# LANGUAGE GeneralizedNewtypeDeriving, NoImplicitPrelude, DeriveTraversable, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Haskoin.Types where

import Protolude
import Crypto.Hash

import Control.Comonad.Cofree
import Data.Data
import qualified Data.Vector as V

newtype Account = Account Integer deriving (Eq, Show, Num)

data Transaction = Transaction {
  _from   :: Account,
  _to     :: Account,
  _amount :: Integer
  } deriving (Eq, Show)

newtype BlockF a = Block (V.Vector a) deriving (Eq, Show, Foldable, Traversable, Functor, Monoid)
type Block = BlockF Transaction

type HaskoinHash = Digest SHA1

data BlockHeader = BlockHeader {
  _miner       :: Account,
  _parentHash  :: HaskoinHash
  } deriving (Eq, Show)

data MerkleF a = Genesis
               | Node BlockHeader a
               deriving (Eq, Show, Functor, Traversable, Foldable)

type Blockchain = Cofree MerkleF Block
{% endhighlight %}

`MerkleF` is a higher-order Merkle tree type that adds a layer onto some other type. The `Cofree MerkleF Block` does two things: It recursively applies `MerkleF` to produce a type for all depths of Merkle trees, and it attaches an annotation of type `Block` to each node in the tree.

When using `Cofree`, `anno :< xf` will construct one of these annotated values.

It will be more useful to look at an "inverted" tree where each node knows its parent, rather than one where each node knows its children. If each node knew its children, adding a single new block to the end would require changing every node in the tree. So `MerkleF` produces a chain, not a tree.

[`Protolude`](https://hackage.haskell.org/package/protolude) is a replacement `Prelude` that I've been using recently in moderately-sized projects. `Prelude` has a lot of backwards-compatibility concerns, so a lot of people shut it off with the `NoImplicitPrelude` language extension and import a custom one.

Why do we choose this weird `MerkleF` type over the simpler one below?

{% highlight haskell %}
newtype Block = Block (V.Vector Transaction)
data Blockchain = Genesis Block
                | Node Block BlockHeader Blockchain
{% endhighlight %}

The main reason is to get those `Functor`, `Traversable`, and `Foldable` instances, because we can use them to work with our Merkle tree without having to write any code. For example, given a blockchain

{% highlight haskell %}
import qualified Data.Vector as V

let genesis_block = Block (V.fromList [])
let block1 = Block (V.fromList [Transaction 0 1 1000])
let genesis_chain = genesis_block :< Genesis
let chain1 = block1 :< Node (BlockHeader { _miner = 0, _parentHash = undefined }) genesis_chain
let chain2 = block1 :< Node (BlockHeader { _miner = 0, _parentHash = undefined }) chain1
{% endhighlight %}

, here's how you can get all of its transactions:
{% highlight haskell %}
let txns = toList $ mconcat $ toList chain2
-- [Transaction {_from = Account 0, _to = Account 1, _amount = 1000},Transaction {_from = Account 0, _to = Account 1, _amount = 1000}]
let totalVolume = sum $ map _amount txns
-- 2000
{% endhighlight %}

I tested the above using `stack ghci` to enter an interactive prompt.

Real blockchains have a lot of useful things in the header, such as timestamps or nonce values. We can add them to `BlockHeader` as we need them.

# Constructing Chains

A bunch of abstract types that are awkward to use aren't very useful by themselves. We need a way to mine new blocks to do anything interesting. In other words, we want to define `mineOn` and `makeGenesis`:

{% highlight haskell %}
module Haskoin.Mining where

type TransactionPool = IO [Transaction]

mineOn :: TransactionPool -> Account -> Blockchain -> IO Blockchain
mineOn pendingTransactions minerAccount root = undefined

makeGenesis :: IO Blockchain
makeGenesis = undefined
{% endhighlight %}

The genesis block is pretty easy, since it doesn't have a header:
{% highlight haskell %}
makeGenesis = return $ Block (V.fromList []) :< Genesis
{% endhighlight %}

We can write `mineOn` without any difficulty, transaction limiting, or security pretty easily if we knew how to calculate a parent node's hash:

{% highlight haskell %}
mineOn :: TransactionPool -> Account -> Blockchain -> IO Blockchain
mineOn pendingTransactions minerAccount parent = do
  ts <- pendingTransactions
  let block = Block (V.fromList ts)
  let header = BlockHeader {
        _miner = minerAccount,
        _parentHash = hash parent
        }
  return $ block :< Node header parent

hash :: Blockchain -> HaskoinHash
hash = undefined
{% endhighlight %}

`Crypto.Hash` has plenty of ways to hash something, and we've chosen `type HaskoinHash = Digest SHA1` earlier. But in order to use it, we need some actual bytes to hash. That means we need a way to serialize and deserialize a `Blockchain`. A common library to do that is [`binary`](https://hackage.haskell.org/package/binary), which provides a `Binary` typeclass that we'll implement for our types.

It's not difficult to write instances by hand, but one of the advantages of using weird recursive types is that the compiler can generate `Binary` instances for us. Here's complete code to serialize and deserialize every type we need:
{% highlight haskell %}
{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Haskoin.Serialization where

import Haskoin.Types
import Control.Comonad.Cofree
import Crypto.Hash
import Data.Binary
import Data.Binary.Get
import Data.ByteArray
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Vector.Binary
import GHC.Generics

instance (Binary (f (Cofree f a)), Binary a) => Binary (Cofree f a) where
instance (Binary a) => Binary (MerkleF a) where
instance Binary BlockHeader where
instance Binary Transaction where
deriving instance Binary Account
deriving instance Binary Block

deriving instance Generic (Cofree f a)
deriving instance Generic (MerkleF a)
deriving instance Generic BlockHeader
deriving instance Generic Transaction
instance Binary HaskoinHash where
  get = do
    mDigest <- digestFromByteString <$> (get :: Get BS.ByteString)
    case mDigest of
      Nothing -> fail "Not a valid digest"
      Just digest -> return digest
  put digest = put $ (convert digest :: BS.ByteString)

deserialize :: BSL.ByteString -> Blockchain
deserialize = decode

serialize :: Blockchain -> BSL.ByteString
serialize = encode
{% endhighlight %}

I only included `deserialize` and `serialize` to make it clearer what the end result of this module is. Let's drop them in favor of `decode` and `encode` from `Data.Binary`.

`Generic` is a way of converting a value into a very lightweight "syntax tree" that can be used be serializers(JSON, XML, Binary, etc.) and many other typeclasses to provide useful default definitions. The [Haskell wiki](https://wiki.haskell.org/GHC.Generics) has a good overview. `binary` uses these `Generic` instances to define serializers that work on just about anything.

We had to hand-write a `Binary` instance for `HaskoinHash` because `Digest SHA1` from the `Crypto.Hash` library didn't provide it or a `Generic` instance. That's okay - digests are pretty much bytestrings anyways, so it was only a few lines.

Here's how to use them to implement `mineOn`:
{% highlight haskell %}
import Crypto.Hash(hashlazy)

mineOn :: TransactionPool -> Account -> Blockchain -> IO Blockchain
mineOn pendingTransactions minerAccount parent = do
  ts <- pendingTransactions
  let block = Block (V.fromList ts)
  let header = BlockHeader {
        _miner = minerAccount,
        _parentHash = hashlazy $ encode parent
        }
  return $ block :< Node header parent
{% endhighlight %}

And here's how to test that this actually works:
{% highlight haskell %}
testMining :: IO Blockchain
testMining = do
  let txnPool = return []
  chain <- makeGenesis
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  return chain

-- GHCI
> chain <- testMining
Block [] :< Node (BlockHeader {_miner = Account 0, _parentHash = efb3febc87c41fffb673a81ed14a6fb4f736df79}) (
  Block [] :< Node (BlockHeader {_miner = Account 0, _parentHash = 2accb557297850656de70bfc3e13ea92a4ddac29}) (
    Block [] :< Node (BlockHeader {_miner = Account 0, _parentHash = f51e30233feb41a228706d1357892d16af69c03b}) (
      Block [] :< Node (BlockHeader {_miner = Account 0, _parentHash = 0072e83ae8e9e22d5711fd832d350f5a279c1c12}) (
        Block [] :< Node (BlockHeader {_miner = Account 0, _parentHash = c259e771b237769cb6bce9a5ab734c576a6da3e1}) (
          Block [] :< Genesis)))))
> encode chain
"\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC4\239\179\254\188\135\196\US\255\182s\168\RS\209Jo\180\247\&6\223y\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC4*\204\181W)xPem\231\v\252>\DC3\234\146\164\221\172)\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC4\245\RS0#?\235A\162(pm\DC3W\137-\SYN\175i\192;\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC4\NULr\232:\232\233\226-W\DC1\253\131-5\SIZ'\156\FS\DC2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC4\194Y\231q\178\&7v\156\182\188\233\165\171sLWjm\163\225\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
> (decode $ encode chain) :: Blockchain
Block [] :< Node (BlockHeader {_miner = Account 0, _parentHash = efb3febc87c41fffb673a81ed14a6fb4f736df79}) (
  Block [] :< Node (BlockHeader {_miner = Account 0, _parentHash = 2accb557297850656de70bfc3e13ea92a4ddac29}) (
    Block [] :< Node (BlockHeader {_miner = Account 0, _parentHash = f51e30233feb41a228706d1357892d16af69c03b}) (
      Block [] :< Node (BlockHeader {_miner = Account 0, _parentHash = 0072e83ae8e9e22d5711fd832d350f5a279c1c12}) (
        Block [] :< Node (BlockHeader {_miner = Account 0, _parentHash = c259e771b237769cb6bce9a5ab734c576a6da3e1}) (
          Block [] :< Genesis)))))
{% endhighlight %}

If you're testing serialization code at home, you may prefer to use the `base16-bytestring` library to hex-encode your `ByteString`s:
{% highlight haskell %}
> import qualified Data.ByteString.Base16.Lazy as BSL
> chain <- testMining
> BSL.encode $ encode chain
00000000000000000100000000000000000000000014efb3febc87c41fffb673a81ed14a6fb4f736df79000000000000000001000000000000000000000000142accb557297850656de70bfc3e13ea92a4ddac2900000000000000000100000000000000000000000014f51e30233feb41a228706d1357892d16af69c03b000000000000000001000000000000000000000000140072e83ae8e9e22d5711fd832d350f5a279c1c1200000000000000000100000000000000000000000014c259e771b237769cb6bce9a5ab734c576a6da3e1000000000000000000
{% endhighlight %}

Note that it will probably be a PITA for a C programmer trying to follow our serialization/deserialization code because the byte-wrangling is hidden in a lot of really generic code. If you want to produce a spec for people to use(always a good idea), you'll probably want to hand-roll your serialization code so it's self-documenting.

# Mining

There are a couple mining-related problems with this so-called blockchain:
1. People can have negative balances, so people can create a "scapegoat account" that they transfer unlimited amounts of money from.
2. There is no transaction limiting, so someone could create a huge block and run our miners out of memory.
3. We always mine empty blocks, so nobody can transfer money.
4. There is no difficulty, so miners aren't proving they've done any work.

I say that these are all mining problems because the code that miners run is going to deal with them.

#3 we'll wait for __Networking__ to solve. The rest we can do now.

To solve #1, we need account balances for anyone with a transaction that we're mining a block for. Let's go ahead and calculate all possible account balances:
{% highlight haskell %}
blockReward = 1000

balances :: Blockchain -> M.Map Account Integer
balances bc =
  let txns = toList $ mconcat $ toList bc
      debits = map (\Transaction{ _from = acc, _amount = amount} -> (acc, -amount)) txns
      credits = map (\Transaction{ _to = acc, _amount = amount} -> (acc, amount)) txns
      minings = map (\h -> (_minerAccount h, blockReward)) $ headers bc
  in M.fromListWith (+) $ debits ++ credits ++ minings
{% endhighlight %}

And then once we have a parent blockchain, we know how to filter out the invalid transactions:
{% highlight haskell %}
validTransactions :: Blockchain -> [Transaction] -> [Transaction]
validTransactions bc txns =
  let accounts = balances bc
      validTxn txn = case M.lookup (_from txn) accounts of
        Nothing -> False
        Just balance -> balance >= _amount txn
  in filter validTxn txns
{% endhighlight %}

To solve #2, I'll let the current miner choose however many transactions he wants to put in his block. That means I'll put a constant `globalTransactionLimit = 1000` at the top that we'll use when mining, but we won't verify past blocks using it.

To solve #4, we need to add a nonce field to our `BlockHeader` that the miner can increment until he finds a good hash. We'll make it an arbitrarily-large integer to avoid the scenario that no nonce values yield a sufficiently-difficult hash. And since we want to adjust our difficulty so blocks take roughly the same time to mine, we'll store a timestamp in the header.
{% highlight haskell %}
import Data.Time.Clock.POSIX

-- Add new fields
data BlockHeader = BlockHeader {
  _miner       :: Account,
  _parentHash  :: HaskoinHash,
  _nonce       :: Integer,
  _minedAt     :: POSIXTime
  } deriving (Eq, Show)

-- Add serializers for POSIXTime
instance Binary POSIXTime where
  get = fromInteger <$> (get :: Get Integer)
  put x = put $ (round x :: Integer)

globalTransactionLimit = 1000

mineOn :: TransactionPool -> Account -> Blockchain -> IO Blockchain
mineOn pendingTransactions minerAccount parent = do
  ts <- pendingTransactions
  ts <- return $ validTransactions parent ts
  ts <- return $ take globalTransactionLimit ts
  loop ts 0
  where
    validChain bc = difficulty bc < desiredDifficulty parent
    loop ts nonce = do
      now <- getPOSIXTime
      let header = BlockHeader {
            _miner = minerAccount,
            _parentHash = hashlazy $ encode parent,
            _nonce = nonce,
            _minedAt = now
            }
          block = Block (V.fromList ts)
          candidate = block :< Node header parent
      if validChain candidate
        then return candidate
        else loop ts (nonce+1)

difficulty :: Blockchain -> Integer
difficulty = undefined

desiredDifficulty :: BlockChain -> Integer
desiredDifficulty = undefined
{% endhighlight %}

We enter `loop` and keep incrementing the counter and fetching the time until we find a candidate with the right difficulty. The actual difficulty of a `Blockchain` is just its hash converted to an integer:
{% highlight haskell %}
import Crypto.Number.Serialize(os2ip)

difficulty :: Blockchain -> Integer
difficulty bc = os2ip $ (hashlazy $ encode bc :: HaskoinHash)
{% endhighlight %}

How do we know what the right difficulty is? To start with, we'll calculate the average time-between-blocks for the last 100 blocks:
{% highlight haskell %}
numBlocksToCalculateDifficulty = 100

blockTimeAverage :: BlockChain -> NominalDiffTime
blockTimeAverage bc = average $ zipWith (-) times (tail times)
  where
    times = take numBlocksToCalculateDifficulty $ map _minedAt $ headers bc

headers :: BlockChain -> [BlockHeader]
headers Genesis = []
headers (_ :< Node x next) = x : headers next

average :: (Foldable f, Num a, Fractional a, Eq a) => f a -> a
average xs = sum xs / (if d == 0 then 1 else d) where d = fromIntegral $ length xs
{% endhighlight %}

Let's have a target time of 10 seconds. Suppose `blockTimeAverage bc` gives 2 seconds, so we want blocks to take 5 times as long: `adjustmentFactor = targetTime / blockTimeAverage bc` = 5. Which means we want only `1/5` of the originally-accepted blocks to be accepted.

Since hashes are uniformly-distributed, `1/5` of the original hashes are less than `originalDifficulty / 5`, which will be our new difficulty. [That's what Bitcoin does](https://bitcoin.stackexchange.com/questions/855/what-keeps-the-average-block-time-at-10-minutes): `difficulty = oldDifficulty * (2 weeks) / (time for past 2015 blocks)`.

{% highlight haskell %}
genesisBlockDifficulty = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
targetTime = 10

-- BEWARE: O(n * k), where k = numBlocksToCalculateDifficulty
desiredDifficulty :: Blockchain -> Integer
desiredDifficulty x = round $ loop x
  where
    loop (_ :< Genesis) = genesisBlockDifficulty
    loop x@(_ :< Node _ xs) = oldDifficulty / adjustmentFactor
      where
        oldDifficulty = loop xs
        adjustmentFactor = min 4.0 $ targetTime `safeDiv` blockTimeAverage x
{% endhighlight %}

Here are a few recent mining times using these calculations:
{% highlight haskell %}
> exampleChain <- testMining
> exampleChain <- mineOn (return []) 0 exampleChain -- Repeat a bunch of times
> mapM_ print $ map blockTimeAverage $ chains exampleChain
6.61261425s
6.73220925s
7.97893375s
12.96145975s
10.923974s
9.59857375s
7.1819445s
2.2767425s
3.2307515s
7.215131s
15.98277575s
{% endhighlight %}

They hover around 10s because `targetTime = 10`.

# Persistence

We'll save the blockchain on disk, and give people 3 tools:
* A tool to mine blocks and create a new chain
* A tool to list account balances

The first tool is the miner:
{% highlight haskell %}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Haskoin.Cli.Mine where

import Haskoin.Mining
import Haskoin.Serialization
import Haskoin.Types

import Protolude
import System.Environment
import Data.Binary
import qualified Data.ByteString.Lazy as BSL
import System.Directory
import Prelude(read)

defaultChainFile = "main.chain"
defaultAccount = "10"

main :: IO ()
main = do
  args <- getArgs
  let (filename, accountS) = case args of
        [] -> (defaultChainFile, defaultAccount)
        [filename] -> (filename, defaultAccount)
        [filename, account] -> (filename, account)
        _ -> panic "Usage: mine [filename] [account]"
      swapFile = filename ++ ".tmp"
      txnPool = return []
      account = Account $ read accountS
  forever $ do
    chain <- loadOrCreate filename makeGenesis :: IO Blockchain
    newChain <- mineOn txnPool account chain
    encodeFile swapFile newChain
    copyFile swapFile filename
    print "Block mined and saved!"

loadOrCreate :: Binary a => FilePath -> (IO a) -> IO a
loadOrCreate filename init = do
  exists <- doesFileExist filename
  if exists
    then decodeFile filename
    else do
      x <- init
      encodeFile filename x
      return x
{% endhighlight %}

The second one prints all of the account balances
{% highlight haskell %}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Haskoin.Cli.ListBalances where

import Haskoin.Mining
import Haskoin.Serialization
import Haskoin.Types

import Protolude
import System.Environment
import Data.Binary
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BSL

defaultChainFile = "main.chain"

main :: IO ()
main = do
  args <- getArgs
  let (filename) = case args of
        [] -> (defaultChainFile)
        [filename] -> (filename)
        _ -> panic "Usage: list-balances [filename]"
  chain <- decodeFile filename :: IO Blockchain
  forM_ (M.toAscList $ balances chain) $ \(account, balance) -> do
    print (account, balance)
{% endhighlight %}

Here's its output:
{% highlight haskell %}
$ stack exec list-balances
(Account 10,23000)
{% endhighlight %}

So I've apparently mined 23 blocks just testing `stack exec mine`.

## Conclusion

We developed a simple blockchain data structure. You can browse the repository on [Github](https://github.com/MichaelBurge/haskoin).

Future Haskoin-related articles may cover
* Using networking and concurrency primitives to set up a peer-to-peer network.
* Securing accounts in wallets, so other people can't transfer money out of your account.
* Building a 'blockchain explorer' website
* GPU-accelerating our hashing
* FPGA-accelerating our hashing

Future cryptocurrency-related articles may cover:
* You may have heard about proof-of-work and proof-of-stake. What about proof-of-proof - where the miners compete to prove novel theorems in an approriate logic?
* Adding a Turing-complete scripting language
* Better ways to parse command line options
* Building a Bitcoin exchange
