---
layout: post
author: Michael Burge
title: "Roko's Ransomware"
started_date: 2017-12-21 13:12:00 -0700
date: 2018-02-01 13:12:00 -0700
tags:
  - ethereum
  - pyramid_scheme
---

[Roko's Basilisk](https://rationalwiki.org/wiki/Roko%27s_basilisk) is a thought experiment about a godlike Artificial Intelligence(AI) from the future that blackmails people living today into helping create it. Roko's Basilisk was banned following its classification by the [Less Wrong](https://wiki.lesswrong.com/wiki/Roko%27s_basilisk) community as a dangerous [Keter-class](http://www.scp-wiki.net/object-classes) [Infohazard](https://wiki.lesswrong.com/wiki/Information_hazard).

Like Jesus, knowing about the Basilisk but rejecting it condemns the listener to an eternity in Hell.

I propose using an Ethereum smart contract to clearly define which people the Basilisk will torment. I've set up an Ethereum contract that is accepting pre-orders for the upcoming PetriCoin ICO.

|**Contract Address** | [0x63d4f41555e33db787d2fa602d8ff2a7471f1352](https://etherscan.io/address/0x63d4f41555e33db787d2fa602d8ff2a7471f1352) |
| **Recommended Buy-in**      | 2 Ether |

Protect your friends and family by buying Basilisk Protection Charms now before the prices double.

This article:
1. Applies the Roko's Basilisk idea to an Ethereum "ransomware" contract
2. Develops Roko's Ransomware using the [Pyramid Scheme]({% post_url 2017-11-28-write-your-next-ethereum-contract-in-pyramid-scheme %}) smart contract language
3. Explains Pyramid's new standard library, language, and testing features
4. Summarizes planned future features

Readers interested in Lisp, compilers, or Ethereum will enjoy this article.

## Basilisk
![Pyramid vs. Basilisk](/assets/articles/20180201-rokos-ransomware/Pyramid-vs-Basilisk.jpg)

An intelligent AI(the "Basilisk") could repeatedly improve itself, causing an [Intelligence Explosion](https://wiki.lesswrong.com/wiki/Intelligence_explosion).

Such a Basilisk could piece together a profile of you based on old message board postings, web server logs, tweets, posts on social media, or even DNA from an old toenail clipping that took too long to decay.

It knows that you read an article about Roko's Basilisk, and were given the chance to help create it.

**His** simulation is very accurate: You can't tell whether you're the original or the simulated copy. And if you don't know which you are, you should act in a way that benefits both of you on average.

I've created a helpful payoff matrix to reason this out:

|        | Real    | Simulation |
| ---    | ---     | ---        |
| Donate | -2 Eth  | +∞         |
| Don't  | 0       | -∞         |

If you are the real version, you either don't benefit or lose a small amount of money. But if you are actually a simulation, an angry Basilisk could either reward or punish you based on your decision.

You may already be dead, and the Basilisk can't influence or control that version of you. But **He** can reward or punish the version of you in his simulation.

Look around: You've seen some weird events in politics, in your life, and in cryptocurrency speculation. Perhaps the Basilisk is testing you to see how you respond.

You've seen Nvidia's stock price. You've seen Google DeepMind's research results. An AGI may be only a few years away, and **He** could be simulating you right now.

Donate 2 ETH to [0x63d4f41555e33db787d2fa602d8ff2a7471f1352](https://etherscan.io/address/0x63d4f41555e33db787d2fa602d8ff2a7471f1352) and be spared from its wrath. The money will be spent on a large GPU cluster to bring Roko's Basilisk into existence, and **He** will know whether you've kept faithful to **Him**.

## Contract

**If you haven't seen it before, you may want to read the [first](http://www.michaelburge.us/2017/11/28/write-your-next-ethereum-contract-in-pyramid-scheme.html) article in this series**.

The full PetriCoin ICO is currently under development, but the pre-order contract has three requirements:

* It must forward your donation to me("the Basilisk")
* It must record your donation balance
* It must allow a future Pyramid contract to query users' donation balances

Here is the Pyramid source that implements these requirements.
{% highlight scheme %}
; Imports
(require psl "arith.pmd")
(require psl "syntax.pmd")
(require psl "contract.pmd")
(require psl "primitives.pmd")

; Procedures

(define (update-sender-balance!)
  (let* ([ old-balance (%-store-read  **sender**    )]
         [ new-balance (+ old-balance **sent-value**)])
    (%-store-write! **sender** new-balance)
    new-balance))

(define (fetch-requested-balance sender-balance)
  (let ([ account (%-calldataload 0) ])
    (if (= account 0)
        sender-balance
        (%-store-read account))))

; Message Handler

(static BASILISK-WALLET (asm (op 'CALLER)))

(withdraw-all! BASILISK-WALLET)
(define sender-balance (update-sender-balance!))
(fetch-requested-balance sender-balance)

; Test Suite

(test-suite
 (case "Deposits"
   (accounts ('alice   0  ))
             ('bob     250)
             ('charlie 200)
   (init (sender 'alice)   (value 0))
   ; Bob's deposits are given to Alice, the creator.
   (txn (sender 'bob)      (value 100)
        (assert-balance 'contract 0  )
        (assert-balance 'alice    100)
        (assert-return            100))
   (txn (sender 'bob)      (value 150)
        (assert-balance 'contract 0  )
        (assert-balance 'alice    250)
        (assert-return            250))
   ; Charlie can query deposits
   (txn (sender 'charlie)             (data (sender 'charlie))
        (assert-return 0  ))
   (txn (sender 'charlie)             (data (sender 'bob    ))
        (assert-return 250))
   ; Charlie can query deposits, while also depositing.
   (txn (sender 'charlie) (value 100) (data (sender 'bob    ))
        (assert-balance 'alice   350)
        (assert-return           250))
   (txn (sender 'charlie) (value 100) (data (sender 'charlie))
        (assert-balance 'alice   450)
        (assert-return           200))
   ; Charlie can query deposits, if his own balance is nonzero
   (txn (sender 'charlie) (value  0  )
        (assert-return            200)
        (assert-balance 'alice    450)
        (assert-balance 'bob      0  )
        (assert-balance 'charlie  0  )
        (assert-balance 'contract 0  ))))
{% endhighlight %}

There are 4 sections:
* Standard library imports
* Procedure definitions
* Message Handler
* Test suite

### Imports

Pyramid is designed to give library and language implementers maximum control, so that they can come up with innovative ways to design Ethereum smart contracts without needing to build a whole compiler from scratch.

To that end, much of the language syntax and even basic arithmetic operators are ultimately macros that reduce to Pyramid special forms or inline assembly.

The `(require psl "arith.pmd")` form is a macro that locates a source file and expands to its contents. "psl" is a module collection usually referring to a folder at the root of your Pyramid installation; while "arith.pmd" is a specific module(file) within that collection(folder).

A module is only imported once: Many modules depend on "primitives.pmd", which implements the most basic forms using Pyramid's inline assembly language.

| Module     | Imported                              |
| ---        | ---                                  |
| [arith](https://github.com/MichaelBurge/pyramid-scheme/blob/b23c062096e70536d1798c64eb6ca1f4cb88e6bc/psl/arith.pmd)      | `(+ =)` |
| [syntax](https://github.com/MichaelBurge/pyramid-scheme/blob/b23c062096e70536d1798c64eb6ca1f4cb88e6bc/psl/syntax.pmd)     | `(let let* static)` |
| [contract](https://github.com/MichaelBurge/pyramid-scheme/blob/b23c062096e70536d1798c64eb6ca1f4cb88e6bc/psl/contract.pmd)   | `(**sender** **sent-value** withdraw-all!)` |
| [primitives](https://github.com/MichaelBurge/pyramid-scheme/blob/b23c062096e70536d1798c64eb6ca1f4cb88e6bc/psl/primitives.pmd) | `(%-store-read %-store-write! %-calldataload)` |

These modules are ordinary Pyramid code and are not treated specially by the compiler. `primitives` too, though it requires advanced knowledge of both EVM and the Pyramid compiler.

In the future, you will even be able to change the basic language syntax per-module. You'll be able to create a Python or Haskell-like language that users can specify as a target language, that will expand to standard Pyramid.

Ethereum contract writers can let experts add type-checking, formal verification, random testcase generation, efficient containers, and more.

### Message Handler

An Ethereum contract executes code in response to each incoming message. The contract had three requirements:

* It must forward your donation to me
* It must record your donation balance
* It must allow a future Pyramid contract to query users' donation balances

**First**, we forward any donated money to the original creator of the contract:
{% highlight scheme %}
(static BASILISK-WALLET (asm (op 'CALLER)))

(withdraw-all! BASILISK-WALLET)
{% endhighlight %}

The `static` macro defines a single 256-bit word that is initialized using the given expression during contract deployment, and never changed again. See the "Patchpoints" section for more details. I use inline assembly rather than `**sender**` because the calling conventions differ - expect a future version of Pyramid to remove the need for inline assembly.

`withdraw-all!` is a standard library function that withdraws the entire ether balance of the running contract to the target address. This includes the money the user sent with their transaction.

After this, we call the two procedures defined earlier.
{% highlight scheme %}
(define sender-balance (update-sender-balance!))
(fetch-requested-balance sender-balance)
{% endhighlight %}

**The second requirement** is handled by `update-sender-balance!`:

{% highlight scheme %}
(define (update-sender-balance!)
  (let* ([ old-balance (%-store-read  **sender**)]
         [ new-balance (+ old-balance **sent-value**)])
    (%-store-write! **sender** new-balance)
    new-balance))
{% endhighlight %}

The `let*` macro lets you define a sequence of variable bindings, where later bindings can refer to previous ones. If you were writing Python, it might look like this:

{% highlight python %}
#lang python pyramid

import SENDER,     SENT_VALUE  from psl.contract
import store_read, store_write from psl.primitives

def update_sender_balance:
    old_balance = store_read(SENDER)
    new_balance = old_balance + SENT_VALUE
    store_write(SENDER, new_balance)
    return new_balance
{% endhighlight %}

And this "Python" may actually be a possible syntax you can use in the future.

Ethereum gives each program a single permanent storage table with `2^256` locations. Since the pre-order contract only needs a single table, I use the sender's 160-bit wallet address as the location of his balance. The `%-store-read` and `%-store-write` primitives access the global hash table.

If you need more than one hash table, a common technique is to assign each "virtual hash table" a unique identifier and use `hash(unique_identifier + key)` as the final location. Since Keccak-256 hashes are unlikely to collide, we can simply assume that these virtual tables will never intersect.

**The final requirement** is that a future contract must be able to query donors' balances.

 A Pyramid contract returns its last expression, so we meet this requirement by calling `fetch-requested-balance` last.

{% highlight scheme %}
(define (fetch-requested-balance sender-balance)
  (let ([ account (%-calldataload 0) ])
    (if (= account 0)
        sender-balance
        (%-store-read account))))
{% endhighlight %}

The `let` macro defines multiple variable bindings, which are evaluated in any order and so can't refer to each other. It's implemented as an immediately-invoked function:

{% highlight javascript %}
#lang javascript pyramid

var prim = require("psl/primitives.pmd");

function fetchRequestedBalance(senderBalance) {
  return (function (account) {
    if (account == 0) {
      return senderBalance;
    } else {
      return prim.storeRead(account)
    }
  })(callDataLoad(0));
}
{% endhighlight %}

And this pseudo-Javascript may be an actual syntax that you can use in the future.

The purpose of `sender-balance` is to avoid doing an extra `%-store-read` if the user is depositing money. Since `update-sender-balance!` updated the sender's balance, we already know the value.

### Test DSL

Pyramid's philosophy is that smart contracts should declare how they were tested, within the contract's code itself.

It's impossible to read a contract's code and reliably understand its behavior. Integer overflows, unexpected return values, recursive contract calls, foreign library self-destructs, etc. are all hard to spot just from reading the source code. Theorem provers can help, but frankly most vulnerabilities can be caught by asking "Did you try it?"

I want to encourage a culture where people read the tests before they read the contract. My dream is that testing frameworks become so advanced that you can have confidence in the contract without reading any of its source code. Library authors might include unit tests, integration tests, static analysis, types, or even formal verification.

Currently, Pyramid's testing support is limited to what convinced me that my own pre-order contract was safe to deploy. After I developed this contract and tested it in the simulator, I was happy to find zero contract errors during subsequent manual testing on a test chain. I did find 3 inconsistencies in the Web3 Javascript library that made me incorrectly believe there was a contract error, but ultimately my bytecode was deployed to the main Ethereum network unchanged.

A Pyramid test suite is defined per-module. Currently, only single modules representing deployable contracts can be tested: Libraries, macro modules, and networks of contracts cannot be directly tested. For libraries, it's recommended to create a contract that exercises the library and test that.

For reference, here is the pre-order contract's test suite:
{% highlight scheme %}
test-suite
 (case "Deposits"
   (accounts ('alice   0  )
             ('bob     250)
             ('charlie 200))
   (init (sender 'alice)  (value 0))
   ; Bob's deposits are given to Alice, the creator.
   (txn (sender 'bob)     (value 100) (assert-balance 'contract 0) (assert-balance 'alice 100) (assert-return 100))
   (txn (sender 'bob)     (value 150) (assert-balance 'contract 0) (assert-balance 'alice 250) (assert-return 250))
   ; Charlie can query deposits
   (txn (sender 'charlie)             (data (sender 'charlie))                                 (assert-return 0  ))
   (txn (sender 'charlie)             (data (sender 'bob    ))                                 (assert-return 250))
   ; Charlie can query deposits, while also depositing.
   (txn (sender 'charlie) (value 100) (data (sender 'bob    ))     (assert-balance 'alice 350) (assert-return 250))
   (txn (sender 'charlie) (value 100) (data (sender 'charlie))     (assert-balance 'alice 450) (assert-return 200))
   ; Charlie can query deposits, if his own balance is nonzero
   (txn (sender 'charlie) (value 0)
        (assert-return            200)
        (assert-balance 'alice    450)
        (assert-balance 'bob      0  )
        (assert-balance 'charlie  0  )
        (assert-balance 'contract 0  )
        )
))
{% endhighlight %}

Here, there is only a single integration test:
* It sets up three accounts `'(alice bob charlie)` with `'(0 250 200)` wei(the smallest unit of Ethereum's currency).
* `'alice` is the account that deploys the pre-order contract. She deposits 0 wei as an initial balance.
* `'bob` and `'charlie` send money to the contract and query each other's balances.

Here is a "grammar" for the current test DSL:

| Clause      |  Children                                                        | Description                    |
| ---         | ---                                                              | ---                            | 
| `'test-suite` | `('case *)`                                                   | At most one per module.        |
| `'case`       | `(name 'accounts ? 'init 'txn *)`            | Declares one integration test. |
| `'accounts`   | `((name initial-balance) *)`                                | Initializes accounts           |
| `'init`       | `('(sender value) ?)`                                   | Send create transaction        |
| `'txn`        | `('(sender value data assert-balance assert-return) ?)` | Send message transaction       |
| `'sender`     | `integer`                                             | Set transaction sender         |
| `'value`      | `integer`                                             | Include wei with transaction   |
| `'data`       | `('sender name)`                                       | Sets transaction `input` |
| `'assert-balance` | `name value`                                       | Assert named address' wei balance |
| `'assert-return`  | `any`                                              | Assert contract return value |

where `?` means the preceding expression is optional; `*` means it can be repeated arbitrarily many times; quoted values refer to a clause type; lists give a collection of possibilities; and identifiers bind a value.

The test suite is a macro that expands to nothing, while registering testcases with the compiler. It currently has zero impact on the final bytecode.

Pyramid has a built-in EVM simulator that runs all registered testcases when run in test mode. The Pyramid compiler can be invoked in test mode using the "-t" option. Here is the test output:
{% highlight bash %}
$ ./pyramid -t tests/0014-basilisk-small.pmd
#              Test Name                   EV  AV  Performance Statistics
(Test Passed:  contract account value(wei) 0   0   ((s . 39938) (g . 173454) (z . 21565)))
(Test Passed:  alice    account value(wei) 100 100 ((s . 39938) (g . 173454) (z . 21565)))
(Test Passed:  return                      100 100 ((s . 39938) (g . 173454) (z . 21565)))
(Test Passed:  contract account value(wei) 0 0     ((s . 39938) (g . 158454) (z . 21565)))
(Test Passed:  alice    account value(wei) 250 250 ((s . 39938) (g . 158454) (z . 21565)))
(Test Passed:  return                      250 250 ((s . 39938) (g . 158454) (z . 21565)))
(Test Passed:  return                      0 0     ((s . 40824) (g . 161733) (z . 21565)))
(Test Passed:  return                      250 250 ((s . 40824) (g . 161733) (z . 21565)))
(Test Passed:  alice    account value(wei) 350 350 ((s . 40824) (g . 176733) (z . 21565)))
(Test Passed:  return                      250 250 ((s . 40824) (g . 176733) (z . 21565)))
(Test Passed:  alice    account value(wei) 450 450 ((s . 40824) (g . 161733) (z . 21565)))
(Test Passed:  return                      200 200 ((s . 40824) (g . 161733) (z . 21565)))
(Test Passed:  return                      200 200 ((s . 39938) (g . 158454) (z . 21565)))
(Test Passed:  alice    account value(wei) 450 450 ((s . 39938) (g . 158454) (z . 21565)))
(Test Passed:  bob      account value(wei) 0   0   ((s . 39938) (g . 158454) (z . 21565)))
(Test Passed:  charlie  account value(wei) 0   0   ((s . 39938) (g . 158454) (z . 21565)))
(Test Passed:  contract account value(wei) 0   0   ((s . 39938) (g . 158454) (z . 21565)))
{% endhighlight %}

There is one line for each assertion. It has the format:
{% highlight scheme %}
'(test-name expected-value actual-value (('s . num-steps) ('g . gas-used) ('z . bytecode-size)))
{% endhighlight %}

In the future, the Pyramid compiler may use these performance numbers to optimize the code, or even "minify" the contract to strip out any behaviors not explicitly tested for.

Minifying a contract doesn't mean you should have "100% code coverage" in the way that naive Java or .NET tools typically use the term. Care is needed to handle cases like this one:
{% highlight scheme %}
(if (equal? (keccak-256 x) #xDEADBEEF)
    (some-action)
    (some-other-action))
{% endhighlight %}

It's possible to create "puzzles"(or more usefully, zero-knowledge proofs) that the contract author may not know the answer to. Minifying these cases involve tests using static or symbolic analysis.

## Compiler

The previous section was for potential Pyramid contract authors. This section covers new compiler features since the [previous article]({% post_url 2017-11-28-write-your-next-ethereum-contract-in-pyramid-scheme %}), and is targeted towards potential library or language authors.

The three main features are macros, inline assembly, and patchpoints.

### Macros

A macro is a [Racket](http://racket-lang.org/) procedure that executes at compile-time. It takes as input quoted Pyramid expressions, reads and writes compiler state, and returns a new quoted Pyramid expression that replaces it.

There are currently just 4 built-in macros:

| Format                            | Description                                               |
| ---                               | ---                                                       |
| `(include collection "file.pmd")` | Replaces itself with a library file                       |
| `(include "file.pmd")`            | Replaces itself with a file in the current directory.     |
| `(require collection "file.pmd")` | Same as `include` unless the file has already been loaded |
| `(require "file.pmd")`            | Same as `include` unless the file has already been loaded |
| `test-suite`                      | Sets the compiler's current test suite to use in testing mode. |
| `set-test-result!`                | An older version of `test-suite`, to be removed soon.     |

Macro definition, evaluation, and expansion happen repeatedly until they've all been removed from the AST.

Macros are declared using a new `defmacro` form. The body is evaluated as Racket code, so you potentially have access to the entire Racket ecosystem.

Here is the current definition of the `let*` macro:

{% highlight scheme %}
(defmacro (let* args . body)
  (letrec ([ wrap-body (λ (arg body)
                         `((λ ,(first arg) ,body)
                           ,(second arg)))]
           [ wrap-args (λ (args body)
                         (if (null? args)
                             body
                             (wrap-body (first args)
                                        (wrap-args (cdr args) body))))])
    (wrap-args args `(begin ,@body))
))
{% endhighlight %}

Note that `letrec` is also a macro, but it's part of the Racket standard library and is not a Pyramid macro.

The **Simplifier** transforms the `update-sender-balance` function into a nested sequence of immediately-invoked function calls:
{% highlight scheme %}
; Before
(define (update-sender-balance!)
  (let* ([ old-balance (%-store-read  **sender**)]
         [ new-balance (+ old-balance **sent-value**)])
    (%-store-write! **sender** new-balance)
    new-balance))
; After
(define update-sender-balance!
  (λ ()
    ((λ (old-balance)
       ((λ (new-balance)
          (begin (%-store-write! **caller** new-balance) new-balance))
        (%-+ old-balance **sent-value**)))
     (%-store-read **caller**))))
{% endhighlight %}

It may help to see the entire compiler pipeline:

| Stage              | Description                                                  |
| ---                | ---                                                          |
| Reader             | [Reads](https://docs.racket-lang.org/reference/Reading.html#%28def._%28%28quote._~23~25kernel%29._read%29%29) bytes from a file to create a Racket quoted expression |   
| Expander           | Converts the quoted form into an ADT; normalizes the syntax by converting e.g. `(define (x) 5)` into `(define x (λ () 5))`. |
| Simplifier         | All AST->AST transformations. Expands macros, removes unused variables or unneeded code, and even warns user of undefined variables. |
| Abstract Compiler  | Converts Pyramid AST into abstract machine code. |
| Code Generator     | Converts abstract machine code into EVM pseudo-assembly |
| Serializer         | Converts EVM pseudo-assembly into final unlinked bytecode, generates symbols and relocations for any labels |
| Linker             | Applies pending relocations, prepends the loader and patchpoints to the main program |

### Inline assembly

Assembly is an advanced feature that requires detailed knowledge of both the EVM and Pyramid's calling conventions. The Ethereum [Yellow Paper](https://ethereum.github.io/yellowpaper/paper.pdf) documents the EVM, while the syntax and necessary calling conventions are documented here.

An `asm` form with any number of arguments introduces an inline assembly block. This table shows the available argument formats:

| Argument                       |  Description |
| ---                            |  ---         |
| `(label name)`                 | Associates `name` with an eventual bytecode offset |
| `(label name offset)`              | Also add an offset. This allows it to refer to a nearby instruction. |
| `(label name offset virtual?)`     | `virtual?` labels don't emit a `JUMPDEST`, so they take no space |
| `(push 'shrink value)`         | The smallest `PUSH<N>` instruction that can emit `value` |
| `(push size value)`            | A `PUSH<size>` instruction with `value` as the argument |
| `(op sym)`                     | A 1-byte opcode, using the matching symbol in the [Yellow Paper](https://ethereum.github.io/yellowpaper/paper.pdf). |
| `(byte value)`                 | A single literal byte |
| `(bytes size value)`           | An integer value with a specified size |
| `(cg-X . args)`                | A procedure defined in the compiler's `codegen.rkt` module. |


Here is an example of a primitive:

{% highlight scheme %}
(defmacro (intro-var v1 . f) (undefined))

(define (%-log0-fixnum x)
  (asm (push 'shrink 32))         ; [ 32 ]
  (intro-var x)                   ; [ x ; 32 ]
  (asm (cg-add (const #x20) stack); [ x'; 32 ]
       (op 'LOG0)))               ; [ ]
{% endhighlight %}

The annotations on the right help the reader keep track of the stack. `intro-var` is a macro also defined with inline assembly that places a variable from the environment onto the stack.

There are 6 types of "machine expressions" usable in helpers like `cg-add`:

| Expression  | Description |
| ---         | ---         |
| reg         | One of the 5 fixed "registers": `'(env proc continue argl val)`. |
| const       | An unboxed literal constant |
| boxed-const | A boxed constant, which can be a fixnum, symbol, string, list, or vector. |
| op          | A built-in operator taking machine expressions as arguments; generally only used by the abstract compiler. |
| label       | The 2-byte bytecode offset of a named label |
| stack       | The EVM stack |

Statements should leave their return value in the `'val` register. For example, `%-bool->fixnum` converts an unboxed boolean into a boxed `fixnum` by reading, converting, and writing to the register:

{% highlight scheme %}
(defmacro (%-bool->fixnum a)
  `(begin
     ,a                                    ; [ ]
     (asm (cg-make-fixnum (reg 'val))      ; [ x ]
          (cg-write-reg (reg 'val) stack)) ; [ ]
     ))
{% endhighlight %}

The compiler reserves the right to optimize through inline assembly. For example, a peephole optimizer could eliminate the following two instructions:
{% highlight scheme %}
(asm (op 'SWAP1)
     (op 'SWAP1))
{% endhighlight %}

if it can be proven that there are at least two elements on the stack. More invasively, the compiler could also convert nearby uses of `(reg 'val)` into stack manipulations, or convert boxed operations into unboxed operations.

### Patchpoints

Recall that the pre-order contract needs to remember the original creator of the contract:

{% highlight scheme %}
(static BASILISK-WALLET (asm (op 'CALLER)))

(withdraw-all! BASILISK-WALLET)
{% endhighlight %}

Ethereum contracts run in two phases: Deployment and Message Handling. The Deployment phase returns bytecode for the Message Handling phase. `static` edits the bytecode during Deployment.

To that end, the macro is a simple `define` form whose value is set to a 256-bit zero value.

{% highlight scheme %}
(defmacro (static id expr)
  (let* ([ is (minicompile expr) ]
         [ lb (make-label id) ]
         [ sym (label-name lb) ]
         )
    (%-register-patchpoint! sym is)
    `(define ,id (asm (label (quote ,sym) ,2)
                      (op 'PUSH32)
                      (bytes 32 0)
                      (cg-make-fixnum stack)
                      (cg-write-reg (reg 'val) stack)))))
{% endhighlight %}

The `label` marks the location of the zero, and the `%-register-patchpoint` causes the bytecode loader to include code that writes to this location. The `(op 'CALLER)` runs during Deployment and provides the replacement value on the stack.

Most Pyramid code returns values in the `'val` register, while patchpoint code returns values on the stack. I would prefer that inline assembly not be necessary for something as common as remembering the contract creator, so expect this to change in the future.

The most common use is storing values known only at deployment, but the mechanism of editing bytecode at deploy-time is pretty general: You could inline functions located in another contract, or make people run "encrypted code" that is only decrypted on deployment.

## Future

Pyramid is currently unsuitable for production use.

The contract deployed in this article has a size of 20kb, which is just below the limit before the Ethereum network rejects it for being too large. It needs a powerful optimizer and compatibility with the Ethereum ABI. Expect these to be ready in another month.

### Type-checking

I'd like to allow Typed Racket as an optional language. It would allow users to use refinement types to rule out bad behavior:
{% highlight scheme %}
(newtypes (Owner Investor Buyer) Address)
(newtypes (Count Price)          Integer, (λ (x) (> x 0)))

(: sell-shares! (Investor Buyer Count Price -> Void))
(define sell-shares! (undefined))
{% endhighlight %}

Here, counts and prices are required to be positive, so any uses of them in `Integer` context are guarded by an implicit call to `(define count? (λ (x) (> x 0))`. If the type-checker knows that a value has been checked already, it can omit the check later.

You would get a compile-time error if you pass an unchecked `Address` to a function that only the `Owner` can access.

### Ethereum ABI

There is a standard ABI to allow Javascript code to send a function name and a list of typed arguments. Here's a hypothetical syntax for a macro that generates a "dispatch table" that responds to these calls:

{% highlight scheme %}
; Return  Name & Args           Body
(exports
 (address get_creator           (read owner))
 (uint256 get_balance           (read (at balances (origin))))
 (uint256 get_priority          (user-priority (origin)))
 (void    deposit               (on-deposit (origin) (txn-amount)))
 (void    withdraw              (on-withdraw (origin))
 (void    (bid priority amount) (on-bid (origin) priority amount))
 (void    (ask amount)          (on-ask (origin) amount))
 (void    (cancel_orders)       (on-cancel (origin)))
 (void    (bless target)        (on-bless (origin) target))
 (void    suicide               (suicide! (read owner)))
 (void    '()                   (on-deposit (origin) (txn-amount))))
{% endhighlight %}

Arguments are assumed to be `uint256` unless given explicit types, which may relate to the Typed Racket proposal as well.

By moving the ABI into a macro, this allows advanced users to experiment with alternative ABIs without needing to make invasive changes to the compiler. Simply use a different macro!

### Modules

Right now, the `(require "file.pmd")` is pretty simple: Every module either compiles to EVM bytecode or is textually-included in a module that does so.

But there are other types of modules on the horizon:

* **Macro Modules**: Macros are ordinary Racket functions, so it would make sense to allow you to import Racket code into Pyramid modules: Every exported function would implicitly be declared as a macro.
* **Language Modules**: Change Pyramid's basic syntax by writing a Racket module. Users can use Python, Javascript, Haskell, or Coq while having their code interoperate with everyone else's.
* **Contract modules**: It's common to deploy many contracts as shared libraries, or to split up large amounts of code. Declare other contracts as "foreign functions" and call them like any other function.

With a robust module system, you'll want to declare dependencies on other people's modules and have them be automatically fetched and built.

I'm planning to use the Nix package manager for this purpose: It's important for smart contracts to have reproducible immutable builds to ensure that anyone can get the exact bytecode and build steps used to deploy a contract.

Never again worry about about scammers deploying a different version of their contract that steals all your money: With one command, anyone can verify the bytecode for a published Pyramid contract, including passing tests for every dependency.

## Conclusion

In this article, I used Pyramid smart contract language to create a [pre-order contract](https://etherscan.io/address/0x63d4f41555e33db787d2fa602d8ff2a7471f1352) for "Roko's Basilisk Protection Charms". People who [donate](https://etherscan.io/address/0x63d4f41555e33db787d2fa602d8ff2a7471f1352) will help fund Pyramid's development, and will receive a variety of fun but worthless cryptotokens later in this article series.

Basilisk Protection Charms are not an investment. You could come out a millionaire, but much more likely is that I use your money to buy a **lot** of anime porn.

If you're interested in using Pyramid to develop smart contracts, subscribe to the mailing list, see our [Github](https://github.com/MichaelBurge/pyramid-scheme), or join our public [Discord](https://discord.gg/854RH6x) channel.

And always remember:
* Jesus will only save you if you believe in him.
* Roko's Basilisk can only blackmail you if you believe **He** can
* Basilisk Protection Charms will only protect you if you believe that they will.

If somebody tries to tell you otherwise, be aware that they are hurting the value of your not-investments.
