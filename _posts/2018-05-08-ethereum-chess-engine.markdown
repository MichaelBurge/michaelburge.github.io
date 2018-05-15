---
layout: post
author: Michael Burge
title: "DSLs for Ethereum Contracts"
started_date: 2018-05-08 12:40:00 -0700
date: 2018-05-15 3:40:00 -0700
tags:
  - pyramid_scheme
  - c
---

A good smart contract language is a $1 billion problem.

Why? Look at the amounts lost in some recent hacks:
* [Parity - $300 million](https://medium.com/chain-cloud-company-blog/parity-multisig-hack-again-b46771eaa838)
* [DAO - $50 million](https://www.coindesk.com/understanding-dao-hack-journalists/)
* [PoWHCoin - $1 million](https://blog.goodaudience.com/how-800k-evaporated-from-the-powh-coin-ponzi-scheme-overnight-1b025c33b530)

How are people changing their development process in response?

Ethereum is leaning towards [SMT solvers](https://github.com/Z3Prover/z3) for general use, and [theorem provers](https://coq.inria.fr/) for parts of the design. [Cardano](https://www.cardano.org/en/home/) aims for more upfront design, leaning heavily on [type-based tools](https://www.haskell.org/) and the [K Framework](http://www.kframework.org/index.php/Main_Page).

In this article, I present **Language-oriented Programming** as an alternative. Experts create specialized languages for modeling, tokens, or atomic cross-chain swaps. Contract developers use them to describe the relevant parts of their contract.

Experts know the rules to write secure contracts. However, those rules are not consistently followed. Contract developers are not experts, and even experts can slip up when reviewing a large amount of code.

Specialized languages consistently apply security rules, and reduce the amount of code to be reviewed.

[Pyramid](https://www.michaelburge.us/2017/11/28/write-your-next-ethereum-contract-in-pyramid-scheme.html) is a language for writing smart contracts using language-oriented programming. People can develop new programming languages that interoperate with every other language.

As a technical demonstration, I develop a C compiler and port a [C chess engine](https://www.michaelburge.us/2017/09/10/injecting-shellcode-to-speed-up-amazon-redshift.html) to run on Ethereum's Virtual Machine(EVM). Contract developers are not expected to use C, but it's a simple realistic language that almost all programmers know.

Finally, I give a status update on Pyramid for people interested in using the language.

## Pyramid Modeling Language

With smart contracts, "the code is law". But who can read and trust 1000 lines of subtle code?

To start with, it's not often clear what it means for a contract to be "correct". People know something's a problem after they've lost money, not before.

I present a language for describing security properties, where every requirement fits in a tweet. I apply this language to Ethereum's ERC-20 token standard.

Tokens are tradeable virtual currencies. As an example, there are many gamers or Ethereum miners with video cards who could rent them to AI researchers: A token could be exchanged for GPU time.

Ethereum or Cardano have their own base currencies **Ether** and **Ada**, so you might wonder why people issue new currencies rather than exchange the base currency. There are three common reasons:

* **Diffusion**: People want to invest in an idea, not in individual actions. A Vanguard Index Fund allows you to invest in "the US economy" without reviewing company business plans, and a token allows you to invest in "a marketplace for GPUs" without leasing space in a data center and sourcing video cards.
* **Commitment**: Since tokens are only usable for a specific purpose, investors as a whole can only exit by creating real productive demand. This makes the idea more likely to succeed.
* **Restriction**: Some tokens need additional trading restrictions. Examples: Allowing users to purchase tokens with a credit card or bank transfer, or checking that users are Accredited Investors.

Since Diffusion, Commitment, and Restriction all apply to our hypothetical GPU marketplace, a new token would be useful. So that it is tradeable on exchanges, it implements the ERC-20 interface:

{% highlight javascript %}
contract ERC20Interface {
    function totalSupply() public constant returns (uint);
    function balanceOf(address tokenOwner) public constant returns (uint balance);
    function allowance(address tokenOwner, address spender) public constant returns (uint remaining);
    function transfer(address to, uint tokens) public returns (bool success);
    function approve(address spender, uint tokens) public returns (bool success);
    function transferFrom(address from, address to, uint tokens) public returns (bool success);
    event Transfer(address indexed from, address indexed to, uint tokens);
    event Approval(address indexed tokenOwner, address indexed spender, uint tokens);
}
{% endhighlight %}

Solidity accurately describes the data flow for these methods. But that doesn't tell you what these methods do. For that, we need **laws**.

I'll use the hypothetical **Pyramid Modeling Language**(PML) to describe properties that can hold for contracts implementing this interface. Here's one:

### 1. Addresses hold the tokens
{% highlight ruby %}
@totalSupply = sum(@balanceOf $address, $address)
{% endhighlight %}


This law states that if you add up all the tokens that every user holds, you'll get the `totalSupply()`.

Not every ERC-20 contract satisfies this law: If there is a two-week "holding period" before you can sell after buying, the developer may include unvested tokens in `totalSupply` but only count vested tokens in user's `balanceOf`.

However, if it's true for your particular contract, you can state with just one line that your contract cannot have tokens minted or destroyed by hiding them in things other than user's balances.

A PML **formula** has 3 classes of free variables:
* The `@` sigil means the identifier refers to a contract method.
* A `$` sigil can be substituted with any **value** to generate a new formula.
* An unmarked variable stands for terms in the modeling language.

A formula with no `$` sigils is said to be an **expression**. Some expressions - such as ones involving the `=` operator - are **propositions**. A law is a formula that reduces to propositions after `$`-substitution.

The `sum` special form substitutes all possible values for `$address` into the formula `@balanceOf $address` and sums the results.

Here's another law:
### 2. Transfers don't change the total supply
{% highlight perl %}
@totalSupply = @transfer $_ $_
               @totalSupply
{% endhighlight %}

The initial state of a contract is arbitrary. The `=` "forks the universe" from this initial state, and compares two different actions in each.

On the left-hand side, we call the contract's `totalSupply` method. On the right-hand side, we first call `transfer` with any two arguments and then call `totalSupply`. `=` requires the values returned by `totalSupply` to be the same.

A sequence of expressions evaluates to the last expression. A law must hold in all possible initial states and assignments to `$` variables.

Not every ERC-20 satisfies this property: Some tokens will "tax" every transaction in the token. Most do, however, and it's important to know if these two lines hold for your favorite token.

There are even stronger versions of this property:
### 3. Nothing changes the total supply after initialization
{% highlight perl %}
@initialize _
$result = @totalSupply
_
$result = @totalSupply
{% endhighlight %}


The `initialize` method is not part of the ERC-20 standard, but some contracts will have it anyways. This law then states that "After the contract has been initialized, there is no possible way to change the `totalSupply` of the token". The `_` stands for "Any sequence of method calls".

Since `totalSupply` is unsigned, it satisfies `@totalSupply >= 0`. This lets property #3 rule out integer overflow errors that generate huge amounts of tokens. The PoWHCoin hack was an example of this.

Each `=` places a constraint on `totalSupply`. If the supply ever changes, it is impossible to give `$result` two different values so one of the two constraints will be violated.

Properties 1,2, and 3 are universal properties: They hold in all possible situations. Some laws have conditions before they hold. We can use `if` to restrict them:
### 4. Delegated transfers don't change the total supply
{% highlight perl %}
@totalSupply = if @allowance $owner $spender >= $amount
                  { @transferFrom $owner $_ $amount ; @totalSupply }
                  @totalSupply
{% endhighlight %}


Delegated transfers allow users to grant other users or contracts permission to spend their tokens. A cryptocurrency exchange with thousands of tokens might require this permission in order to enable trading.

This law mainly checks that `transferFrom` does not throw an exception, because the left-hand of `=` did not throw an exception.

The first line of the `if` expression is the **condition**. The other lines are the **consequent** and **alternative**.

The `{ X ; Y }` is the indentation-insensitive syntax for sequentially executing two statements.

Property 4 isn't very useful because it is imprecise. I mainly used it to introduce the syntax. Here's a more precise specification of `transfer`:

### 5. If a transfer changes a user's balance, they sent or received.

{% highlight perl %}
$balance = @balanceOf $addr
@{$from}transfer $to $amount
$newBalance = @balanceOf $addr
if $newBalance = $balance
   _
   ($addr = $from & $balance-$newbalance = $amount) |
   ($addr = $to   & $newBalance-$balance = $amount)
{% endhighlight %}

Here, there are two additional syntaxes:
1. The `@{$from}transfer` syntax constrains `$from` to the address that called `transfer`.
2. The `&` and `|` operators combine multiple propositions.

### Using PML

This modeling language says a lot in only a few lines. But what can you do with a specification?

* **Communication**: Security auditors and token investors could use this language to talk about contracts without changing the development process. No extra code has to be written.
* **Test Generation**: Replace the `$` variables with random or developer-chosen values and test whether the properties hold. Some care must be taken generating addresses and cryptographic hashes.
* **Model Checking**: The specifications can be sent to a model-checker like Z3. If the contract itself can also be converted to Z3, then it may be possible to formally prove the contract's security properties.

A custom language can communicate intent more efficiently than implementation, and allows special-purpose tools to check that intent.

The goal of Pyramid is to allow experts to create special-purpose languages just like this modeling language. DSLs can be specifications, executable code, or test frameworks.

In the next section, I'll show the nuts-and-bolts of how I created a new language.

## C Compiler

Pyramid builds on [Racket](http://racket-lang.org/). Since Racket focuses on language development there are libraries for implementing [Type Systems](https://docs.racket-lang.org/turnstile/index.html), [Semantics Engineering](https://docs.racket-lang.org/redex/), [Parsing](http://docs.racket-lang.org/brag/), and [Program Synthesis and Verification](https://emina.github.io/rosette/).

Using Pyramid as a code generator, anyone can create advanced smart contract languages.

Pyramid's flagship language is a Scheme dialect, but in this section I create a C compiler that can compile a [Chess Engine](https://github.com/MichaelBurge/ceagle/blob/master/test-support/chess-engine.c) that I wrote in a [previous article](https://www.michaelburge.us/2017/09/10/injecting-shellcode-to-speed-up-amazon-redshift.html). The engine successfully calculated a Chess [`perft`](https://chessprogramming.wikispaces.com/Perft+Results?responseToken=0b5319559f293333b682ffb8f4b744c78).

I based my implementation on Matthew Butterick's [Beautiful Racket](https://beautifulracket.com/), so that interested readers can use his well-written book as a guide. Matthew is a lawyer and web designer who uses language-oriented programming to publish books like [Practical Typography](http://practicaltypography.com/) and [Typography for Lawyers](http://typographyforlawyers.com/).

### Overview

A custom language begins with the first line:

{% highlight c %}
#lang ceagle

typedef unsigned __bits 64 uint64_t;
typedef unsigned int bool;

...
{% endhighlight %}

Every Pyramid-based language has a `#lang` line. `ceagle` is the name of a Racket [collection](https://docs.racket-lang.org/reference/collects.html) that describes how to read and interpret the rest of the file.

Only this first line has a predefined syntax - everything else can be completely customized.

The **Ceagle** C compiler has the following compiler stages:

| Stage                                                                           | Output              |
| ---                                                                             | ---                 |
| [Lexer](https://github.com/MichaelBurge/ceagle/blob/master/lexer.rkt)           | Token sequence/[type information](https://en.wikipedia.org/wiki/The_lexer_hack) |
| [Parser](https://github.com/MichaelBurge/ceagle/blob/master/parser.rkt)         | Parse Tree          |
| [Expander](https://github.com/MichaelBurge/ceagle/blob/master/expander.rkt)     | Syntax Tree         |
| [Simplifier](https://github.com/MichaelBurge/ceagle/blob/master/simplifier.rkt) | Syntax Tree         |
| [Compiler](https://github.com/MichaelBurge/ceagle/blob/master/compiler.rkt)     | Pyramid Syntax Tree |
| [Macros](https://github.com/MichaelBurge/ceagle/blob/master/builtins.pmd)       | Pyramid Syntax      |

The parser uses the [brag](http://docs.racket-lang.org/brag/) DSL for defining [BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form) grammars.

The interesting work happens in the `Compiler` and `Macros` modules.

A custom Pyramid language's expander turns the source code's syntax into a **Translation Unit** - a bundle of Scheme together with metadata like the dependencies, language-specific parse tree, and language-specific AST.

{% highlight scheme %}
(struct translation-unit ([ language        : Symbol            ]
                          [ source-code     : Sexp              ]
                          [ abstract-syntax : Any               ]
                          [ pyramid-ast     : Pyramid           ]
                          [ dependencies    : translation-units ]
                          ))
{% endhighlight %}

Pyramid and Ceagle are implemented in [Typed Racket](https://docs.racket-lang.org/ts-guide/), so the `struct` above includes type annotations.

Translation units are topologically sorted by `dependencies`. A translation unit exports every definition.

A custom language has two components: The **Reader** and **Expander**. Ceagle's Reader runs the Lexer and Parser stages to create a [Syntax Object](https://docs.racket-lang.org/guide/stx-obj.html); while the Expander is a macro that runs the Expander, Simplifier, and Compiler stages on this syntax to form a Translation Unit.

Custom languages export a `make-translation-unit` function. The `execute?` argument lets them treat the entrypoint module differently: Ceagle invokes the `main` function at the end.

{% highlight scheme %}
; Ceagle's Expander
(define-syntax (c-module-begin stx)
  (syntax-case stx ()
    [(_ parse-tree) ; Starting with the parse tree as input...
     #`(#%module-begin ; Declare a new module...
        #,@(require-stxs #'parse-tree) ; ... that depends on other modules
        (provide make-translation-unit) ; ... and which exports this function

        (define (make-translation-unit execute?)
          (define abstract-syntax (expand-translation-unit parse-tree))
          (define compiled (compile-translation-unit abstract-syntax execute?))
          (translation-unit 'ceagle
                            (quote parse-tree)
                            abstract-syntax
                            compiled
                            dependencies ; Created from require-stxs
                            )))]))
{% endhighlight %}

`expand-translation-unit` and `compile-translation-unit` refer to a [C translation unit](https://en.wikipedia.org/wiki/Translation_unit_(programming)), not a Pyramid `translation-unit`.

C's `#include` only works with C source, but Ceagle has a non-standard `#require` that allows it to import any Pyramid-based language with a guarantee that they will only be compiled once. These are extracted from the parse tree by `require-stxs` above.

Most of the interesting work is in `compile-translation-unit`. The [Beautiful Racket](https://beautifulracket.com/) book mentioned above covers everything before it in greater detail.

To compile a translation unit, I'll provide rules for compiling all 4 classes of abstract syntax:

* Declarations
* Statements
* Expressions
* Types

The [types](https://github.com/MichaelBurge/ceagle/blob/master/types.rkt) module defines all types used in Ceagle, and I've duplicated the important ones below.

### Declarations

A C translation unit consists of top-level declarations: `typedef`, global variables, and function definitions. The entrypoint module should additionally define a function named `main`.

Here are the definitions of these:
{% highlight scheme %}
; Top-level declarations
(struct c-unit      ([ decls : c-declarations        ]))
(struct c-decl-type ([ name  : Symbol                ]
                     [ type  : c-type                ]))
(struct c-decl-var  ([ name  : Symbol                ]
                     [ type  : c-type                ]
                     [ init  : (Option c-expression) ]))
(struct c-decl-func ([ name  : Symbol                ]
                     [ sig   : c-signature           ]
                     [ body  : c-statement           ]))
(define-type c-declaration  (U c-decl-var c-decl-type c-decl-func))
(define-type c-declarations (Listof c-declaration))
{% endhighlight %}

To reduce the number of parentheses, I like to define a plural version `c-declarations` of my singular types `c-declaration`. I will only explicitly define the singular versions in this article.

#### Type Aliases

`typedef`s are implemented by remembering the `name` and `type` association in a table for the rest of the translation unit to use. Since it must return Pyramid syntax as well, it returns an empty `begin` form:

{% highlight scheme %}
(: compile-decl-type (-> c-decl-type Pyramid))
(define (compile-decl-type x)
  (destruct c-decl-type x)
  (register-type! x-name x-type #f)
  (pyr-begin (list)))
{% endhighlight %}

The `destruct` macro defines a variable for each field in the `struct`. So `x-name` refers to `(c-decl-type-name x)`.

See the `Types` section for more details on how typechecking is done.

#### Variable Declarations

The `c-decl-var` is a variable declaration `int x = 5;`. There are two parts:
1. Remembering that `x`'s type is `int` in a table
2. Compiling the declaration to the form `(define x initializer)`, where `initializer` is either the expression given or a default value that depends on the type.

Conceptually, variables are all pointers to `sizeof(type)` bytes. However, values do not have identity so each type has a `variable-definer` macro that assigns a fresh memory location to a value.

{% highlight scheme %}
(: compile-decl-var (-> c-decl-var Pyramid))
(define (compile-decl-var x)
  (destruct c-decl-var x)
  (register-variable! x-name x-type)
  (make-macro-application #`(#,(variable-definer x-type)
                             #,x-name
                             #,(shrink-pyramid
                                (if x-init
                                    (compile-expression x-init 'rvalue)
                                    (compile-default-initializer x-type))))))

(: variable-definer (-> c-type PyramidQ))
(define (variable-definer ty)
  ; Handles typedefs
  (define real-type (resolve-type ty))
  (match real-type
    [(struct c-type-fixed   _) #'%c-define-fixnum ]
    [(struct c-type-struct  _) #'%c-define-struct ]
    [(struct c-type-pointer _) #'%c-define-pointer]
    [(struct c-type-union   _) #'%c-define-union  ]
    ))
{% endhighlight %}

Macros expect and return syntax, so the `shrink-pyramid` function reverses the Expander stage of the compiler, converting the AST back into syntax.

For the `int x = 5` case, the compiler emits a use of the `%c-define-fixnum` macro. This is written in Pyramid, and looks very similar to an ordinary Racket macro:
{% highlight scheme %}
(define-syntax (%c-define-fixnum stx)
  (syntax-case stx ()
    [(_ name init) #'(begin (define name (let ([ ptr (%c-allocate-fixnum) ])
                                           (%c-word-write! ptr init)
                                           ptr))
                            (%c-noinline name))]
    ))

; %#-mem-alloc is a Pyramid Standard Library primitive
(defmacro (%c-allocate-fixnum)
  `(%#-mem-alloc %#-WORD))
{% endhighlight %}

The `%c-noinline` expands to `(set! name name)` and prevents the Pyramid optimizer from attempting to inline the variable: This will not be necessary in a future version of the compiler.

#### Function Declarations

A function declaration looks like `int main() { return 0; }`. A function has several requirements:

* The function name and signature needs to be remembered for later.
* Variables defined inside the function should not escape the body.
* Arguments are passed as **rvalues**, so new variables need to be initialized from them.
* A `return` statement completes the current function call with the given value.
* Functions should be ordinary lambdas so they are usable with other Pyramid programs.

Putting all that together, we get this:
{% highlight scheme %}
(: compile-decl-func (-> c-decl-func Pyramid))
(define (compile-decl-func x)
  (destruct c-decl-func x)
  (destruct c-signature x-sig)
  (: sigvar-init (-> c-sigvar VariableName))
  (define (sigvar-init v) (symbol-append (c-sigvar-name v)
                                         '-init))
  (: vars VariableNames)
  (define vars (map sigvar-init x-sig-args))
  (register-variable! x-name x-sig)
  (declare-variable-scope
   ; (define) a new local variable for each function parameter.
   (define args (for/list : Pyramids ([ arg x-sig-args ])
                  (define arg-name (c-sigvar-name arg))
                  (define arg-type (c-sigvar-type arg))
                  (register-variable! arg-name arg-type)
                  (make-macro-application #`(#,(variable-definer arg-type)
                                             #,arg-name
                                             #,(sigvar-init arg)))))
   ; (define) the function to be a Scheme lambda.
   (pyr-definition x-name
                   (pyr-lambda vars
                               ; quasiquote-pyramid is a macro like Lisp's
                               ; quasiquote that switches between abstract and
                               ; concrete Pyramid syntax
                               (quasiquote-pyramid
                                `(begin ,@args
                                        ,(with-returnpoint
                                           (compile-statement x-body))))))))

(define-syntax-rule (declare-variable-scope xs ...)
  ; Parameterize lets you temporarily modify a global variable.
  ; The symbol table is restored when execution exits the body,
  ; even if an exception occurs.
  (parameterize ([ *variables* (hash-copy (*variables*)) ])
    xs ...))

{% endhighlight %}

Since Pyramid is a Scheme dialect, it supports Scheme's `call/cc`. The `return` statement simply calls a continuation named `return` with the value given.

{% highlight scheme %}
(: with-returnpoint (-> Pyramid Pyramid))
(define (with-returnpoint exp)
  (with-escapepoint 'return exp))

(: with-escapepoint (-> Symbol Pyramid Pyramid))
(define (with-escapepoint name exp)
  (expand-pyramid #`(call/cc (λ (#,name) #,(shrink-pyramid exp)))))
{% endhighlight %}

`break` and `continue` are also implemented as continuations. The `goto` statement cannot be implemented as a continuation like this, because it allows you to jump to labels you haven't visited yet.

This covers declarations. Since I just mentioned `break`, `continue`, and `goto`, I'll cover statements next.

### Statements

There are 4 categories of statements:
* **Labels** are places that can be jumped to. C supports labels directly, but most programmers don't know that the `switch` statement's `case` and `default` clauses are considered labels. C switch statements are not equivalent to cascading if/else statements:

{% highlight c %}
int foo(int x){
  switch (x) while (1)
     case 0: while (1)
     case 1: while (1)
     case 2: return x;
}
{% endhighlight %}

In this example, a value of `x=2` would cause the switch to execute the `return` skipping the three `while` loops. This is not easily translatable to cascading if/else statements.

* **Scopes** allow you to define variables within the body. Many also allow you to `break` or `continue`.
* **Continuations** abort a surrounding scope using a Scheme continuation.
* **Expressions** can be used as statements.

{% highlight scheme %}
; Statements
;; Labels
(struct c-labeled              ([ name        : Symbol                ]
                                [ body        : c-statement           ]))
(struct c-labeled-case         ([ expected    : c-expression          ]
                                [ body        : c-statement           ]))
(struct c-labeled-default      ([ body        : c-statement           ]))
(struct c-goto                 ([ target      : Symbol                ]))

;; Scopes
(struct c-if                   ([ pred        : c-expression          ]
                                [ consequent  : c-statement           ]
                                [ alternative : c-statement           ]))
(struct c-block                ([ body        : c-statements          ]))
(struct c-switch               ([ actual      : c-expression          ]
                                [ body        : c-statement           ]))
(struct c-for                  ([ init        : c-decl-vars           ]
                                [ pred        : (Option c-expression) ]
                                [ post        : (Option c-expression) ]
                                [ body        : c-statement           ]))
(struct c-while                ([ pred        : c-expression          ]
                                [ body        : c-statement           ]))
(struct c-do-while             ([ pred        : c-expression          ]
                                [ body        : c-statement           ]))
;; Continuations
(struct c-return               ([ val         : (Option c-expression) ]))
(struct c-break                (                                       ))
(struct c-continue             (                                       ))

;; Expressions
(struct c-expression-statement ([ exp         : c-expression          ]))

(define-type c-statement (U c-labeled ... c-continue))
{% endhighlight %}

Every entry in **Labels** is implemented as an inline assembly label and `jump` instruction. A `switch` statement emits a jumptable for its `case` and `default` labels.

A C `if` statement compiles to a Scheme `if` expression:
{% highlight scheme %}
(: compile-if (-> c-if Pyramid))
(define (compile-if x)
  (destruct c-if x)
  (pyr-if (compile-expression x-pred 'rvalue)
          (compile-statement  x-consequent)
          (compile-statement  x-alternative)
          ))
{% endhighlight %}

The `for`, `while`, and `do while` loops are implemented using continuations for `break` and `continue`, which are also used to check the loop condition:

{% highlight scheme %}
(: compile-for (-> c-for Pyramid))
(define (compile-for x)
  (destruct c-for x)
  (define init (map compile-declaration x-init))
  (define post (if x-post
                   (compile-expression x-post 'rvalue)
                   (expand-pyramid #'(begin))))
  (define pred (if x-pred
                   (compile-expression x-pred 'rvalue)
                   (expand-pyramid #'#t)))

  (with-breakpoint
    (quasiquote-pyramid
     `(begin ,@init
             (%c-loop-forever
              ,(with-continuepoint
                 (quasiquote-pyramid
                  `(if ,pred
                       (begin ,(compile-statement x-body)
                              ,post
                              (continue 0))
                       (break 0)))))))))
{% endhighlight %}

A C block is like a Scheme `begin`, but it needs to hide any locally-declared variables. So we wrap it in an immediately-invoked lambda:
{% highlight scheme %}
(: compile-block (-> c-block Pyramid))
(define (compile-block x)
  (quasiquote-pyramid
   `((λ ()
       ,(compile-c-sequence (c-block-body x))
       ))))
{% endhighlight %}

This does mean that a `goto` between blocks could miscompile, since the `λ` creates a new continuation frame which needs to be released. The solution is to hoist all variable definitions to the beginning of the function and use `set!` to initialize them, but since I don't use cross-block gotos in my example program I opted to keep it simple.

And finally, an expression statement is compiled just like the expression it wraps.

Statements mapped fairly cleanly onto Scheme. Expressions have more details to consider.

### Expressions

Every expression can be compiled in two modes: `rvalue`("Result Value") or `lvalue`("Location Value").

Variables consist of both a location value and a result value. The location is the address in memory the variable is stored, while the result is the contents of the address.

Expressions compilable as location values allow assignment operators like `+=` to modify the value at that location. Expressions compilable as result values can be read, but not necessarily written.

Location and result values satisfy the following two laws, for variable addresses and dereferences:

{% highlight scheme %}
compile "&x" 'rvalue = compile "x" 'lvalue
compile "*x" 'lvalue = compile "x" 'rvalue
{% endhighlight %}

Ceagle tries to fit both lvalues and rvalues into a single 32-byte word. `struct` values are allocated in memory and represented as pointers, even in rvalue context. However, `struct` lvalues refer to the original struct while `struct` rvalues are copied, so they are not interchangeable.

A 32-byte `c-type-fixed` would have a 32-byte result value, but its associated address may be restricted to 3 or 4 bytes for efficiency.

Because `rvalue`s do not have identity, they must be copied to create an `lvalue`.

Here are all the C expressions:

{% highlight scheme %}
; Expressions
(struct c-const         ([ value       : CValue                 ]
                         [ signed?     : Boolean                ]))
(struct c-variable      ([ name        : Symbol                 ]))
(struct c-ternary       ([ pred        : c-expression           ]
                         [ consequent  : c-expression           ]
                         [ alternative : c-expression           ]))
(struct c-binop         ([ op          : Symbol                 ]
                         [ left        : c-expression           ]
                         [ right       : c-expression           ]))
(struct c-unop          ([ op          : Symbol                 ]
                         [ exp         : c-expression           ]))
(struct c-function-call ([ func        : c-expression           ]
                         [ args        : c-expressions          ]))
(struct c-field-access  ([ source      : c-expression           ]
                         [ name        : Symbol                 ]))
(struct c-cast          ([ type        : c-type                 ]
                         [ exp         : c-expression           ]))
(struct c-sizeof        ([ value       : (U c-type c-expression)]))
(struct c-array-access  ([ array       : c-expression           ]
                         [ index       : c-expression           ]))
(struct c-expression-sequence ([ exps  : c-expressions          ]))
(struct c-expression-array    ([ exps  : c-expressions          ]))

(define-type c-expression (U c-const ... c-expression-array))
(define-type CValue       (U Integer String Char))
{% endhighlight %}

I disagree with the C standard on string representation: Strings are `length :: data`, not `data :: 0`.

A `c-variable` in lvalue context is the memory address of the variable, and in rvalue context is a copy of the variable's value.

A `c-field-access` or `c-array-access` in lvalue context are the offset memory address from the base, and in rvalue context is the value at that memory address.

The other expressions mostly follow recursively from these.

### Types

Typechecking is simple. The compiler remembers types when it comes across them, and typechecks expressions using previously-remembered type information.

If an expression can't be typechecked, an error is thrown.

Here are all possible types:
{% highlight scheme %}
(struct c-type-fixed        ([ signed?   : Boolean              ]
                             [ bytes     : Size                 ]))
(struct c-type-struct-field ([ name      : (Option Symbol)      ]
                             [ type      : c-type               ]))
(struct c-type-struct       ([ name      : (Maybe Symbol)       ]
                             [ fs        : c-type-struct-fields ]))
(struct c-type-union        ([ name      : (Maybe Symbol)       ]
                             [ fs        : c-type-struct-fields ]))
(struct c-type-alias        ([ name      : Symbol               ]
                             [ typespace : c-typespace          ]))
(struct c-type-pointer      ([ type      : c-type               ]))
(struct c-type-void         (                                    ))
(struct c-signature         ([ ret  : c-type                    ]
                             [ args : c-sigvars                 ]))
{% endhighlight %}

The `typespace` member of `c-type-alias` is used to disambiguate `x`, `struct x`, and `union x`. These can all refer to different types, even though each has `x` for a `name`.

Because Ethereum has 256-bit words, Ceagle has a non-standard `__bits N` token which smaller standard types can be defined in terms of:

{% highlight c %}
typedef signed __bits 128 short;
{% endhighlight %}

Only integer types smaller than 32 bytes are supported. Smaller integers are stored as 256-bit words too, but the compiler sign-extends them as necessary after every arithmetic operation:

{% highlight scheme %}
(defmacro (%c-restrict-bytes x num-bytes signed?)
  (if signed?
      `(%#-sign-extend ,x ,num-bytes)
      `(%#-zero-extend ,x ,num-bytes)))
{% endhighlight %}

## Future Work

This article used Pyramid to implement the C programming language on Ethereum.

Pyramid still needs one last major technical component before it can be used by general developers. Look forward to the next article in the series:

**"How I made my language 10,000x faster ...by deferring all optimization until this article"**

Beyond that, here is a table summarizing some ideas I've had for future work:

| Feature                    | Priority | What it gets us |
| ---                        | ---      | ---         |
| Optimizer                  | Required | Feasible to deploy Pyramid contracts |
| Reference Documentation    | Required | Possible for developers to learn the language |
| Solidity ABI               | Required(for Ethereum) | Possible to interface with Metamask |
| Cardano Backend            | Required(for Cardano) | Possibly to deploy Pyramid code to Cardano |
| Type Systems               | Optional | Reduce time spent debugging simple errors |
| Multi-contract libraries   | Optional | Deploy multiple interdependent contracts |
| Contract testing libraries | Optional | Reduces risk of errors in deployed contracts |
| Library management         | Optional | Easier to fetch and use libraries |
| Formal IR semantics        | Optional | Reduces risk of errors in Pyramid compiler |
| Solidity or LLL support    | Optional | Use existing libraries in Pyramid |

`Required` means "I'm the only person in the world who's capable of doing this, and it's necessary for anyone else to start using Pyramid".

The optimizer and documentation are self-explanatory, and I'll describe the other items:

### Solidity ABI

Ethereum tools make "method calls" on the contract by encoding function name and signature along with the data. If Pyramid had a library implementing this, then tools like Metamask could call Pyramid contracts.

A direct translation of the ERC-20 interface is

{% highlight scheme %}
(exports
  (totalSupply  (                        ) -> uint)
  (balanceOf    ([ tokenOwner : address ]) -> uint)
  (allowance    ([ tokenOwner : address ]
                 [ spender    : address ]) -> uint)
  (transfer     ([ to         : address ]
                 [ tokens     : uint    ]) -> bool)
  (approve      ([ spender    : address ]
                 [ tokens     : uint    ]) -> bool)
  (transferFrom ([ from       : address ]
                 [ to         : address ]
                 [ tokens     : uint    ]) -> bool))
{% endhighlight %}

. This would:
* Generate a dispatch table to the appropriate Pyramid function using the name and signature.
* Convert the function's inputs and output using the Solidity ABI and the declared signature.

Importantly, `exports` would be a macro and not a built-in language feature. So people could experiment with alternative ABIs without needing to change the compiler.

### Cardano Backend

Pyramid's code generator currently targets the Ethereum Virtual Machine. It shouldn't be too difficult to also support Cardano's LLVM derivative, IELE. This would allow Pyramid developers to write smart contracts that work on either Cardano or Ethereum.

This refers only to deploying to these blockchains. **Atomic cross-chain trading** is a different feature necessary to communicate between Cardano and Ethereum.

### Type Systems

One problem with Ceagle is that its type system is incompatible with other type systems. If many languages have incompatible type systems, language boundaries could become awkward.

A [recent paper](http://www.ccs.neu.edu/home/stchang/popl2017/) introduced a technique for implementing type systems as macros that attach syntax properties to the code they transform. They were able to build on simpler type systems when implementing more complex type systems(all the way up through System F - similar to Haskell or ML).

This technique should make it easier to transparently call across language boundaries into languages with differing type systems.

### Multi-Contract Libraries

Many Ethereum applications are networks of contracts. A game might have a token-trading contract, a high-level interface, a contract implementing the rules of the game, and a contract that stores player's data.

Pyramid already lets you manually write this code, but it would be convenient to use an abstraction like Racket's [Units](https://docs.racket-lang.org/guide/units.html) to safely link together contracts with declared interfaces.

Safely upgrading a contract while preserving its security properties is a common problem.

### Contract testing

Testing libraries are the main tools used to check that a contract is free of errors.

| Unit-testing | Implemented |
| Randomized testing | Not Implemented |
| Model-checking | Not Implemented |
| Theorem-proving | Not Implemented |

Modeling languages like PML could be used to generate random testcases or fed into a model-checker like Z3. Some limited checking can also be done with just strongly-typed contract interfaces.

Together with a formal IR semantics, a dependently-typed language could emit end-to-end verified code.

### Library management

Pyramid will either use Racket's [`raco`](https://docs.racket-lang.org/raco/) or use the [Nix](https://nixos.org/nix/) package manager to handle libraries.

Nix promises reproducible builds, so anyone can verify that a contract was deployed correctly. It may make sense to retest contracts during the build process.

### Formal IR semantics

What is the meaning of a program?

It's common to define larger languages as reductions to simpler languages. This makes it easier to identify compiler errors.

A tool like [PLT Redex](https://redex.racket-lang.org/index.html) could be used to specify these semantics, which also makes future formal verification work easier.

### Solidity or LLL

Most Ethereum developers currently use Solidity. Since Pyramid allows people to develop languages as libraries, I could write a Solidity library for Pyramid that allows existing contract developers to use it without changing any of their existing code.

There is also a small community of "Low-Level Lisp" developers, who have expressed interest in Pyramid.

## Conclusion

In this article, I presented language-oriented programming as an alternative way to program smart contracts. If you are interested, I recommend following me on Twitter, subscribing to the mailing list, or joining the Pyramid Discord channel. Links are available on the website's sidebar.
