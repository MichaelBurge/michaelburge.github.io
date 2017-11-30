---
layout: post
author: Michael Burge
title: "Write your next Ethereum Contract in Pyramid Scheme"
started_date: 2017-10-21 17:34:00 -0700
date: 2017-11-28 14:34:00 -0700
tags:
  - racket
  - scheme
  - ethereum
---

Haskell programmers often code in ivory towers with their heads in the cloud. In this multi-part article series, we'll get our feet wet diving deep below C level.

I create __Pyramid__: A dialect of the Scheme programming language that targets the Ethereum Virtual Machine(EVM). Pyramid Scheme is implemented using the appropriately-named [Racket](http://racket-lang.org/). The Pyramid compiler is currently 3512 lines of code, and includes code from [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-35.html#%_sec_5.5).

This article covers the high-level design of the Pyramid compiler: The compiler's components and Pyramid's runtime environment.

People interested in Scheme, compilers, or Ethereum will enjoy this series. At the end of it, I'll use Pyramid to take preorders for a new book: __Destabilizing Nation-states with Math: An Ethereum Hacker's Handbook__. Readers are encouraged to subscribe to the mailing list to receive new articles.

## Overview

The Pyramid compiler turns plain text into executable EVM code. Its 5 components are:

* __Parser__: Converts plain text into an AST. Racket makes this easy with its [read](https://docs.racket-lang.org/reference/Reading.html#%28def._%28%28quote._~23~25kernel%29._read%29%29) built-in.
* __Compiler__: The [SICP Scheme compiler](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-35.html#%_sec_5.5) targets an abstract machine with 5 registers, 8 operations, a stack, and 13 built-in operations.
* __Code Generator__: Converts the abstract machine code into EVM assembly.
* __Serializer__: Converts EVM assembly into deployable EVM bytecode.
* __Debugger__: A Google Sheets script that simulates disassembled EVM bytecode.

An example Pyramid program is
{% highlight scheme %}
(begin
 (define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
 (factorial 5))
{% endhighlight %}

. The Pyramid compiler outputs a long hexadecimal string, which can be deployed to a real Ethereum blockchain using the [Web3](https://github.com/ethereum/web3.js/) library:
{% highlight javascript %}
var from = "0x0703722a41e9ec7a0497043380a3cc6b16eb242d";
var code = "The output of the Pyramid compiler";
var gas = 10000000;

var g_transactionHash;

web3.eth.sendTransaction({from: from, data: code, gas: gas}, function(err, transactionHash) {
    g_transactionHash = transactionHash;
});

var receipt = web3.eth.getTransactionReceipt(g_transactionHash);

web3.eth.call({ from: from, to: receipt.contractAddress });
// Output:
// "0x0000000000000000000000000000000000000000000000000000000000000078"
// Note: factorial(5) = 120 = 0x78
{% endhighlight %}

The complete syntax of Pyramid is

{% highlight scheme %}
; Constants
5      ; Decimal integers
#xff   ; Hexadecimal integers
'hello ; Symbols - up to 32 8-bit ASCII characters

; Variables
(define x 5)
(set! x 10)
x

; Conditional Execution
(if true 10 100)
(cond ((eq? x 'red)   #xff0000)
      ((eq? x 'green) #x00ff00)
      (else           #x0000ff))

; Sequential Execution
(begin
  (define x 5)
  (define y 6))

; Procedures
(lambda (x) (* x x))        ; Anonymous
(define (square x) (* x x)) ; Named
(square 5)
{% endhighlight %}

. All other language features are provided by the __Pyramid Standard Library__, which are Procedure and Variable definitions installed into the program's environment at startup.

## Abstract Machine

After Pyramid is parsed, it is compiled to __Abstract Machine Code__.

I'll show examples of each syntax translated to abstract machine code, before giving the full language. The compiler is documented in [SICP](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-35.html#%_sec_5.5).

### Variables

Variables can be defined, modified, and accessed:
{% highlight scheme %}
(define x 1234)
(set! x 10)
x
{% endhighlight %}

The definition `(define x 1234)` compiles to

{% highlight scheme %}
(assign 'val (op 'box ((const 1234))))
(perform (op 'define-variable! ((const 'x) (reg 'val) (reg 'env))))
{% endhighlight %}

. The integer `1234` is __Boxed__, and a pointer to it stored in the `val` register. Plain integers have no type: They could be confused for pointers or symbols. Boxing attaches a type to the value. Boxed values are always pointers.

The primitive operation `define-variable!` takes three arguments: A name to define, a value to set it to, and an environment. The `env` register holds the current environment.

The next two lines are similar:

{% highlight scheme %}
; (set! x 10)
(assign 'val (op 'box ((const 10))))
(perform (op 'set-variable-value! ((const 'x) (reg 'val) (reg 'env))))

; x
(assign 'val (op 'lookup-variable-value ((const 'x) (reg 'env))))
{% endhighlight %}

The `val` register holds one argument, or the output of an operation. In general, the last Pyramid statement executed stored its result in `val`.

### Conditionals

The `if` statement evaluates to either its __True Branch__ or its __False Branch__. The expression

{% highlight scheme %}
(if true 10 100)
{% endhighlight %}

evaluates to `10` because the condition is `true` and 10 is the true branch.

`if` cannot be a standard library procedure, because procedure calls evaluate all arguments. The `if` statement has two branches, but only evaluates one.

That `if` statement compiles to
{% highlight scheme %}
; true is defined as 1
(assign 'val (op 'lookup-variable-value ((const 'true) (reg 'env))))

; branch skips code depending on the test
(test (op 'false? ((reg 'val))))
(branch (label 'false-branch2))

; branch did not skip
(label 'true-branch1)
(assign 'val (op 'box ((const 10))))
(goto (label 'after-if3))

; branch skipped
(label 'false-branch2)
(assign 'val (op 'box ((const 100))))

(label 'after-if3)
{% endhighlight %}

. The `if` stores its result in `val`, because each branch stores its result in `val`.

### Procedures

A Procedure Definition is a Variable Definition set to a Lambda. A Lambda creates a __Compiled Procedure__, which is a code pointer and an environment. Compiled Procedures are entered by jumping to the code pointer, and exited by jumping to the `continue` register.

Here is the simplest Procedure Definition:
{% highlight scheme %}
(define (id x) x)
{% endhighlight %}

Which compiles to:

{% highlight scheme %}
(assign 'val (op 'make-compiled-procedure ((label 'entry1) (reg 'env))))
(goto (label 'after-lambda2))

; Lambda Body
(label 'entry1)
(assign 'env (op 'compiled-procedure-env ((reg 'proc))))
(assign 'env (op 'extend-environment ((const '(x)) (reg 'argl) (reg 'env))))
(assign 'val (op 'lookup-variable-value ((const 'x) (reg 'env))))
(goto (reg 'continue))

(label 'after-lambda2)
(perform (op 'define-variable! ((const 'id) (reg 'val) (reg 'env))))
{% endhighlight %}

After compiling the Lambda body, it is skipped over using the first `goto` because procedures must be explicitly called to execute their body.

When Compiled Procedures are entered, their arguments are in the `argl` register. The Lambda first moves the arguments to the environment, and then runs the body.

An explicit call(or Application) looks like
{% highlight scheme %}
(id 5)
{% endhighlight %}

. It looks shorter, but the compiled code
{% highlight scheme %}
; Get code pointer and arguments
(assign 'proc (op 'lookup-variable-value ((const 'id) (reg 'env))))
(assign 'val (op 'box ((const 5))))
(assign 'argl (op 'list ((reg 'val))))

(test (op 'primitive-procedure? ((reg 'proc))))
(branch (label 'primitive-branch1))

(label 'compiled-branch2)
(assign 'continue (label 'after-call3))
(assign 'val (op 'compiled-procedure-entry ((reg 'proc))))
(goto (reg 'val))

(label 'primitive-branch1)
(assign 'val (op 'apply-primitive-procedure ((reg 'proc) (reg 'argl))))

(label 'after-call3)
{% endhighlight %}

is longer.

A Procedure can be either a Compiled Procedure created from a Lambda, or a built-in __Primitive Procedure__. They are currently similar at the EVM level, but may be optimized in the future. For example, Primitive Procedures could pass their arguments on the stack rather than allocating a linked list.

### Abstract Machine Language

These tables summarize the abstract machine that the compiler targets.

There are 8 instructions:
{% highlight scheme %}
(label name)                 ; A location to jump to
(assign register expression) ; Write the expression's result
(test expression)            ; Decide if the next branch jumps
(branch expression)          ; Conditionally jumps to a location
(goto expression)            ; Unconditionally jumps to a location
(save register)              ; Pushes a register onto the stack
(restore register)           ; Pops the stack into a register
(perform expression)         ; Executes an expression for side effects
{% endhighlight %}

4 expressions:
{% highlight scheme %}
(label name)   ; The offset of a location in the final EVM bytecode
(const value)  ; An unboxed integer, unboxed symbol, or list of consts
(reg name)     ; One of `env`, `proc`, `continue`, `argl`, `val`. These are documented in the Code Generator section.
(op name args) ; One of the Operations below together with its arguments.
{% endhighlight %}

13 primitive operations:
{% highlight scheme %}
(op 'make-compiled-procedure (label env))   ; A new Compiled Procedure
(op 'compiled-procedure-entry (proc))       ; Compiled Procedure's label
(op 'compiled-procedure-env (proc))         ; Compiled Procedure's env
(op 'define-variable! (name value env))     ; New variable
(op 'set-variable! (name value env))        ; Change variable
(op 'lookup-variable-value (name env))      ; Access variable
(op 'box (value))                           ; Create Fixnum
(op 'extend-environment (names values env)) ; Create Frame
(op 'primitive-procedure? (proc))           ; True if Primitive Procedure
(op 'apply-primitive-procedure (proc argl)) ; Call Primitive Procedure
(op 'false? (value))                        ; Boolean NOT.
(op 'list (value))                          ; Create one-element List
(op 'cons (head tail))                      ; Create Pair
{% endhighlight %}

## Code Generator

The Abstract Machine is a good intermediate target, but doesn't know anything about the EVM. The __Code Generator__ has detailed knowledge of EVM memory, instructions, stack, and storage. It has two responsibilities:

* Convert abstract machine code into almost EVM assembly
* Initialize the environment

### Abstract Assembler

The target EVM pseudo-assembly has these primitives:
{% highlight scheme %}
(label name)          ; JUMPDEST, but remembers byte offset for eth-push
(eth-asm symbol)      ; 1-byte EVM opcode
(eth-push size value) ; size = integer | 'shrink. value = integer | label
(eth-unknown value)   ; 1 literal byte
{% endhighlight %}

The __Serializer__ turns this into deployable bytecode. 

The Code Generator must implement each instruction, expression, and operation in the previous section. The abstract instructions generally map to a single assembly instruction:

| Abstract | EVM      |
| ---      | ---      |
| label    | JUMPDEST |
| assign   | MSTORE   |
| test     |          |
| branch   | JUMPI    |
| goto     | JUMP     |
| save     | MLOAD    |
| restore  | MSTORE   |
| perform  |          |

Because abstract instructions evaluate their arguments, the final EVM assembly can be longer than zero or one instructions. The complexity of the code generator is in the expressions.

Expressions leave their result on the stack. The expressions are single instructions, except for operations and list constants:

| Abstract       | EVM |
| ---            | --- |
| label          | Given to Serializer |
| const(integer) | PUSH |
| const(symbol)  | PUSH |
| const(list)    | op: `make-list` |
| reg            | MLOAD |
| op             | Custom EVM assembly |

Operations use the stack for input and output. Their arguments are themselves expressions, although operations do not take operations as arguments.

Expressions evaluate to plain 256-bit words: Integers are 256-bits, Symbols are up to 32 8-bit ASCII characters(which totals 256 bits), and everything else is a 256-bit pointer to a dynamically-allocated object.

### Memory allocation

Memory is allocated by increasing the `allocator` register. If the value in `allocator` is `n`, then increasing `allocator` by 32 reserves the memory addresses `[n, n+32)`. Memory is freed by decreasing the allocator: You cannot free an address without also freeing all address allocated after it.

Pyramid does not use a garbage collector. Complex garbage collectors can have unpredictable gas usage, and there are no long-lived EVM executions. In the future, the Pyramid compiler may optimize memory by freeing dead variables or placing variables on the stack.

Pyramid values are either __Tagged__ or __Untagged__(alternatively, __Boxed__ or __Unboxed__). Untagged values are single 256-bit words that either point to tagged values or temporarily live on the stack. Tagged values are stored in allocated memory.

There are 7 __Primitive Types__ of tagged values:

| Tag | Name                | Size | Description |
| --- | ---                 | ---  | ---         |
| 0   | Fixnum              |  1   | A 256-bit integer |
| 1   | Symbol              |  1   | Exactly 32 8-bit ASCII characters |
| 2   | Compiled Procedure  |  2   | A code pointer, and closure environment pointer |
| 3   | Primitive Procedure |  1   | A code pointer |
| 4   | Pair                |  2   | Two tagged values |
| 5   | Vector              |  2+n | A capacity `n`, a size, and `n` tagged values |
| 6   | Nil                 |  0   | Only the tag word |

A tagged value is a pointer to a `1 + Size` group of 256-bit words. The first word is the type tag from the above table; the rest depend on the type.

__Derived Types__ are combinations of primitive types. Documentation uses them to specify which primitive values are valid arguments:

| Name        | Type                          | Description |
| ---         | ---                           | --- |
| Any         |                               | Any tagged value |
| List X      | Nil or (Pair X List)          | A linked list of pairs. Used to pass function arguments. |
| Environment | List Frame                    | Holds all variable bindings |
| Frame       | Pair (List Symbol) (List Any) | A lexical scope. The __Global Frame__ has the standard library and top-level user-defined variables. |
| Procedure   | Compiled or Primitive         | Any callable value |

Untagged values have no runtime type information, but are used in these contexts:

| Type    | Description |
| ---     | --- |
| Boolean | Conditional branching |
| Code    | Offset into deployed Ethereum bytecode |
| Memory  | Unaligned offset into EVM memory |

Most memory is allocated, but there are some reserved memory locations:

| Address | Name      | Initial Value | Description |
| ---     | ---       | ---           | --- |
| 0x00    | invalid   | N/A                  | The 0 pointer is never used |
| 0x20    | env       | Standard Library     | Environment currently in scope |
| 0x40    | proc      | 1337                 | Procedure about to be called |
| 0x60    | continue  | 31337                | Return address from a Procedure |
| 0x80    | argl      | 337                  | List of arguments |
| 0xa0    | val       | N/A                  | Result of last executed statement |
| 0xc0    | nil       | 6(the Nil tag)       | One shared copy of the Nil object |
| 0xe0    | allocator | 0x100                | Next memory address to allocate |
| 0x100   |           | N/A                  | First allocated memory address |

The initial values of `proc`, `continue`, and `argl` are useful when debugging the compiler. They shouldn't be observable to user code.

### Initialization

Before the user's code runs, reserved memory locations are set to their initial values and the Pyramid Standard Library is installed.

Everything except `env` writes a constant to memory. `env` first gets a newly-allocated empty Environment, and then the Pyramid Standard Library is installed by defining several Primitive Procedure objects. Pyramid's `define` syntax could be used for this, except that users don't currently have a way to create Primitive Procedure values.

This article used this example at the beginning:
{% highlight scheme %}
(begin
 (define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))
 (factorial 5))
{% endhighlight %}

This requires 3 standard library functions:

| Name | Type                     | Description |
| ---  | ---                      | --- |
| =    | Fixnum Fixnum -> Boolean | 1 if two numbers are equal; 0 otherwise |
| *    | Fixnum Fixnum -> Fixnum  | Multiplication |
| -    | Fixnum Fixnum -> Fixnum  | Subtraction |

A Primitive Procedure is a tagged label. It is called by jumping to the label with two items on the stack: A pointer to an argument list and a return address. The code following the label should:

* Move arguments from the list to the stack
* Unbox integers
* Apply the EQ, MUL, or SUB assembly instructions
* Box the result(except for EQ)
* Jump to the return address

If Pyramid supported inline EVM assembly, then users could define their own Primitive Procedures as a special type of Lambda. This is better than adding primitives to the code generator, because users could remove functions they don't need. Shorter programs are easier to audit. Expect to see this implemented in the future.

### Serializer

Most compiled programs directly execute on their target platform. The EVM is different in that compiled programs are actually __Program Initializers__. This initializer generates a new program that handles all future transactions and messages.

The Pyramid Serializer prepends a __Loader__ to the Pyramid program that returns the Pyramid program:
{% highlight assembly %}
PUSH programSize
PUSH afterLoader
PUSH 0
CODECOPY
PUSH programSize
PUSH 0
RETURN
{% endhighlight %}

where `programSize` is the size in bytes of the compiled Pyramid bytecode, and `afterLoader` is the size in bytes of the loader(usually 14).

The Serializer also calculates the byte offset of each label, and adds it to a __Symbol Table__. Labels may be defined after they are used, so the Serializer emits a 0 as a placeholder and adds the offset to a __Relocation Table__. After the bytecode is generated, the Symbol Table is matched with the Relocation Table and the bytecode edited in-place.

The Serializer also figures out the correct size for pushes without a size: Ethereum supports different sizes of `PUSH` instructions.

## Further Work

In this article, we looked at the high-level design of the Pyramid Scheme compiler. To keep it accessible, I left out the compiler's implementation code. In my next article "Debugging a Compiler Backend with Google Sheets", I'll cover the code generator's implementation, including developing custom debugging tools.

The code is available on [Github](https://github.com/MichaelBurge/pyramid-scheme).

Future Pyramid articles may cover:
* Optimizations: Lexical addressing, liveness analysis, early unboxing
* Package Manager: Users will want to share code. EVM languages can have "internal" and "external" modules
* EVM Tooling: Implement the standard ABI to allow other contract languages and Javascript tooling to work with Pyramid
* Contract Development: Development of a contract that takes pre-orders for a book.

Future Ethereum articles may cover:
* Security: Conducting a security audit, common pitfalls, vulnerability scanners
* Contract Libraries: Organizing larger codebases, CALL vs. CALLCODE vs. DELEGATECALL
* Formal Verification: Get an absolute 100% guarantee that your contracts are free of errors
* Frontend: Build an ecommerce store using a contract as the backing datastore
