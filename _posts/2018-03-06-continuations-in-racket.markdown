---
layout: post
author: Michael Burge
title: "Continuations in Racket"
started_date: 2018-03-04 09:33:00 -0700
date: 2018-03-04 09:33:00 -0700
tags:
  - pyramid_scheme
---

In Scheme, __Continuations__ are procedure-like values used for advanced flow control. They are created using the built-in `call-with-current-continuation`(or `call/cc`) procedure.

## call/cc

Consider the top-level expression `(+ 1 (+ 2 (+ 3 4)))`. `+` has the reduction `(+ ?1 ?2) => ?3`, where `?1` and `?2` are integer literals, and `?3` is an integer literal equal to their sum.

Our expression has the form `(+ 1 ?)`, but `? = (+ 2 (+ 3 4))` is not syntactically an integer. In order to evaluate it, we must first reduce `?` to an integer. There is only one possible sequence of reductions:
{% highlight scheme %}
(+ 1 (+ 2 (+ 3 4)))
(+ 1 (+ 2 7))
(+ 1 9)
10
{% endhighlight %}

The continuation `(+ 1 ?)` is awaiting reduction of `?` to `9`. After, the continuation is resumed by substituting `9` for `?`.

In principle, we could consider an alternate version of the program where `?` reduced to `3` instead. Then, the top-level expression would evaluate to `4`.

Scheme makes this concept a first-class language feature with __Continuations__. They are created using `call-with-current-continuation`(or `call/cc`). Continuations can be resumed multiple times: Each time `?` is replaced with a possibly-different value.

{% highlight scheme %}
(define *c* #f)
(define (save-continuation k)
  (set! *c* k)
  0)
(+ 1 (call/cc save-continuation)) ; 1

(*c* (+ 2 (+ 3 4)))                ; 10
(*c* 3)                            ; 4
(*c* -5)                           ; -4
{% endhighlight %}

The continuation `k` is the expression `(+ 1 ?)` above. The `?` takes the place of the entire `call/cc` call. `save-continuation` returns `0`, but also remembers the continuation so that we can use a different return value later.

`(*c* (+ 2 (+ 3 4)))` evaluates the smaller `(+ 2 (+ 3 4))` and then resumes the `(+ 1 ?)` computation with its result.

Here is a more advanced example. Think about how it is evaluated, or click the spoiler:
{% highlight scheme %}
(((call/cc (λ (c) c)) (λ (x) x)) 5)
{% endhighlight %}

{:.spoiler}
{% highlight scheme %}
(((call/cc (λ (c) c)) (λ (x) x)) 5)
((c (λ (x) x)) 5) ; c represents the continuation value
(((λ (x) x) (λ (x) x)) 5)
((λ (x) x) 5)
5
{% endhighlight %}

Continuations can be used to implement C-style `break`, `continue`, and `return`; exceptions; coroutines; and more.

{% highlight scheme %}
(call/cc (λ (return)
  (let loop ([ n 0 ])
    (if (= n 7)
        (return n)
        (loop (+ n 1))))))
{% endhighlight %}

Using `(return ?)` within the body causes the entire`call/cc` to be immediately replaced with `?`.

## Delimited Continuations

Old Scheme interpreters ran each top-level expression one-by-one as in a REPL, so a continuation that reaches the top-level does not automatically go to the next expression. This is surprising because it seems to violate the [β-reduction](https://en.wikipedia.org/wiki/Lambda_calculus) rule:

{% highlight scheme %}
; Noted Example
(define *c* #f)
(define (save-continuation k) (set! *c* k) 0)

; Prints 0
(begin (call/cc save-continuation)
       (*c* 0))

; Infinite loop
((λ (x)
    (begin (call/cc save-continuation)
           (*c* 0))
 ) 0)
{% endhighlight %}

Evaluation steps such as procedure applications create new __Continuation Frames__, with the top-level being a __Prompt__. Continuations created within a prompt are completed when they reach the prompt, and do not proceed to the next expression. Racket's default behavior with non-void top-level expressions is to print them.

Because prompts cause a computation to complete, a prompt-level `call/cc` generates a continuation `?` that is equivalent to the identity function.

`begin` does not create a new continuation frame, so its expressions are in the top-level prompt. `*c*` is a prompt-level continuation, so it is equivalent to the identity function: `(*c* 0)` reduces to `0`.

The expression `(*c* 0)` prints `0` because it evaluates to `0` and then the top-level handler prints it. Neither the continuation nor the prompt handle the print behavior:

{% highlight scheme %}
(define *c* #f)

(call/cc (λ (k) (set! *c* k)))

((λ ()
   (*c* 0)
   (*c* 0)))
{% endhighlight %}

This prints only a single `0` because the continuation only evaluates to 0, and does not print the value. Here is how that last expression is evaluated:
{% highlight scheme %}
((λ () (*c* 0) (*c* 0))) ; *c* is an identity
((λ () 0 (*c* 0)))
((λ () (*c* 0)))
((λ () 0))
0
{% endhighlight %}

Because the final value is `0`, the top-level expression handler then prints it.

However, in the second block of the Noted Example, the extra `λ` does create a new continuation frame. Since it's no longer a prompt-level expression, the continuation `*c*` captures the next one too:
{% highlight scheme %}
(begin ?
       (*c* 0))
{% endhighlight %}

The β-reduction violation happens because the top-level prompt truncates continuations. This is a pretty strange concept for just the top-level to have, isn't it? 

Standard Scheme only has the one top-level prompt, but [Racket](https://docs.racket-lang.org/reference/cont.html) supports __Delimited Continuations__: [`(call-with-continuation-prompt)`](https://docs.racket-lang.org/reference/cont.html#%28def._%28%28quote._~23~25kernel%29._call-with-continuation-prompt%29%29) adds a prompt to the stack of continuation frames.

Here is the Noted Example using an explicit continuation prompt:

{% highlight scheme %}
(define *c* #f)
(define (save-continuation k) (set! *c* k) 0)

((λ (x)
   (call-with-continuation-prompt (λ () (call/cc save-continuation)))
   (*c* 0)
   ) 0)
{% endhighlight %}

Continuations saved under such a prompt don't need to store the stack past the prompt, and have a smaller scope that is easier for the optimizer to understand.

There is one last subtlety:

{% highlight scheme %}
(call-with-continuation-prompt (λ () (call/cc (λ (x) (set! *c* x)))))
(*c* 0)
{% endhighlight %}

This program outputs nothing. `call/cc` first rolls back continuation frames until it reaches a prompt with the same tag as the continuation, then it applies the frames stored in the continuation.

Here, `(*c* 0)` is rolled back to the empty continuation, and then an identity continuation is applied. The result is `void`, and so nothing is printed.

This can be corrected by either using a different prompt tag or by using `call-with-composable-continuation`, which applies the continuation without rolling back any frames.
{% highlight scheme %}
(define *c* #f)
(define *prompt-tag* (make-continuation-prompt-tag))

; call/cc with a different prompt tag
(call-with-continuation-prompt (λ () (call/cc (λ (x) (set! *c* x))))
                               *prompt-tag*)

(*c* 0)

; call-with-composable-continuation
(call-with-continuation-prompt
  (λ ()
    (call-with-composable-continuation
      (λ (x)
        (set! *c* x)))))
(*c* 0)
{% endhighlight %}

Expressions like `(+ 1 (+ 2 (+ 3 4)))` have nested continuations `(+ 1 ?)` and `(+ 1 (+ 2 ?))`. So `call-with-composable-continuation` is "composable" because it can be used in expressions easily, since it nests continuations just like ordinary expressions.
