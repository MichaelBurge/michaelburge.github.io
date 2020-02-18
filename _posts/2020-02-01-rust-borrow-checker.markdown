---
layout: post
author: Michael Burge
title: "The Complete Guide to Rust's Borrow Checker"
started_date: 2020-02-01 20:00:04 -0700
date: 2020-02-01 20:00:04 -0700
tags:
  - rust
---

This article explains the Rust borrow checker and catalogues all of ways I know of to correct a borrow-related compilation error.

If you are:
* A Rust beginner: The entire article should be a great boon to you.
* An intermediate Rust developer: It may help fill in some gaps on "advanced" or less common concepts.
* Someone interested in Rust: The earlier sections may be interesting.

<!-- -->
Contents:
* This list is replaced with the Table of Contents during page generation.
{:toc}

If you see a borrow-checker error that is not explained by this article, send me a message via email or Twitter and I'll consider adding it to the list.

## Concepts

A variable can be accessed in one of five ways:

{% highlight rust %}
fn foo<T>(x:T) {
  let mut owned:       T = x         ; // Ownership
  let shared   :     & T = &owned    ; // Shared Reference
  let exclusive:  &mut T = &mut owned; // Exclusive Reference
  let read     :*const T = &owned    ; // Read-only Pointer
  let readwrite:  *mut T = &mut owned; // Read/Write Pointer
}
{% endhighlight %}

A variable **Owns** data if it is responsible for its allocation, destruction, and deallocation. When a variable of type `T` is created(through a `let` binding or pattern match), [`size_of::<T>()`](https://doc.rust-lang.org/std/mem/fn.size_of.html) bytes are allocated. All variables are [stack-allocated](https://doc.rust-lang.org/stable/reference/variables.html) in Rust, though types may contain references or pointers to heap-allocated memory.

Every variable has a `(creation_time, expiration_time)` pair associated with it - its **Scope**. A **Lexical Scope** is the scope associated with a [**Block**](https://doc.rust-lang.org/stable/reference/expressions/block-expr.html) when it is entered and exited. All scopes are bounded by a **Lexical Scope**[^1]. A variable is **Live** between creation and expiration.

The data that a variable owns may **Reference** data owned by a different variable. That data had better not have been destructed or deallocated when the reference is used, leading to rule #1:

### **Rule 1:** A live reference must refer to live data.

The following snippet is rejected because `shared`'s creation time is before `owned`'s:
{% highlight rust %}
fn foo() {
  let owned:u8;
  let shared:&u8 = &owned;
  owned = 5;
}
{% endhighlight %}

And this one is rejected because `shared`'s destruction time is after `owned`'s:
{% highlight rust %}
fn foo() {
  let shared:&u8 = { let owned = 5; &owned};
}
{% endhighlight %}

The braces `{}` define a Lexical Scope. Variables created within such a scope are also destroyed before that scope ends. So `shared` would reference the deallocated `owned` if this were allowed.

Ownership is tied to a specific variable, not the data:
{% highlight rust %}
fn use_variable<T>(t:T) { }

fn foo<T>(owned:T) {
  let shared:&T = &owned;
  let transferred:T = owned;
  use_variable(shared);
}
{% endhighlight %}

 In the following, `owned` and `transferred` could share the same storage cell since their lifetimes are sequential: They have no gaps and don't overlap. A reference to that cell would be valid at all times. But the snippet is rejected because the transfer of ownership applies to the variables, not to the minimized assignment of storage cells.

Rust differs from other languages with references by also distingishing **Shared** and **Exclusive** references. This leads to the second rule:

### **Rule 2:** A live exclusive reference is the only live reference to its storage cell.

Shared references only allow reads, while exclusive references allow reads and writes. This isn't required for correctness: There are many ways to safely modify a storage cell through a shared references. These are covered in the section on **Interior Mutability**.

`&T` and `&mut T` are sometimes called **Immutable Reference** and **Mutable Reference** respectively, reflecting the default read/write access. I believe Shared and Exclusive are more accurate, and the `&mut T` syntax is a historical anomaly that is difficult to change now.

The borrow checker rules are defined by runtime behavior, but have no runtime-cost because they are checked at compile-time by the **Static Borrow-Checker**. The types providing interior mutability all have a slight runtime cost because they skip this compile-time validation.

### Lifetime variables

Lifetime variables represent a variable's scope. During typechecking, the compiler(and many [formalizations](https://arxiv.org/abs/1903.00982)) resolves each one into a set of variables and how they were borrowed(exclusive or shared); these limit what references are safe to acquire.

Every reference carries an associated lifetime, but the [lifetime-elision rules](https://doc.rust-lang.org/nomicon/lifetime-elision.html) mean that they are implicit rather than explicit. When you call a function or store references in a type, the lifetime variables allow the reference's dependencies to follow it into a new context. Because lifetime variables reduce to a borrow-set, the rules required of them are exactly the same as the borrow checker's rules.

{% highlight rust %}
struct X;
impl X {
    fn foo<'a>(&'a self){}
}

fn use_variable<T>(t:T) {}

fn bar() {
    let x1 = X;
    {
        let x2 = X;
        let y = &x1;
        x2.foo();
        x1.foo();
        use_variable(y);
    }
}
{% endhighlight %}

Here, `x2.foo()` expands `'a` to the borrow-set associated with `x2`, a specific variable. That borrow-set contains only `set{ (x2, SHARED) }`.

`x1.foo()` resolves `'a` to the borrow-set `set{ (y, SHARED), (x1, SHARED) }`, because there is an existing borrow `y` of `x1`. Because it's okay to have multiple shared references, this borrow-set is redundant.

There's more subtlety when lifetimes are in the return position:
{% highlight rust %}
struct X{y:u8}
impl X {
  fn foo<'a>(&'a self) -> &'a u8 { &self.y }
}
u8 plus<'a, 'b>(a:&'a u8, b:&'b u8) { *a + *b }
fn bar() {
  let x:X = X{x:5};
  let y:&u8 = x.foo();
  let z = plus(y, &x.x);
}
{% endhighlight %}

Here, the call `x.foo() creates a temporary reference to `x`. `y` has an elided lifetime matching that temporary. Even though `y is derived from a temporary value, `*y` is just a pointer dereference and doesn't need that temporary to be alive. So it is safe to destroy it.

Even though the temporary is destroyed, `y`'s existence prevents any exclusive references to `x`. `foo`'s signature implies `y` has the same borrow-set as a shared reference to `x`, so the borrow-checker treats it as if there's a live shared reference. And it's not allowed to have both a shared and exclusive reference to `x`.

You can use `+` to union lifetimes in input position, and `+` to intersect lifetimes in output position:
{% highlight rust %}
struct X;
// ERROR: Intersection of 2 arbitrary lifetimes is empty, so reference is impossible to construct.
//fn foo<'a, 'b, 'c: 'a + 'b>(x:&'a X, y:&'b X) -> &'c X { &x }
// PASS: Union of 2 arbitrary lifetimes allows both parts to be constructed.
fn bar<'a, 'b, 'c: 'a + 'b>(x:&'c X) -> (&'a X, &'b X) { (&x, &x) }

// PASS: 'c is impossible to construct, but 'static is a subtype of every lifetime.
// More generally: "'a: 'b" means both "'a outlives 'b" and "'a is a subtype of 'b".
static GLOBAL:X = X;
fn baz<'a, 'b, 'c: 'a + 'b>(x:&'a X, y:&'b X) -> &'c X { &GLOBAL }
{% endhighlight %}

Types and method blocks can also carry lifetimes:
{% highlight rust %}
struct Foo<'a>(&'a u8);
impl<'a> Foo<'a> {
  fn new(x:&u8) -> Foo { Foo(x) }
}
{% endhighlight %}

`T: 'a` means "on every instance of `T`, every lifetime dependency outlives `'a`". In particular, if `T` has no references or lifetime dependencies, `T: 'static` vacuously: Types that are plain data like integers or strings are `'static`.

There are also [Unbounded Lifetimes](https://doc.rust-lang.org/nomicon/unbounded-lifetimes.html) created from 1. Unsafe code or 2. Lifetime parameters that aren't used.

### Early and Late-bound Lifetimes

On a function, lifetimes that appear 1. In an argument's type and 2. Not in a where clause are **Late-bound**. All other lifetimes are **Early-bound**.

Late-bound lifetime variables have a borrow-set which differs at every call of a function, while early-bound lifetimes are the same on every call. Early-bound lifetimes can differ between calls only if the function itself is newly-instantiated:  `foo::<T>()` is a different function from `foo::<U>()`, but `foo::<'a>()` is the same function as `foo::<'b>()` if `foo`'s first lifetime parameter is late-bound.

Because lifetimes are a tool of the borrow checker, and Rust's borrow checker runs at compile-time, lifetimes are erased from runtime types. The [`TypeId`](https://doc.rust-lang.org/nightly/core/any/struct.TypeId.html) is the same regardless of the lifetime, so it and the [`Any`](https://doc.rust-lang.org/std/any/trait.Any.html) trait currently require the type to be `'static`. 

Early-bound lifetimes are generic parameters, but late-bound lifetimes are desugared into a special syntax `for <'a>`:

{% highlight rust %}
// Original function:
// foo<'a, 'b>(x:&'a u8) -> &'b u8 { unimplemented!() }

// One way of desugaring, if 'b is 'static.
// 'b is early-bound, 'a is late-bound
type FooF<'b> = for <'a> fn(x:&'a u8) -> &'b u8;
const foo:FooF = |x| unimplemented!();
{% endhighlight %}

You can force lifetimes to be early-bound by adding it to the `where` clause, and to be late-bound by using `PhantomData` or an explicit `for`(as above):
{% highlight rust %}
// 'a is late-bound. 'b is early-bound
fn foo<'a, 'b>(x:&'a u8) -> &'b u8 { unimplemented!() }
// 'a is early-bound. 'b is late-bound
fn bar<'a, 'b>(x:&'a u8, _:PhantomData<'b>) -> &'b u8 where 'static: 'a { unimplemented!() }
{% endhighlight %}

Finally, [here](https://rust-lang.github.io/rfcs/0387-higher-ranked-trait-bounds.html) is a discriminator between early and late-bound lifetimes:
{% highlight rust %}
pub struct X;
// ERROR - x does not live long enough
pub fn with_early<'a, T>(callback: T) where T: FnOnce(&'a X) {
    let x = X;
    callback(&x)
}

// PASS - Lifetime is bound to the stack frame of with_late
pub fn with_late<T>(callback: T) where T: for <'a> FnOnce(&'a X) {
    let x = X;
    callback(&x);
}
{% endhighlight %}

`with_early` is unable to bind `'a` to "the stack frame of `with_early`", because `'a` must be the same for every call but each call to `with_early` has a different stack frame(and hence a different lifetime).

`with_late` delays binding `'a` until `callback(&x)`, when the `x` has been created and hence the stack frame has already been set up.

The [Rust Quiz](https://dtolnay.github.io/rust-quiz/5) has another discriminator using traits. Type inference produces early-bound lifetimes, while user-written types infer late-bound lifetimes:
{% highlight rust %}
fn foo(_:&()){}
fn bar() {
  // x1 uses type inference, so foo will have a specific lifetime assigned to it
  let x1: fn(_) = foo;
  // x2 has a user-written &, so foo will have a late-bound lifetime
  let x2: fn(&_) = foo;
}
{% endhighlight %}

### `Copy` Trait

This example failed to compile earlier, but succeeds with the `Copy` constraint:
{% highlight rust %}
fn use_variable<T>(t:T) { }

fn foo<T: Copy>(owned:T) {
  let shared:&T = &owned;
  let transferred:T = owned;
  use_variable(shared);
}
{% endhighlight %}

Types that are `Copy` can be fully duplicated using a simple bitwise copy(i.e. C's `memcpy()`). This includes simple data: integers, pointers, and shared references. Types that are not `Copy` include exclusive references and data owned through a pointer.

{% highlight rust %}
#[derive(Copy,Clone)]
struct Foo<T>(*mut T);

fn good<T>(x:&mut Box<T>) -> Foo<T> {
    Foo(&mut **x as *mut T)
}

fn bad<T>(x:Box<T>) -> Foo<T> {
    Foo(Box::into_raw(x))
}
{% endhighlight %}

`good` constructs a `Foo` that points to a `u8` that someone else owns. So the `u8` is not owned through a pointer. It's more like a shared reference, so it's safe to copy.

`bad` takes ownership of the `Box<u8>`, storing an unmanaged pointer to its contents. So the `u8` is owned through a pointer. Copying `bad()` would not create a separate `u8`, so it's not correct to copy.

### Standard Library

It's important to understand each of these library types:
* [`RwLock`](https://doc.rust-lang.org/std/sync/struct.RwLock.html): It is safe to elevate a shared reference to an exclusive reference by blocking until all active borrows are released.
* [`RefCell`](https://doc.rust-lang.org/std/cell/struct.RefCell.html): checks for active borrows (the Ref and RefMut) at runtime and panics. This is a type of dynamically-checked borrow checker. It is only valid in single-threaded context because the reference count is not protected, so a Ref dropped at the same time in two different threads could only update the counter once. 
* [`Mutex`](https://doc.rust-lang.org/std/sync/struct.Mutex.html): The same as RwLock, but shared references are also exclusive. This prevents a constant stream of readers from preventing any writers from ever obtaining a lock. It treats readers and writers equally, so guarantees progress for all interested values. 
* [`Cell`](https://doc.rust-lang.org/std/cell/struct.Cell.html): A cell never allows a reference to it's interior. Updates happen with `set()` and `get()` which always copy the entire value. This only works in a single thread so that two `set()`s don't happen at the same time.
* [`UnsafeCell`](https://doc.rust-lang.org/nightly/core/cell/struct.UnsafeCell.html): Converts a shared reference into an exclusive pointer. It is unsafe to use the pointer, because you must verify outside the compiler that there are no other references or readers/writers. (It is however ok to have two mut * to the same data. But not references.)
* [`Rc`](https://doc.rust-lang.org/std/rc/struct.Rc.html) implements reference-counted ownership. So two owners can each obtain a shared reference to their data, and `Drop` only decreases the reference count (except the last one will also `Drop` the value). It is common to combine this with interior mutability like `RwLock` or `RefCell`. Otherwise neither owner could get exclusive access to the data.
* [`CoW`](https://doc.rust-lang.org/std/borrow/enum.Cow.html) is a "Copy-on-Write" type. Reads use a shared reference, exclusive writes mutate a single copy, and shared writes act on their own local copies.

## Why have a borrow checker?

This section is a catalogue of reasons a borrow checker might be useful.

### Data Races

CS courses used to introduce Object-Oriented Programming(OOP) by modeling real-world services like bank accounts using objects:
{% highlight rust %}
type UsdCents = u64;
struct BankAccount {
  account_number: String,
  routing_number: String,
  balance: UsdCents, // NOTE: See ticket #4321 re:Mr. Gates
  // ...
}
impl BankAccount {
  pub fn credit(&mut self, cents:UsdCents) { unimplemented!() }
  pub fn debit(&mut self, cents:UsdCents) { unimplemented!() }
  // ...
}
{% endhighlight %}

This example is fundamentally flawed in a number of ways[^3]. Among the problems is that it is vulnerable to **Data Races**. In practice, banks want something that looks more like this:
{% highlight rust %}
type UsdCents = u64;
type AccountId = u64;
struct BankAccount;
struct Transaction {
  from: AccountId,
  to: AccountId,
  amount: UsdCents,
  created_at: std::time::SystemTime,
}
enum Error {
  AccountTooPoor,
  AccountFrozenByIRS,
  // ...
}
type Result<T> = std::result::Result<T, Error>;
struct TransactionLog(Vec<Transaction>);
impl TransactionLog {
  pub fn transfer(&mut self, from:&mut BankAccount, to:&BankAccount, amount:UsdCents) -> Result<TransactionId> { unimplemented!() }
}
{% endhighlight %}

That word `Transaction` has two meanings here: 1. In banking, it represents an atomic unit of money transfer. And 2. In Computer Science, a database transaction is used to express atomic updates to centralized data. Partial updates are rolled back if an error occurs, or all changes committed at once if everything is successful.

Rust's Shared and Exclusive references allow you to use database terminology to guarantee correctness in multithreaded or even singlethreaded programs.

The Rust-specific benefit is the `from:&mut BankAccount` and `to:&BankAccount` above. Despite the `&mut`, these do not necessarily modify the associated account. Instead, they take an **Exclusive Lock** on the bank account so that no other Rust code can access them while `transfer` is active.

Transfers should check "Does `from` have enough money to allow this transaction?" But if someone starts two $100 transfers from a $100 account very quickly, both might succeed since the other hasn't finished yet. The sender could have -$100 while the receiver has $200. And then the criminal takes the money via ATM and you never see them again.

I'm making the possibly-dubious choice of making `to:&BankAccount` a shared reference. This allows many people to deposit money into the same account. An actual bank may require exclusive access for other reasons, but on this specific business rule it may be safe to use a shared reference.

For multithreaded programs, the `from` might be borrowed from e.g. a `Mutex`. This means other threads wait until this account has been unlocked before attempting to deposit money.

For single-threaded programs, this protects the programmer from accidental programming mistakes like recursively calling `deposit` using the same account. Rust requires such a recursive call to transfer **Ownership** of the exclusive lock, which effectively sequences the two transactions.

The borrow checker allows code to assert "There is no other Rust code in the same program that can access this data. I am safe to modify it and call whatever helper functions I please, and it is guaranteed that nothing will break".

You might argue "The first example had a `&mut self`. Isn't that the same?" It does use the same feature, but the lock is over the incorrect scope: You can imagine there is some code that calls `credit` on one account and `debit` on the other. Single-entry functions like these are intrinsically dangerous - at best, they should be private methods. The second version uses `&mut` purely for the locking effect, not to modify the account.

Other example might be a game, which creates many abstract resources(A vertex buffer, a connection to a friends list, a filehandle, an asset database, etc.) Exclusive locks statically-guarantee that independent subsystems won't touch each other's data at the same time. I've heard this has historically been a huge problem in game engines that have order-dependent initialization stages.

### Performance

There is a Wikipedia article that shows how to save 1 assembly instruction on a hypothetical RISC machine:

[Wikipedia - Restrict](https://en.wikipedia.org/wiki/Restrict)

When a pointer is tagged with `restrict`, the compiler is allowed to assume that it is the only live pointer to its address. That's very similar to Rusts exclusive references.

Since very little C code actually uses `restrict`, a C compiler must work very hard to recover this information. Rust code is littered with `&mut` and only a small fraction uses `*mut`.

On small toy examples, this tends to not be an issue: There's only 1 file, so the C compiler has the entire source code available for analysis and can figure out which pointers can alias.

But on larger codebases, a Rust compiler should allow more aliasing-sensitive optimizations than the equivalent C code and compiler.

### Style

Since long-lived locks are the source of most borrow-checker errors, a static borrow-checker encourages people to limit the scope of any state-modifying code.

You can implement a runtime borrow checker in e.g. C++. But you might depend on 3rd-party libraries, and your own code may not use it consistently. So if you want to write programs as if there were a borrow checker, it saves time to have a static borrow checker built into the language.

### Program Traces

A **Trace** is a log of all runtime statements made by a program. A "CPU Trace" logs each CPU instruction as run in sequential order. A modern CPU may have multiple cores, hyperthreads, speculative execution, etc., but usually there must exist a sequential list of (Execution Context #, Instruction) pairs that replicates the observed behavior. 

Every CPU trace then satisfies the borrow checker: The "exclusive lock" on a hypothetical `fn clock(&mut Cpu, &ExternalIO)` is 1. Limited in time - it is only held for a single CPU clock cycle and 2. Limited in scope - it only locks a fixed-size bit pattern(say, 64 bits).

Since the program trace satisfies the borrow checker, it is natural that its source representation should also satisfy the borrow checker.



## Common Scenarios

This section gives a list of common compilation errors and techniques to resolve them.

### Exclusive access to part, shared access to rest

In the below sample, the intention is to lock one element of the array for writing, while reading the other elements:
{% highlight rust %}
fn update_entry<T>(x:&mut T, xs:&Vec<T>) { unimplemented!() }
fn foo<T>(data:Vec<T>) {
  update_entry(&mut data[0], &data);
}
{% endhighlight %}

This is rejected because the exclusive reference `x` is derived from an exclusive reference to `data`. So the entire vector is locked, even if `update_entry` never attempts to read `x`.

#### Solution 1: Use a structure or tuple

Field accesses(both to named and positional fields) do not lock the entire structure. So if there are a fixed number of fields or one field is special, you can convert the array:

* Split `x` into its own field
{% highlight rust %}
struct Foo<T> { x:T, xs:Vec<T> }
fn update_entry(x:&mut T, xs:&Vec<T>) { unimplemented!() }
fn foo<T>(data:Foo<T>) {
  update_entry(&mut data.x, &data.xs);
}
{% endhighlight %}

* Convert the vector into a tuple
{% highlight rust %}
struct Foo<T>(x1:T, x2:T, x3:T);
fn update_entry(x:&mut T, xs:&Vec<T>) { unimplemented!() }
fn foo<T>(data:Foo<T>) {
  update_entry(&mut data.0, &vec!(data.1, data,2));
}
{% endhighlight %}

This solution fails if `Foo` is passed through a function, because Rust does not track the exact set of borrows across function boundaries.

{% highlight rust %}
struct Foo<T>{x1:T,x2:T};
fn get_x1_mut<T>(foo:&mut Foo<T>) -> &mut T { &mut foo.x1 }
fn get_x2<T>(foo:&Foo<T>) -> &T { & foo.x2 }
fn update_entry(x1:&mut T, x2:&T) { unimplemented!() }
fn foo<T>(data:&mut Foo<T>) {
  update_entry(get_x1_mut(data), get_x2(data));
}
{% endhighlight %}

There is an RFC to allow annotating functions with the groups of fields that they lock, but it is not something you can expect to use anytime soon.

Until then, using getters will prevent you from accessing independent fields unless you also use one of the other solutions.

#### Solution 2: Interior Mutability

If you hold an exclusive reference in a high-level function, this tends to prevent anything else in the program from helping that function do its work.

Another way to approach this is to hold shared references in high-level functions, and convert those to exclusive references only while actually doing the writing.

{% highlight rust %}
type Foo<T> = Vec<RefCell<T>>;
fn update_entry(x:&mut T, xs:&Foo<T>) { unimplemented!() }
fn foo<T>(data:Vec<RefCell<T>>) {
     update_entry(data[0].borrow_mut(), &data);
}
{% endhighlight %}

If `update_entry` ever attempts to write to `x` through `xs` rather than the exclusive reference it was given, `RefCell` will throw a runtime panic.

This also works with Solution 1:
{% highlight rust %}
struct Foo<T>{x:RefCell<T>, xs:Vec<RefCell<T>>};
fn update_entry(x:&mut T, xs:&Foo<T>) { unimplemented!() }
fn foo<T>(data:&Foo<T>) {
  update_entry(data.x.borrow_mut(), data);
}
{% endhighlight %}

#### Solution 3: Split the structure

Some datatypes have a natural way to define "everything except one element". For an array slice, splitting the first element means creating a new slice that starts at the 2nd element. Vectors are built on slices, so they also have a meaningful way to distinguish the first element.

{% highlight rust %}
type Foo<T> = [T];
fn update_entry(x:&mut T, xs:&Foo<T>) { unimplemented!() }
fn foo<T>(data:&mut Foo<T>) {
  if let Some(first, rest) = data.split_at_mut(0) {
    update_entry(first, rest);
  }
}
{% endhighlight %}

Since `update_entry` is given a container `xs` with the `x` removed, it is impossible for it to access `x` through `xs`.

You could imagine extending this to hash tables, binary trees, or many other data structures.

If your datatype doesn't naturally support this split operation, see Solution 4.

#### Solution 4: Swap a placeholder value in

If you can obtain any value at all of the correct type, you can temporarily replace `x` with it while you perform your write. And then replace it back at the end.

{% highlight rust %}
struct Foo<T>(x1:T, x2:T);
fn update_entry<T>(x:&mut T, xs:&Foo<T>) { unimplemented!() }
fn foo<T: Default>(data:&mut Foo<T>) {
  let mut x = Default::default();
  std::mem::swap(&mut x, &mut data.x1);
  update_entry(&mut x, data);
  std::mem::swap(&mut x, &mut data.x2);
}
{% endhighlight %}

Since `update_entry` takes a shared reference to `xs`, it's unlikely to attempt to write to `x`. So this should be safe from write-collisions.

However, it introduces the risk that `update_entry` reads an incorrect value instead of the `x` it wanted. This silent error might not generate a runtime error(if `T` were a numerical type that defaults to `0`).

A type like `Option<T>` has a good placeholder value to prevent this: `None`. If `update_entry` attempts to read `x` and gets back `None`, it might throw a runtime error alerting you to a logic error.

### Circular References

#### Solution 5: Use an Arena

There is a 3rd-party library providing the `Arena` and `TypedArena` types. These are containers that deallocate all objects "at the same time", allowing them to maintain references to each other.

{% highlight rust %}
INCLUDE SAMPLE CODE SOLVING CPUPPUINTERCONNECT USING AN ARENA
{% endhighlight %}

### Two Logical Owners

#### Solution 6: Use an `Rc`

Consider a web interface to a controller that manages many other servers. You can resize the cluster, distribute tasks to the cluster, and kill long-running or erroneous tasks through the interface.

The web interface needs access to the server list. It can't be a reference, because it's possible that the web interface is the only accessor to the server data. A reference isn't allowed to `Drop` the data, but if the website is the only accessor it should drop that data when it is shut down.

There is also a thread that runs in the background. Even if nobody visits the website, idle servers must have tasks allocated to them. So the background work cannot be owned by the website. And the website doesn't necessarily need access to the background task either. Both the background task and the website have **Shared Ownership** over the server data, because neither needs to be aware of the other's existence.

A reference-counted type like `Rc` or `Arc` solve this problem. They implement `Clone` by increasing a reference count, and `Drop` by decreasing it. So both the web interface and the background task can be given a `clone` of the data.

However, it's not possible for either service to mutate through a reference-counted type: These types only provide a shared reference, because exclusive access requires there to be exactly 1 owner. To solve this, you can store an interior-mutability[Include link to Interior Mutability section] type to elevate the shared reference to an exclusive one. Since `Arc` is usually used in multi-threaded contexts(like a website and background task on separate threads), usually `Mutex` or `RwLock` are used.

`Arc` exists in other languages. C++ has `shared_ptr` for example. However, Rust is unique in safely providing `Rc`, which only works in single-threaded context. The traits `Send` and `Sync` mark which types can or cannot be safely sent across threads, and `Rc` does not implement these. `Rc` is faster than `Arc` and you'll never accidentally misuse it without using `unsafe`. But don't be afraid to use `Arc` if it solves a problem.

#### Solution 7: Clone the value

Sometimes there may be two owners, but one doesn't mind working off of an older copy. This is a common pattern:
* Query an owner for some data
* Use that data to decide how to modify the owner

But the data may include references, which blocks getting exclusive access to the original owner:

{% highlight rust %}
struct Foo { x:u8, y:u8 }
impl Foo {
  fn get_x(&self) -> &u8 { &self.x }
  fn exclusive(&mut self, x:&u8) { unimplemented!() }
}

fn bar() {
  let mut foo = Foo{x:0,y:0};
  // ERROR: Mutable and immutable reference to foo
  // let x = foo.get_x();
  // PASS: No immutable reference to foo, just to a clone.
  let x = &foo.get_x().clone();
  match x {
    0 => foo.exclusive(x),
    _ => { },
  }
}
{% endhighlight %}

It's often fine to make decisions based on data as-of 2 statements ago(the clone), instead of up-to-date data(the reference)

### Two writers

Sometimes a program needs multiple-write access to the same data:

{% highlight rust %}
fn foo<T>(x1:&mut T, x2:&mut T) { unimplemented!() }
fn bar<T>(x:T) {
  foo(&mut x, &mut x);
}
{% endhighlight %}

In this example, many implementations of `foo` could simply use only one of the references. However, sometimes this arises when `x2` is a larger container type that many helper functions share:

{% highlight rust %}
struct Foo<T>(T, u8, String);
impl<T> Foo<T> {
  fn helper1(&mut self) { unimplemented!() }
  fn helper2(&mut self) { unimplemented!() }
  fn helper3(&mut self) { unimplemented!() }
  fn foo(&mut self, x:&mut T) { unimplemented!() }
}
fn bar<T>(foo:Foo<T>) {
  foo.foo(&mut foo.x);
}
{% endhighlight %}

`foo` would like to use these helper functions in its implementation, but they require a lock on the entire `Foo` object making it impossible to also lock the `T` inside.

Further, these helper functions may actually write the `T` field, as does `foo`. So unlike the previous case[Insert reference to "only needs exclusive access to one part"], this design seems to truly need multiple writers.

#### Solution 8: Use pointers

The pointer type `*mut T` carries no assumption that it has exclusive access. You can wrap any access to `T` in an `unsafe` block and it will work:

{% highlight rust %}
fn use_variable<T>(_:&mut T) { unimplemented!() }
fn use_variables<T>(x1:*mut T, x2:*mut T){
  unsafe {
    use_variable(&mut *x1);
    use_variable(&mut *x2);
  }
}
// ERROR - can't borrow t as mutable twice
fn bad_foo<T>(mut t:T) {
  let x1 = &mut t;
  let x2 = &mut t;
  use_variables(x1,x2);
}
// PASS - Pointers don't lock t
fn good_foo<T>(mut t:T) {
  let x1:*mut T = &mut t;
  let x2:*mut T = &mut t;
  use_variables(x1,x2);
}
{% endhighlight %}

Note that it would be undefined behavior to use the pointer to create two `&mut T` that are both live at the same time. In `use_variables`, the `&mut T`s each expire at the end of the `use_variable` statement, so at most one is alive.

#### Anchor Lifetimes

Pointers not only lose the exclusivity constraint, but also the lifetime of the data they point to. You can use an **Anchor Lifetime** to match a pointer to an existing variable:

{% highlight rust %}
enum Either<'a, 'b, T> {
  Left(&'a T),
  Right(&'b T)
  }

fn foo<'a, 'b, T>(cond:bool, ptr:*const T, _l1:&'a T, _l2:&'b T) -> Either<'a, 'b, T> {
  if cond { Either::Left(unsafe { &*ptr }) }
  else { Either::Right(unsafe { &*ptr }) }
}
{% endhighlight %}

`_l1` and `_l2` are anchor variables: They may point to different data than ptr`. But the type checker is made to match their lifetimes `'a` and `'b` with the output of the function: `Either<'a, 'b, T>`. So, even though there may be 3 different storage locations, the programmer is made to ensure that all locations are deallocated at the same time.

You don't have to match a `*const T` or `*mut T` with a `&T` or `&mut T`. It's probably a mistake if you use `&mut` as an anchor lifetime, unless you're using it like a compile-time mutex: Since there can be many `*mut T` and only one `*&mut T`, it's tempting to use another `&mut` to avoid accidentally creating two. But if the pointer is derived in any way from the anchor, this would generate undefined behavior. So in practice, nobody uses `&mut` as an anchor.

#### Solution 9: Narrow the writes

As discussed in section [TODO: Link to "Program Traces"](), a program trace always satisfies the borrow checker.

So if you write a program which claims to require multiple exclusive references to data, it is interesting to consider that the execution of this program will not. So, it is worth considering how this "extraneous" exclusive reference is required.

The root cause is this: Locks are only problematic if they are held for a long time, or over a large amount of data. If you believe you need multiple writers, it is because you are holding locks for too long.

Consider passing shared references to data, and elevating those shared references to exclusive references only when you actually perform the write. This sounds similar to Solution #2, but with one difference: Earlier, we started by making high-level functions shared. Here, we start by making everything except low-level-functions shared. The program design is slightly different, because the interior mutability types are held lower in the program stack.

The trade-off is this: It is convenient for development to move interior-mutability lower-level, because it causes fewer compile-time errors and gives you more freedom to design your program. It is convenient for runtime debugging for interior-mutability to be higher-level, because there will be fewer locks in total and fewer interactions between locks.

Examples of problematic interactions include: Panics(if using `RefCell`), deadlocks(if using `RwLock` or `Mutex`), excessive copying(if using `Cell`). You should never trigger undefined behavior using these types, but that doesn't mean your program will work. Since `& and `&mut` are checked at compile-time, you won't have any problems at runtime using them.

If the writers are on separate threads, you'll want either a `Mutex` or `RwLock` somewhere in your program. They will take an explicit lock, preventing the other thread from executing.

#### Solution 10: Delay the writes

In a game, you might have 10 different objects that want to create a projectile. They all want to write to the global projectile store as a result. You don't want them to take an exclusive lock `&mut`, because this forces them all to execute sequentially rather than in parallel.

For something like "Create an object", you often don't need to immediately do that work. Create an event, send it through an `EventChannel`[Link to an event channel library], and create the object when you later get exclusive access. This does require a write and hence a lock of some sort, but it is very small.

One disadvantage is that the compiler's optimizer won't be able to eliminate the `EventChannel`, turning it into an "optimization fence" that prevents inlining. If your events are very small("Increase a player's gold by 20"), then you're replacing a single `add` instruction with a 3-stage process. 

### Variables referencing another

The below sample gives an error:
{% highlight rust %}
fn foo<T>(x:T) {
  let mut f:Box<dyn FnMut(T)>;
  let mut flag = false;
  f = Box::new(|t| flag = true);
  f(x);
  assert!(flag);
}
{% endhighlight %}

Variables are dropped in reverse order. `flag` here is dropped before `f`, but `f` still maintains a reference to it.

This doesn't really matter, because dropping `f` won't invoke any destructor that uses `flag`. But the mere existence of the reference means the compiler won't accept it.

There are 5 ways to accomplish this:

#### Solution 11: Use `drop`

{% highlight rust %}
fn foo<T>(x:T) {
  let mut f:Box<dyn FnMut(T)>;
  let mut flag = false;
  f = Box::new(|t| flag = true);
  f(x);
  drop(f);
  assert!(flag);
}
{% endhighlight %}

#### Solution 12: Reverse variable declarations

Reversing the order of `flag` and `f` causes `flag` to be dropped after `f`:

{% highlight rust %}
fn foo<T>(x:T) {
  let flag = true;
  let mut f:Box<dyn FnMut(T) -> bool> = Box::new(|t| flag);
  f(x);
  assert!(flag);
}

Note that I switched the function to a shared reference here.

{% endhighlight %}

#### Solution 13: Using `FnOnce`

This trait's `call` causes ownership to be consumed, dropping the value and releasing the lock.

{% highlight rust %}
fn foo<T>(x:T) {
  let mut f:Box<dyn FnOnce(T)>;
  let mut flag = false;
  f = Box::new(|t| flag = true);
  f(x);
  assert!(flag);
}
{% endhighlight %}

However, this and other ownership-based techniques only work if `f` is to be called exactly once. You can't call it twice, even though there is no scope issue.

{% highlight rust %}
fn foo<T>(x:T) {
  let mut f:Box<dyn FnOnce(T)>;
  let mut flag = false;
  f = Box::new(|t| flag = true);
  f(x);
  f(x); // ERROR
  assert!(flag);
}
{% endhighlight %}

#### Solution 14: Using a scope to force destruction

Here, the explicit scope forces `f` to be destroyed right after it's used.
{% highlight rust %}
fn foo<T>(x:T) {
  let mut flag = false;  
  {
    let mut f:Box<dyn FnMut(T)> = Box::new(|t| flag = true);
    f(x);
  }
  assert!(flag);
}
{% endhighlight %}

This is the most general technique. Unlike ownership-based techniques(`drop` and `FnOnce`), it works on 1. Functions that need to be called multiple times(but are never called again) and 2. Allows you to force-drop variables that implement `Copy`:

{% highlight rust %}
fn use_variable<T>(_:T) { unimplemented!() }

fn foo<T>(x:T) {
  let a:&T = &x;
  drop(a);
  use_variable(a); // ALLOWED
  use_variable(x);
}
{% endhighlight %}

Here, the intention is to release `a`. But because `&T` is `Copy`, the `drop` only releases a copy of `a` and not `a` itself. A scope allows you to release `a`:
{% highlight rust %}
fn use_variable<T>(_:T) { unimplemented!() }

fn foo<T>(x:T) {
  {
    let a:&T = &x;
    drop(a);
  }
  use_variable(a); // ERROR
  use_variable(x);
}
{% endhighlight %}

If you're defining a single function that has distinct borrow sections(like slight variations on the same unit test), put each section in its own scope to ensure borrows don't leak between each section.

#### Solution 15: Reorder arguments.

There is a deterministic destruction order within a statement. The `;` is the **Statement Separator** but also marks **Sequence Points**. Between each sequence point, the following occurs:
* Storage for scoped variables is reserved
* Storage for demanded expressions is reserved
* Temporaries are evaluated from left to right, producing **Values**. For expressions which contain other expressions, temporaries are created depth-first.
* Values are copied into reserved storage
* The statement executes
* Destructors for all unowned temporaries are run, in reverse order that they executed.
* Storage for temporaries is deallocated.
* Destructors for scoped variables which expired are run.
* Storage for scoped variables is deallocated.

In the below example,
{% highlight rust %}
fn foo() {
  let x = bar(5+5);
}

fn bar(x:u8) -> u8 { x * 2 }
{% endhighlight %}

* `x` is a scoped `u8` variable which occupies `1` byte of storage(Storage cell #1).
* `5`, and `bar(_)` are `u8`s which reserve `2` bytes total(Storage cells #2,#3)
* `=` demands the value of `bar(5+5)`, which demands the value of `5+5`. This produces `10`(in cell #3) and then `20`(in cell #2).
* The statement `let x = 20` transfers ownership of storage cell `3`(containing `20`) to storage cell `1`(currently uninitialized). This move is semantically equivalent to `memcpy()`.
* Storage cells #2 and #3 have destructors run. The type `u8` has a "do nothing" destructor.
* Storage cells #2 and #3 are deallocated. On x64 systems, this might be done by adding to a stack pointer.

The below experiment demonstrates that the borrow checker considers the order that temporaries are dropped:
{% highlight rust %}
fn foo<T>(mut t:T) {
  let x = bar({ let u:&mut T = &mut t; &t}, &t); // ALLOWED
  // let y = bar(&t, { let u:&mut T = &mut t; &t}); // REJECTED
}

fn bar<'a, T>(t1:&'a T, t2:&'a T) -> &'a T { unimplemented!() }
{% endhighlight %}

In this example, `x` is allowed because the exclusive reference `u` is obtained and released while there are no other references to `t`.

But `y` is rejected because the `&t` executes before it, creating a live reference to `t`.

The below example demonstrates that the borrow checker considers the specific order of storage cell allocations:
{% highlight rust %}
use tokio::process::Command;

fn foo() {
    // Example 1
    {
        let process = Command::new("ls").arg("--help").spawn(); // ACCEPTED
    }
    // Example 2
    {
        let command = Command::new("ls").arg("--help"); // ERROR - temporary value dropped while borrowed
        command.spawn();
    }
    // Example 3
    {
        let mut command = Command::new("ls");
    	command.arg("--help");
    	command.spawn(); // ACCEPTED
    }
}
{% endhighlight %}

Here, `tokio` is a library which provides helpers for running code asynchronously, such as a multi-threaded runtime and functions for creating **Futures**. `Command` is an example of a Future that returns the exit status of a spawned shell command.

`tokio::process::Command` uses the "`&mut`-builder pattern"[Insert link to explain the two builder patterns. `&mut`-based builders make conditional logic easier] (as opposed to the "owned-builder" pattern). There is a tradeoff between these two patterns: 

Example 2 fails because `Command::new` creates a temporary, but `arg` only returns a `&mut` to its input. So by the sequence point, the storage for the temporary is deallocated.

Example 1 is very similar, but is accepted because the storage for that temporary is deallocated after `spawn` uses it: The position of the `;` determines when storage is deallocated.

Example 3 is accepted because the temporary is moved into `command`, which remains valid for the entire scope.

The below example demonstrates the order destructors run relative to the other stages:
[Include example where implementing Drop causes code to fail to compile]

All of these experiments are explained by the initial model I provided.

### Lifetime Issues

#### Solution 16: Mark additional lifetime variables

Rust generally infers the correct borrow-set for a lifetime variable, but isn't very good at determining how many independent lifetimes there should be.

{% highlight rust %}
pub struct Foo<'b>(&'b u8);
trait Bar<'a> { }
impl<'a, 'b> Bar<'a> for Foo<'b> { }
impl<'b> Foo<'b> {
    fn foo(&self) -> &impl Bar { self } // ERROR - hidden type for `impl Trait` captures lifetime that does not appear in bounds
}
{% endhighlight %}

This example fails to compile because the [lifetime-elision rules](https://doc.rust-lang.org/nomicon/lifetime-elision.html) give `Bar` a single output lifetime, but there are actually two independent lifetimes.

A fully expanded `foo` looks like:

{% highlight rust %}
pub struct Foo<'b>(&'b u8);
trait Bar<'a> { }
impl<'a, 'b> Bar<'a> for Foo<'b> { }
impl<'b> Foo<'b> {
    fn foo<'a>(&'a self) -> &'a(impl Bar<'a> + 'a) { self } // ERROR - hidden type for `impl Trait` captures lifetime that does not appear in bounds
}
{% endhighlight %}

The lifetimes `'a` and `'b` are independent, but `foo` uses `'a` for everything.
{% highlight rust %}
pub struct Foo<'b>(&'b u8);
trait Bar<'a> { }
impl<'a, 'b> Bar<'a> for Foo<'b> { }
impl<'b> Foo<'b> {
    fn foo(&self) -> &(impl Bar + 'b) { self }
}
{% endhighlight %}

Errors like these are common when returning `impl Trait` values. Add an explicit lifetime `'b` or fresh anonymous lifetime `'_` to the bound.

#### Solution 17: Return an owned wrapper object

Consider the following argument:
1. A reference `&T` can never be null.
2. The [Null Pointer Optimization](https://doc.rust-lang.org/1.30.0/book/first-edition/ffi.html#the-nullable-pointer-optimization) allows the `None` discriminant for `Option<&T>` to be stored as the all-zeroes bit pattern.
3. For all `T`, `Option<&T>` is then effectively a `**T`(pointer-to-pointer-to-T).
4. `Option<&T> = **T = &Option<T>`

And so it should be possible to write the function `foo` below:
{% highlight rust %}
fn foo<T>(x:Option<&T>) -> &Option<T> {
  match x {
    None => &None,
    Some(ref x) => unimplemented!(),
  }
}
{% endhighlight %}

The `None` case takes advantage of the [undocumented](https://doc.rust-lang.org/reference/items/enumerations.html) Rust feature where nullary enum variants implicitly create a`'static` reference to some global constant. The `&None` refers to this hidden unnamed constant[^2].

The `Some` case is possible to implement with unsafe code, but not all types allow the NPO and so `foo` is not possible in general. For `u8`:

{% highlight rust %}
use std::num::NonZeroU8;

type T<'a> = Option<&'a NonZeroU8>;
type U<'a> = &'a Option<NonZeroU8>;
fn foo(x:T) -> U {
  match x {
    None => &None,
    Some(x) => unsafe { std::mem::transmute::<&NonZeroU8, U>(x) },
  }
}

fn main() {
    println!("x:{:?}", foo(NonZeroU8::new(5).as_ref()).unwrap().get())
}
{% endhighlight %}

The first problem with `foo::<u8>` is that `Option<u8>` requires `2` bytes: `1` for the enum discriminant, and `1` for the `u8`. And while we can point to the existing `u8`, we can't acquire the byte preceding it. We could allocate 2 bytes and copy `x` and the enum tag into them, but:
* A local variable would be temporary
* A global variable would return a reference whose lifetime should be bounded by the next call to `foo`.
* A dynamically-allocated value would work, but since it's unowned this would leak memory.

For similar reasons, this weaker `bar` is also impossible:
{% highlight rust %}
fn bar<T>(x:Option<&T>) -> impl Deref<Target=Option<T>> { unimplemented!()}
{% endhighlight %}

Here, any type implementing `Deref` is allowed, so you'd think we have more freedom. But:
* `Deref` eventually requires a shared reference to an `Option<T>`, which is still impossible to acquire
* It's impossible to construct an `Option<T>` because it could own a `T` while `x` does not own its `T`.

The even weaker `baz` is possible, however:
{% highlight rust %}
use std::ops::Deref;

fn baz<T: Clone>(x:Option<&T>) -> impl Deref<Target=Option<T>> {
  match x {
    None => Ref(None),
    Some(x) => Ref(Some(x.clone())),
  }
}

struct Ref<T>(Option<T>);
impl<T> Deref for Ref<T> {
  type Target = Option<T>;
  fn deref(&self) -> &Self::Target { &self.0 }
}
{% endhighlight %}

More generally: If you have trouble returning a reference to the object you need, instead return an owned object that knows where to find it.

The `RefCell` type uses this technique with similar `Ref` and `RefMut` types. Any functions that return a `&T` can generally return an `impl Deref<Target=T>` instead, which helps future-proof an API.

The syntax `&*x` will convert `x` from an `impl Deref<Target=T>` to a `&T`. But remember that references don't keep an object alive:

{% highlight rust %}
use std::cell::{RefCell, Ref};

fn foo() {
    let xref = &*RefCell::new(5).borrow();
    println!("xref:{}", xref);
}
{% endhighlight %}

so if a `Deref` owns a temporary, you still can't return a reference to it: You'll need to return that `Deref` or one derived from it:

{% highlight %}
use std::cell::{RefCell, Ref};
use std::ops::Deref;

struct Foo<T>(RefCell<T>);
impl<T> Foo<T> {
  fn foo(& self) -> impl Deref<Target=T> + '_ { self.0.borrow() }
  
  // ERROR - Reference to temporary
  fn bar<'a, U: 'a>(&'a self, modify:fn (&T) -> &U) -> impl Deref<Target=U> + 'a {
    modify(&*self.0.borrow())
  }
  // PASS - Use RefCell's map to modify the contained value
  fn baz<'a, U: 'a>(&'a self, modify:fn (&T) -> &U) -> impl Deref<Target=U> + 'a {
    Ref::map(self.0.borrow(), modify)
  }
}

{% endhighlight %}

## Case Studies

This section contains realistic scenarios I've encountered writing Rust, and how I used the above rules to reason through them.

### NES Emulator

My NES emulator had a shared data bus between two hardware components: The CPU and PPU. When the CPU read or wrote to specific memory addresses, those referenced registers on the PPU or initiated a transfer of data from the CPU to the PPU.
{% highlight rust %}
struct CpuPpuInterconnect {
  cpu: *mut Cpu,
  ppu: *mut Ppu,
}
struct Cpu{shared_bus: CpuPpuInterconnect}

struct Ppu;
fn copy(ic:&CpuPpuInterconnect) {
  let cpu:&Cpu = unsafe { &*ic.cpu };
  let ppu:&mut Ppu = unsafe { &mut *ic.ppu };
  ppu.write(cpu.read());
}

impl Cpu { fn read(&self) -> u8 { unimplemented!() } }
impl Ppu { fn write(&mut self, x:u8) { unimplemented!() } }
{% endhighlight %}

This is simplified from the original code. Here, the `CPU` holds a `CpuPpuInterconnect`, which holds a pointer back to the `CPU`. Accessing the `CPU` requires `unsafe` to dereference the pointer.

The question is: How can this be modeled in standard Rust without using any unsafe code?

* **Step 1**: Remove unnecessary `mut` qualifiers.

The `cpu` field is a `*mut Cpu`, but it is only used as input to `read` which takes a `&Cpu`. Since it's okay to have multiple shared references, we should first convert it to a `*const Cpu`:

{% highlight rust %}
struct CpuPpuInterconnect {
  cpu: *mut Cpu,
  ppu: *mut Ppu,
}
struct Cpu{shared_bus: CpuPpuInterconnect}

struct Ppu;
fn copy(ic:&CpuPpuInterconnect) {
  let cpu:&Cpu = unsafe { &*ic.cpu };
  let ppu:&mut Ppu = unsafe { &mut *ic.ppu };
  ppu.write(cpu.read());
}

impl Cpu { fn read(&self) -> u8 { unimplemented!() } }
impl Ppu { fn write(&mut self, x:u8) { unimplemented!() } }
{% endhighlight %}

* **Step 2**: Convert pointer to shared reference

`CpuPpuInterconnect` has a reference that must live as long as the `Cpu`. This is specified using a lifetime parameter. And because `Cpu` owns the interconnect, it must take its own lifetime as a parameter.

{% highlight rust %}
struct CpuPpuInterconnect<'a> {
  cpu: &'a Cpu<'a>,
  ppu: *mut Ppu,
}
struct Cpu<'a>{shared_bus: CpuPpuInterconnect<'a>}

struct Ppu;
fn copy(ic:&CpuPpuInterconnect) {
  let cpu:&Cpu = ic.cpu;
  let ppu:&mut Ppu = unsafe { &mut *ic.ppu };
  ppu.write(cpu.read());
}

impl<'a> Cpu<'a> { fn read(&'a self) -> u8 { unimplemented!() } }
impl Ppu { fn write(&mut self, x:u8) { unimplemented!() } }
{% endhighlight %}

One difficulty is in actually constructing the `CpuPpuInterconnect` here. The below seems to work:

{% highlight rust %}
fn main() {
  let mut cpu:Cpu;
  let mut ppu = Ppu;
  let ic = CpuPpuInterconnect {
    cpu: unsafe { std::mem::uninitialized() }, // DANGER
    ppu:&mut ppu,
  };
  cpu = Cpu{shared_bus: ic };
  unsafe {
    let dest:*mut &Cpu = &mut cpu.shared_bus.cpu;
    std::ptr::write(dest, &cpu);
  };
}
{% endhighlight %}

but `ic.cpu` is probably not allowed to be uninitialized even though it's never read from.

Here, it's easier to use an option type:

{% highlight rust %}
struct CpuPpuInterconnect<'a> {
  cpu: Option<&'a Cpu<'a>>,
  ppu: *mut Ppu,
}
struct Cpu<'a>{shared_bus: CpuPpuInterconnect<'a>}

struct Ppu;
fn copy(ic:&CpuPpuInterconnect) {
  let cpu:&Cpu = ic.cpu();
  let ppu:&mut Ppu = unsafe { &mut *ic.ppu };
  ppu.write(cpu.read());
}

impl<'a> Cpu<'a> { fn read(&'a self) -> u8 { unimplemented!() } }
impl Ppu { fn write(&mut self, x:u8) { unimplemented!() } }
impl<'a> CpuPpuInterconnect<'a> {
  pub fn cpu(&'a self) -> &'a Cpu<'a> { self.cpu.as_ref().unwrap() }
}
fn main() {
  let mut ppu = Ppu;
  let ic = CpuPpuInterconnect {
    cpu: None,
    ppu: &mut ppu,
  };
  let cpu = Cpu{shared_bus: ic };
}
{% endhighlight %}

One drawback is that the type allows a `None` value, even though the reference is always valid after initialization.

* **Step 3**: Handle the exclusive reference

The `*mut Ppu` brings fewer problems, but there are two minor points
{% highlight rust %}
struct CpuPpuInterconnect<'a> {
  cpu: Option<&'a Cpu<'a>>,
  ppu: &'a mut Ppu,
}
struct Cpu<'a>{shared_bus: CpuPpuInterconnect<'a>}

struct Ppu;
fn copy(ic:&mut CpuPpuInterconnect) {
  let cpu:&Cpu = ic.cpu.as_ref().unwrap();
  let ppu:&mut Ppu = ic.ppu;
  ppu.write(cpu.read());
}

impl<'a> Cpu<'a> { fn read(&'a self) -> u8 { unimplemented!() } }
impl Ppu { fn write(&mut self, x:u8) { unimplemented!() } }
impl<'a> CpuPpuInterconnect<'a> {
  pub fn cpu(&'a self) -> &'a Cpu<'a> { self.cpu.as_ref().unwrap() }
}
fn main() {
  let mut ppu = Ppu;
  let ic = CpuPpuInterconnect {
    cpu: None,
    ppu: &mut ppu,
  };
  let cpu = Cpu{shared_bus: ic };
}
{% endhighlight %}

First, `copy` now takes an exclusive reference to the `CpuPpuInterconnect`. Pointers don't need exclusive access, but the reference `ic.ppu` does.

Second, the getter `CpuPpuInterconnect::cpu` had to be inlined in `copy`, because functions lock the entire structure. [Insert reference to Solution #1 which covers this]

## Conclusion

### References
* The [Rustonomicon](https://doc.rust-lang.org/nomicon/index.html)
* The [Stacked Borrows](https://www.ralfj.de/blog/2018/08/07/stacked-borrows.html) model
* The [Oxide](https://arxiv.org/abs/1903.00982) formalization of Rust's borrow checker
* [Definition](https://rust-lang.github.io/rfcs/0387-higher-ranked-trait-bounds.html) of early/late-bound lifetimes and `for<'a>` syntax.
* [Lifetimes RFC](https://rust-lang.github.io/rfcs/0387-higher-ranked-trait-bounds.html)
* [Lifetimes Lint Warning](https://github.com/rust-lang/rust/issues/42868)

### Footnotes

[^1]: At worse, the lexical scope of `fn main() { ... }` bounds every other scope. Theoretically, `static` variables have a lifetime strictly longer than even `main`, but they are restricted from running destructors
[^2]: `None` can't be the name of the constant: 1. `None` is the name of the type. 2. It can't be an ordinary variable - A. `Option<T>` is generic but `None` isn't, so their names would collide. B. It doesn't trigger the [non_snake_case](https://doc.rust-lang.org/rustc/lints/listing/warn-by-default.html#non-snake-case) warning. 3. It can't be a function - even though tuple structs can be used as functions without triggering the warning - because Rust requires parentheses to call a function. 4. 

[^3]: Banks use double-entry accounting. The code is vulnerable to data races. The code is unauthenticated. If the server goes down, money may be credited or debited without the rest of the side effects occurring.