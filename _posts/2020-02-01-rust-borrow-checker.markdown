---
layout: post
author: Michael Burge
title: "The Complete Guide to Rust's Borrow Checker"
started_date: 2020-02-01 20:00:04 -0700
date: 2020-02-01 20:00:04 -0700
tags:
  - rust
---

This article is aimed at beginner or practicing Rust developers. It aims to completely explain the borrow checker, so that any compilation error can be solved using a list of standard techniques.

If you are:
* A Rust beginner: You frequently have difficulty getting seemingly-reasonable code to compile. The entire article should be a great boon to you.
* An intermediate Rust developer: There may be gaps on "advanced" or less common concepts, so the completeness here may be helpful. For example, you may not know: What exactly does a lifetime variable represent? Can it be used like an abstract variable for other purposes than just variable lifetimes? Answer: No - its semantics follow completely from the standard borrow checker rules.
* Someone interested in Rust: The first two sections may be interesting. The third is long, difficult to relate to, and unlikely to inform.
* A recruiter or internet commenter with an opinion: Would you kindly please "fuck off"?

<!-- -->
Contents:
* This list is replaced with the Table of Contents during page generation.
{:toc}
Contents:
* High-level concepts defining the borrow checker
* Reasons why one would want a borrow checker at all
* A "cookbook" of techniques for correcting borrow checker errors, with code samples.

If you see a borrow-checker error that is not explained by this article, send me a message via email or Twitter and I'll consider adding it to the list.

## Concepts

Under a borrow checker, code accesses data in one of three ways:
* Ownership
* Exclusive access
* Shared access

{% highlight rust %}
fn foo() {
  let owned:u8 = 5;
  let shared:&u8 = &owned;
  let exclusive:&mut u8 = &mut owned;
}
{% endhighlight %}

**Owned** data has an associated variable. This variable allocates storage for the data when it is created, and deallocate it when they expire. This (creation, expiration) pair defines its **Scope**. It is common for these to be tied to a block of code being entered - such a scope is a **Lexical Scope**. The data that a variable owns may **Reference** data owned by a different variable.

**Rule #1**: A reference to data must exist after its allocation, and before its deallocation.

The following snippet is rejected via the allocation clause:
{% highlight rust %}
fn foo() {
  let owned:u8;
  let shared:&u8 = &owned;
  let exclusive:&mut u8 = &mut owned;
  owned = 5;
}
{% endhighlight %}

And this one is rejected via the deallocation clause:
{% highlight rust %}
fn foo() {
  let shared:&u8 = { let owned = 5; &owned};
}
{% endhighlight %}

The braces `{}` define a Lexical Scope. Variables created within such a scope are also destroyed before that scope ends. So `shared` would reference the deallocated `owned` if this were allowed.

Ownership is tied to a specific variable, not the data. The following code is rejected because the owning variable **Transfers** its ownership while references exist:
{% highlight rust %}
fn use_variable<T>(t:T) { }

fn foo<T>(owned:T) {
  let shared:&T = &owned;
  let transferred:T = owned;
  use_variable(shared);
}
{% endhighlight %}

This example uses a generic type `T` because simple integer types like `u8` are more permissive and would be accepted. I'll cover this in more detail in the section on **Copy** types.

Rust differs from other languages with references by also distingishing **Shared** and **Exclusive** references. This leads to the second rule:

* **Rule #2**: If an exclusive reference exists to data, then no other references to it exist.

By default, shared references only allow reads while exclusive references allow reads and writes. However, there are many types in the standard library that provide **Interior Mutability**, which allows you to mutate through a shared reference. They don't bypass Rust's guarantees at all - each of them has specific requirements that make it safe. I'll cover these later.

The syntax `&T` and `&mut T` are sometimes called **Immutable Reference** and **Mutable Reference** respectively. These reflect the default read/write access. I prefer the terms Shared and Exclusive because they're more accurate when you consider the rest of Rust's features. It's better to think of the `&mut T` syntax as a historical anomaly that is difficult to change.

The borrow checker rules are defined by runtime behavior. However, no runtime code executes to validate them - they are checked at compile-time by the **Static Borrow-Checker**. The types providing interior mutability all have a slight runtime cost because they skip this compile-time validation.

Lifetime variables are a Rust feature related to scopes. A lifetime variable is resolved by the compiler to a specific borrow-set: A set of variables and how they were borrowed(exclusive or shared). When you call a function or store references in a type, the lifetime variables allow the reference's dependencies to follow it into a new context. Because lifetime variables reduce to a borrow-set, the rules required of them are exactly the same as the borrow checker's rules.

Late-bound lifetime variables are those whose borrow-set differs at every call of a function. Early-bound lifetimes are fixed at every instantiation. (Example of these 2. Obvious candidate is a static value, but are any arguments early-bound?)

Async functions introduce one more problem: If a reference is used across an await, its lifetime increases to the future as a whole. Sometimes these can be corrected by treating sections almost like separate bodies.

The Copy trait interacts uniquely with the borrow checker: A simple memcpy () is enough to duplicate a Copy variable. This applies to any simple data: integers, pointers, shared references. However, exclusive references and data owned through a pointer are not Copy: it is illegal to own two exclusive references or the same value twice.

Rc is a reference-counted ownership. So two owners can each obtain a shared reference to their data, and Drop only decreases the reference count (except the last one). It is common to combine this with interior mutability like Rwlock or Refcell. Otherwise neither owner could get exclusive access to the data.

### Interior Mutability
The different interior mutability types provide different guarantees:
* RwLock: It is safe to elevate a shared reference to an exclusive reference by blocking until all active borrows are released.
* RefCell: checks for active borrows (the Ref and RefMut) at runtime and panics. This is a type of dynamically-checked borrow checker. It is only valid in single-threaded context because the reference count is not protected, so a Ref dropped at the same time in two different threads could only update the counter once. 
* Mutex: The same as RwLock, but shared references are also exclusive. This prevents a constant stream of readers from preventing any writers from ever obtaining a lock. It treats readers and writers equally, so guarantees progress for all interested values. 
* Cell: A cell never allows a reference to it's interior. Updates happen with set () and get () which always copy the entire value. This only works in a single thread so that two set() don't happen at the same time.
* UnsafeCell: Converts a shared reference into an exclusive pointer. It is unsafe to use the pointer, because you must verify outside the compiler that there are no other references or readers/writers. (It is however ok to have two mut * to the same data. But not references.)

With these in mind, I will give a list of common errors and how to resolve them. I will appeal to the above rules/concepts each time.

My NES emulator provides many good examples. It was my first Rust program, and there are several places I "gave up" and used unsafe without understanding these rules. I've gone through and classified every unsafe and show how to resolve. I also extract patterns from an unrelated project.

## Why have a borrow checker?

This section is intended to be a complete list of reasons a borrow checker could be useful(whether static, runtime, or abstract). If you would like to contribute an additional entry, please contact me via email or Twitter.

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

This example is fundamentally flawed in a number of ways[^2]. Among the problems is that it is vulnerable to **Data Races**. In practice, banks want something that looks more like this:
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

The following C code has a problem:
{% highlight rust %}
[Insert snippet demonstrating pointer aliasing
{% endhighlight %}

It's not a correctness problem: The code executes fine as-is. It's a performance problem: The compiler can't guarantee that the pointer is unique. So when the helper function is called, the compiler must pessimistically assume the value has changed and generate a memory-read every single time.

The CPU also tracks which memory addresses have been invalidated, so luckily such a read will hit L1 cache rather than main memory. But it would be even register if that value could be stored in a register.

Rust allows you to mark variables as having exclusive access to data. So the compiler can aggressively cache values in registers.

C did eventually add the `restrict` keyword(it had it before Rust did). This makes the compiler assume that a pointer is unique. But C doesn't have a borrow checker, so if you violate this assumption it's undefined behavior.

Rust for larger codebases may be faster than C or C++, if pervasive pointer-aliasing can be eliminated by the borrow-checker and limited to just a few places with interior mutability.

### Style

* As a matter of style, the borrow checker encourages you to write in a single-assignment form, which is easier for compilers to prove correct and which some people find easier to reason about.

* You can implement a runtime borrow checker in e.g. C++. But nobody would subject themselves to that without using a statically-checked borrow checker first. Once you're convinced the rules are meaningful for your program, it's natural to want a statically-checked one to save time & reduce runtime errors.

### Program Traces
* Since program traces always satisfy the borrow checker rules(ignoring pointers to expired data), programs which require an exception likely only require that exception in a small fragment. Requiring unsafe or interior mutability helps limit this exception to a small place.

## Common Scenarios

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

#### Solution 8: Use an Arena

There is a 3rd-party library providing the `Arena` and `TypedArena` types. These are containers that deallocate all objects "at the same time", allowing them to maintain references to each other.

{% highlight rust %}
INCLUDE SAMPLE CODE SOLVING CPUPPUINTERCONNECT USING AN ARENA
{% endhighlight %}

### Two Logical Owners

Consider a web interface to a controller that manages many other servers. You can resize the cluster, distribute tasks to the cluster, and kill long-running or erroneous tasks through the interface.

The web interface needs access to the server list. It can't be a reference, because it's possible that the web interface is the only accessor to the server data. A reference isn't allowed to `Drop` the data, but if the website is the only accessor it should drop that data when it is shut down.

There is also a thread that runs in the background. Even if nobody visits the website, idle servers must have tasks allocated to them. So the background work cannot be owned by the website. And the website doesn't necessarily need access to the background task either. Both the background task and the website have **Shared Ownership** over the server data, because neither needs to be aware of the other's existence.

A reference-counted type like `Rc` or `Arc` solve this problem. They implement `Clone` by increasing a reference count, and `Drop` by decreasing it. So both the web interface and the background task can be given a `clone` of the data.

However, it's not possible for either service to mutate through a reference-counted type: These types only provide a shared reference, because exclusive access requires there to be exactly 1 owner. To solve this, you can store an interior-mutability[Include link to Interior Mutability section] type to elevate the shared reference to an exclusive one. Since `Arc` is usually used in multi-threaded contexts(like a website and background task on separate threads), usually `Mutex` or `RwLock` are used.

`Arc` exists in other languages. C++ has `shared_ptr` for example. However, Rust is unique in safely providing `Rc`, which only works in single-threaded context. The traits `Send` and `Sync` mark which types can or cannot be safely sent across threads, and `Rc` does not implement these. `Rc` is faster than `Arc` and you'll never accidentally misuse it without using `unsafe`. But don't be afraid to use `Arc` if it solves a problem.

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

#### Solution 5: Use pointers

The pointer type `*mut T` carries no assumption that it has exclusive access. You can wrap any access to `T` in an `unsafe` block and it will work.

#### Solution 6: Narrow the writes

A **Trace** is a log of all runtime statements made by a program. A "CPU Trace" logs each CPU instruction as run in sequential order. A modern CPU may have multiple cores, hyperthreads, speculative execution, etc., but usually there must exist a sequential list of (Execution Context #, Instruction) pairs that replicates the observed behavior. 

Every CPU trace satisfies the exclusivity constraint: The "lock" is 1. Limited in time - it is only held for a single CPU instruction and 2. Limited in scope - it only locks a fixed-size bit pattern(say, 64 bits).

So if you write a program which claims to require multiple exclusive references to data, it is interesting to consider that the execution of this program will not. So, it is worth considering how this "extraneous" exclusive reference is required.

The root cause is this: Locks are only problematic if they are held for a long time, or over a large amount of data. If you believe you need multiple writers, it is because you are holding locks for too long.

Consider passing shared references to data, and elevating those shared references to exclusive references only when you actually perform the write. This sounds similar to Solution #2, but with one difference: Earlier, we started by making high-level functions shared. Here, we start by making everything except low-level-functions shared. The program design is slightly different, because the interior mutability types are held lower in the program stack.

The trade-off is this: It is convenient for development to move interior-mutability lower-level, because it causes fewer compile-time errors and gives you more freedom to design your program. It is convenient for runtime debugging for interior-mutability to be higher-level, because there will be fewer locks in total and fewer interactions between locks.

Examples of problematic interactions include: Panics(if using `RefCell`), deadlocks(if using `RwLock` or `Mutex), excessive copying(if using `Cell`). You should never trigger undefined behavior using these types, but that doesn't mean your program will work. Since `& and `&mut` are checked at compile-time, you won't have any problems at runtime using them.

If the writers are on separate threads, you'll want either a `Mutex` or `RwLock` somewhere in your program. They will take an explicit lock, preventing the other thread from executing.

#### Solution 7: Delay the writes

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
#### Solution 9: Drop the variable sooner

Variables are dropped in reverse order. `flag` here is dropped before `f`, but `f` still maintains a reference to it.

This doesn't really matter, because dropping `f` won't invoke any destructor that uses `flag`. But the mere existence of the reference means the compiler won't accept it.

There are 3 ways to accomplish this:

* Explicitly, using `drop`
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

* Reversing the order of `flag` and `f` so that `flag` is dropped after f`:
{% highlight rust %}
fn foo<T>(x:T) {
  let flag = true;
  let mut f:Box<dyn FnMut(T) -> bool> = Box::new(|t| flag);
  f(x);
  assert!(flag);
}

Note that I switched the function to a shared reference here.

{% endhighlight %}

* Using `FnOnce`

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

* Using a scope to force destruction

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

Here, the intention is to release `a`. But because `&T` is copy, the `drop` only releases a copy of `a` and not `a` itself. A scope allows you to release `a`:
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

* Reorder arguments.

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

The below example demonstrates order of temporaries determining which code is allowed:
{% highlight rust %}
fn foo<T>(mut t:T) {
  let x = bar({ let u:&mut T = &mut t; &t}, &t); // ALLOWED
  // let y = bar(&t, { let u:&mut T = &mut t; &t}); // REJECTED
}

fn bar<'a, T>(t1:&'a T, t2:&'a T) -> &'a T { unimplemented!() }
{% endhighlight %}

In this example, `x` is allowed because the exclusive reference `u` is obtained and released while there are no other references to `t`.

But `y` is rejected because the `&t` executes before it, creating a live reference to `t`.

The below example demonstrates that the specific order of storage cell allocations determines which code is allowed:
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

* Interaction with `RefCell` and `Deref`

## Case Studies

This section contains realistic scenarios I've encountered writing Rust, and how I used the above rules to reason through them.

### `&Option<T>`
Consider the following argument:
1. A reference `&T` can never be null.
2. The [Null Pointer Optimization](TODO: Find a link) for types that can never be null, allows the `None` discriminant to be stored as the all-zeroes bit pattern.
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

The `None` case takes advantage of the [undocumented](https://doc.rust-lang.org/reference/items/enumerations.html) Rust feature where nullary enum variants implicitly create a`'static` reference to some global constant. The `&None` refers to this hidden unnamed constant[^1].

The `Some` case is possible to implement with unsafe code, but not all types allow the NPO and so `foo` is not possible in general. For `u8`:

{% highlight rust %}
fn impossible_foo(x:Option<&u8>) -> &Option<u8> {
  match x {
    None => &None,
    Some(ref x) => unimplemented!(),
  }
}
{% endhighlight %}

The first problem with `impossible_foo` is that `Option<u8>` requires `2` bytes: `1` for the enum discriminant, and `1` for the `u8`. And while we can point to the existing `u8`, we can't acquire the byte preceding it. This can be corrected by allocating `2` bytes and copying `x` and the enum tag into them. However, this storage would be temporary and impossible to take a reference to.

For similar reasons, this weaker `bar` is also impossible:
{% highlight rust %}
fn bar<T>(x:Option<&T>) -> impl Deref<Target=Option<T>> { unimplemented!()}
{% endhighlight %}

Here, any type implementing `Deref` is allowed, so you'd think we have more freedom. But:
* `Deref` eventually requires a shared reference to an `Option<T>`, which is still impossible to acquire
* It's impossible to constructo an `Option<T>` because it could own a `T` while `x` does not own its `T`.

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

There is a general principle that, if you have trouble returning a reference to the object you need, you can instead return an object that owns it. So references to the object always carry references to the object you want.

The `RefCell` type uses this technique with similar `Ref` and `RefMut` types. Any functions that return a `&T` can generally return an `impl Deref<Target=T>` instead, which helps future-proof an API.

* Interaction with `async`
[An `await` increases the lifetime of a reference from the surrounding block to the lifetime of the entire future. Possible solution: Use scopes between awaits; reacquire the reference after the await.]
[Use a map, since it takes references to keys and returns references to elements. So can demonstrate both]
[Is it impossible to store an exclusive reference across an await, unless the value is owned by the block? Otherwise, it seems like whoever owns it could access it while the future is `await`ing]

[^1] The name can't be `None` because `None` is the name of the type. And if it were an ordinary variable, you'd trigger the [non_snake_case](https://doc.rust-lang.org/rustc/lints/listing/warn-by-default.html#non-snake-case) warning. It can't be a function - even though tuple structs can be used as functions without triggering the warning - because Rust requires parentheses to call a function.
[^2] Banks use double-entry accounting. The code is vulnerable to data races. The code is unauthenticated. If the server goes down, money may be credited or debited without the rest of the side effects occurring.