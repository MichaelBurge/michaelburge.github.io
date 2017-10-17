---
layout: post
author: Michael Burge
title: "Haskell Typeclasses vs. C++ Classes"
date: 2017-10-15 16:20:00 -0700
tags:
  - c
  - haskell
---

Classes from Object-Oriented Programming languages such as C++ allow types to declare a pre-selected list of overridable functions.

Typeclasses in Haskell are a general-purpose way to write functions whose implementations change depending on the type of a parameter.

They're used very differently in practice, but this article will:
* Describe how vtables implement C++ classes
* Show that Haskell constructs a very similar data structure in typeclass-using code

## Classes

C++ has the concept of a "class" - a record together with functions that manipulate it. The class creates a new namespace for the functions, so that different classes can have functions with the same name. Each function also takes the record as an implicit argument.

Here is an example of a class:
{% highlight c++ %}
// Example 1
struct x {
  int n;
  void f();
  void g();
  void h();
};

void all_funcs_x(x _x) {
  _x.f();
  _x.g();
  _x.h();
}
{% endhighlight %}

and here is how it might translate into C:

{% highlight c %}
// Example 1
typedef struct x {
  int n;
} x;

void x_f(x*);
void x_g(x*);
void x_h(x*);

void all_funcs_x(x _x) {
  x_f(&_x);
  x_g(&_x);
  x_h(&_x);
}
{% endhighlight %}

A C++ compiler infers the namespace from a variable's type, while a C programmer would have to remember which variant of `f` he wanted to invoke.

C++ also supports subtyping: `x` is a subtype of `y` if every record field and function in `y` is also in `x`. The narrower type `x` is responsible for declaring that it is a subtype of `y`, and then it doesn't have to repeat the fields and functions it __derived__ from `y`.

{% highlight c++ %}
// Example 2

struct y : x { };

void all_funcs_y(y _y) {
  all_funcs_x(_y);
}
{% endhighlight %}

Notice that `all_funcs_x` was invoked on an argument of type `y` even though we earlier declared it to take an `x`. Conceptually, a new function `y_cast_x` is created that converts the `x` into a `y` by copying its record fields. The C++ compiler will silently insert calls to this function to convert `y` into `x` by dropping extra fields; but will not convert `x` into `y`, since it has no way of knowing how to repopulate them.

{% highlight c %}
// Example 2
typedef struct y {
  int n;
} y;

x y_cast_x(y _y) {
  x _x;
  _x.n = _y.n;
  return _x;
}

void all_funcs_y(y _y) {
  all_funcs_x(y_cast_x(_y));
}
{% endhighlight %}

In practice, a C++ compiler will organize the fields so that `y_cast_x` is free when used with references or pointers.

Notice that `f`, `g`, and `h` must refer to `x`'s namespace. They can't refer to `y`, because the functions have no way of knowing what the "original type" of `_x` was. They are only given a copy without the extraneous fields. So if `y` declared its own `f`, `g`, and `h` functions in its own `y` namespace, these would be ignored and the ones in `x`'s namespace called instead.

C++ also allows you to declare that certain functions should be looked up in the most-specific namespace available.

{% highlight c++ %}
// Example 3
struct x {
  int n;
  virtual void f();
  virtual void g();
  virtual void h();
};

void all_funcs_x(x _x) {
  _x.f();
  _x.g();
  _x.h();
}
{% endhighlight %}

This is implemented by creating a new implicit type `x_vtable` that has references to `f`, `g`, and `h`, adding an implicit field for it to `x`, and then copying these references during each call to `y_cast_x`:

{% highlight c %}
// Example 3
typedef struct x_vtable {
  void (*f)(); // 64-bit pointers to functions
  void (*g)();
  void (*h)();
} x_vtable;

typedef struct y_vtable {
  void (*f)();
  void (*g)();
  void (*h)();
} y_vtable;

x_vtable yvtable_cast_xvtable(y_vtable yvt) {
  x_vtable xvt;
  xvt.f = yvt.f;
  xvt.g = yvt.g;
  xvt.h = yvt.h;
  return xvt;
}

typedef struct x {
  x_vtable *_xvt;
  int n;
} x;

typedef struct y {
  y_vtable *_yvt;
  int n;
};

x y_cast_x(y _y) {
  x _x;
  _x.n = _y.n;
  _x->_xvt = yvtable_cast_xvtable(_y->_xvt);
  return _x;
}

void all_funcs_x(x _x) {
  (*((_x._xvt)->f))(&_x);
  (*((_x._xvt)->g))(&_x);
  (*((_x._xvt)->h))(&_x);
}

void all_funcs_y(y _y) {
  all_funcs_x(y_cast_x(_y));
}
{% endhighlight %}

There is also an implicit subtyping relation between `x_vtable` and `y_vtable`, and the compilers arrange the fields such that `yvtable_cast_xvtable` is usually free in the same manner than `y_cast_x` is usually free.

The compiler can be made to output its vtables. 
{% highlight cpp %}
// Example 3
// g++ -fdump-class-hierarchy -O3 -c x.cpp
Vtable for x
x::_ZTV1x: 5u entries
0     (int (*)(...))0
8     (int (*)(...))(& _ZTI1x)
16    (int (*)(...))x::f
24    (int (*)(...))x::g
32    (int (*)(...))x::h

Class x
   size=16 align=8
   base size=12 base align=8
x (0x0x7fb0516125a0) 0
    vptr=((& x::_ZTV1x) + 16u)
{% endhighlight %}

Besides the three functions we intended, we also get an __offset-to-top__ parameter at offset 0 and a __typeinfo__ pointer at offset 8. The first is used for multiple-inheritance, and the second is a pointer to a [`type_info`](http://www.cplusplus.com/reference/typeinfo/type_info/) object. The vtable usually points to offset `16` to simplify code generation and not offset `0`, so negative offsets are used when accessing the first two.

Why are functions not virtual by default? Two reasons:
* The data in a C-style struct can be copied with `memcpy` and reused in other programs(or later versions of the same program). vtables are absolute pointers, so a simple bytewise copy is unusable except by the original program.
* They are slightly slower, because every implicit cast might cause an extra 64-bit pointer to be written, every virtual function call adds a layer of indirection, and - most importantly - virtual function calls prevent inlining.

You can see the indirections in the disassembly.
{% highlight bash %}
// void all_funcs_x(x& _x) {
$ g++ -O3 -fPIC -c x.cpp
$ 0000000000000000 <_Z11all_funcs_xR1x>:
   0: push   rbx
   1: mov    rax,QWORD PTR [rdi]
   4: mov    rbx,rdi
   7: call   QWORD PTR [rax]
   9: mov    rax,QWORD PTR [rbx]
   c: mov    rdi,rbx
   f: call   QWORD PTR [rax+0x8]
  12: mov    rax,QWORD PTR [rbx]
  15: mov    rdi,rbx
  18: pop    rbx
  19: mov    rax,QWORD PTR [rax+0x10]
  1d: jmp    rax
  1f: nop
{% endhighlight %}

The vtable pointer is located at offset `0` in `x`, so the first `mov` copies it into `rax`. Then, because the vtable points at the first function `f`, the `call` and `jmp` instructions use offsets `0`, `0x8`, `0x10` to call `f`, `g`, and `h`.

Here's the version using non-virtual function calls:
{% highlight bash %}
$ g++ -O3 -fPIC -c x.cpp
0000000000000000 <_Z11all_funcs_xR1x>:
   0: push   rbx
   1: mov    rbx,rdi
   4: call   9 <_Z11all_funcs_xR1x+0x9>
   9: mov    rdi,rbx
   c: call   11 <_Z11all_funcs_xR1x+0x11>
  11: mov    rdi,rbx
  14: pop    rbx
  15: jmp    1a <_Z11all_funcs_xR1x+0x1a>

$ objdump -r x.o
RELOCATION RECORDS FOR [.text]:
OFFSET           TYPE              VALUE 
0000000000000005 R_X86_64_PLT32    _ZN1x1fEv-0x0000000000000004
000000000000000d R_X86_64_PLT32    _ZN1x1gEv-0x0000000000000004
0000000000000016 R_X86_64_PLT32    _ZN1x1hEv-0x0000000000000004
{% endhighlight %}

Those three relocations means that non-virtual function calls are actually available to the linker, which patches their addresses in at link-time. Thus, non-virtual calls always go to the same target regardless of what inputs are passed in, while virtual calls look at the argument to decide which function to call.

## Typeclasses

A Haskell typeclass is a collection of related functions shared by many types. Typeclass instances give rules for finding a specific implementation for any given type. The simplest rule is that a concrete type is given a specific implementation:

{% highlight haskell %}
class TC a where
  f :: a -> IO ()
  g :: a -> IO ()
  h :: a -> IO ()

data X = X
instance TC X where
  f _ = return ()
  g _ = return ()
  h _ = return ()

all_funcs :: TC a => a -> IO ()
all_funcs x = do
  f x
  g x
  h x
{% endhighlight %}

In `all_funcs` above, `a` is a type such that the compiler knows how to find implementations for `f`, `g`, and `h`(the `TC` typeclass).

In an object-oriented language like C++, the rules for locating an implementation might involve traversing the class hierarchy upwards until you find the first defined function with the same name. Depending on the language, this could be greatly complicated by multiple inheritance, roles/traits, or adding methods to the class at runtime. Typeclasses should generalize any situation where it is possible to locate the implementation at compile-time.

### Foreign C code

To start with, we need the non-virtual case. In the previous section, we showed that the virtual/non-virtual distinction can be observed by looking for relocations in the resulting object file. So we want to write Haskell that emits relocations that are resolved at link-time.

In C, every function by default is exposed to the linker and available to be called from other modules; and you have to explicitly mark functions as `static` to hide them from the linker. Haskell depends very heavily on inlining, so functions are hidden by default and you have to explicitly mark linker visibility using the __Foreign Function Interface__.

{% highlight haskell %}
{-# LANGUAGE ForeignFunctionInterface, MagicHash #-}

module ClassX where

import Foreign.Ptr
import GHC.Prim

data X = X Int#

foreign import ccall "f" f :: Ptr X -> IO ()
foreign import ccall "g" g :: Ptr X -> IO ()
foreign import ccall "h" h :: Ptr X -> IO ()

all_funcs :: Ptr X -> IO ()
all_funcs x = do
  f x
  g x
  h x

foreign export ccall "all_funcs" all_funcs :: Ptr X -> IO ()
{% endhighlight %}

The full disassembly generated by these function calls is a bit long, but I can go over the section immediately wrapping the `f x` call. I use `====>` to mark where a block transfers control to the next.

{% highlight cpp %}
$ ghc -fPIC -O2 X.hs
$ objdump -M intel -d X.o

0000000000000058 <c1Xm_info>:
      58: sub    rsp,0x8
      5c: mov    rdi,QWORD PTR [rbx+0x7]
      60: xor    eax,eax
====> 62: call   67 <c1Xm_info+0xf>   # RELOCATION
      67: add    rsp,0x8
      6b: lea    rbx,[rip+0x0]        # RELOCATION
      72: add    rbp,0x8
      76: jmp    QWORD PTR [rbp+0x0]

$ readelf -r X.o
Relocation section '.rela.text' at offset 0x11e8 contains 87 entries:
      Offset       Sym. Name + Addend
      00000000003d .data + 13c
====> 000000000063 f - 4
      00000000006e ghczmprim_GHCziTuple_Z - 3
{% endhighlight %}

The compiler generates 3 relocations for this expression. The latter two are the most relevant:
* `f - 4`: This is the location of our `f` function.
* `ghczmprim_GHCziTuple_Z - 3`: This is a library function that generates an empty tuple.

The important point here is that the location of `f` is resolved by the linker, so it is a fixed implementation comparable to the earlier C code.

Clearly C has a more efficient FFI for calling into foreign C code than Haskell does. But Haskell programmers usually use the module system to import and export code, rather than depending entirely on the linker. You also can't export typeclass constraints via the FFI, which I need in order to claim that they are similar to C++'s vtables.

### Haskell Modules

Let's look at that again without the FFI:

{% highlight haskell %}
module ClassX where

import SomeFuncs(X, f,g,h)

import Foreign.Ptr

all_funcs :: Ptr X -> IO ()
all_funcs x = do
  f x
  g x
  h x

module SomeFuncs where

import Foreign.Ptr
import GHC.Prim
import Data.Word
import Foreign.Storable
import GHC.Storable

data X = X Int#

f :: Ptr X -> IO ()
f ptr = writeWord64OffPtr (castPtr ptr) 0 0x12345678
g :: Ptr X -> IO ()
g ptr = writeWord64OffPtr (castPtr ptr) 0 0x87654321
h :: Ptr X -> IO ()
h ptr = writeWord64OffPtr (castPtr ptr) 0 0x12344321
{% endhighlight %}

The constants make it easier to locate the implementation of `all_funcs` in the disassembly:

{% highlight bash %}
00000000000000a0 <ClassX_allzufuncs_info>:
====> a0: jmp    18 <ClassX_allzufuncs1_info>
  
0000000000000018 <ClassX_allzufuncs1_info>:
      18: lea    rax,[rbp-0x8]
      1c: cmp    rax,r15
      1f: jb     3a <ClassX_allzufuncs1_info+0x22>
====> 21: lea    rax,[rip+0x30]        # 58 <c1WD_info>
      28: mov    QWORD PTR [rbp-0x8],rax
      2c: mov    rbx,r14
      2f: add    rbp,0xfffffffffffffff8
      33: test   bl,0x7
      36: jne    58 <c1WD_info>
      38: jmp    QWORD PTR [rbx]
      3a: lea    rbx,[rip+0x0]       # RELOCATION
      41: jmp    QWORD PTR [r13-0x8]

0000000000000058 <c1WD_info>:
      58: mov    rax,QWORD PTR [rbx+0x7]
      5c: mov    QWORD PTR [rax],0x12345678
      63: mov    ebx,0x87654321
      68: mov    QWORD PTR [rax],rbx
      6b: mov    QWORD PTR [rax],0x12344321
====> 72: mov    rax,QWORD PTR [rip+0x0]      # RELOCATION
      79: lea    rbx,[rax+0x1]
      7d: add    rbp,0x8
      81: jmp    QWORD PTR [rbp+0x0]

Relocation section '.rela.text' at offset 0xb48 contains 2 entries:
      Offset       Sym. Name + Addend
      00000000003d ClassX_allzufuncs1_clo - 4
====> 000000000075 ghczmprim_GHCziTuple_Z - 4
{% endhighlight %}

GHC inlines the 3 pointer writes, and boxes up an empty tuple to return. Thus, the choice of function is determined at compile-time(and therefore link-time), which for our purposes agrees with the non-virtual function case from C.

That FFI imports and exports are matched by the linker, and that module imports are inlined probably won't surprise anybody. But it's good to confirm these easy cases as a baseline before making claims about typeclasses.

### Typeclasses

C++ generates all of its vtables at compile-time. Haskell seems to create its typeclass dictionaries on the heap at the first function with a typeclass constraint, and then passes a pointer to child calls that use it. This indicates to me two possible performance implications:

* Move the typeclass constraint as high up in your program's dependency graph as possible, to avoid repeatedly creating typeclass dictionaries.
* Use 6 or fewer functions in a given typeclass, which is the number of 64-bit integer registers available in the x64 System V ABI. Functions 7 or higher require twice as many `mov` instructions.

Here's a big typeclass to demonstrate these:

{% highlight haskell %}
module ClassX where

class TC a where
  f1 :: a -> IO ()
  f2 :: a -> IO ()
  f3 :: a -> IO ()
  f4 :: a -> IO ()
  f5 :: a -> IO ()
  f6 :: a -> IO ()
  f7 :: a -> IO ()
  f8 :: a -> IO ()              

all_tc :: TC a => a -> IO ()
all_tc x = do
  f1 x
  f2 x
  f3 x
  f4 x
  f5 x
  f6 x
  f7 x
  f8 x
{% endhighlight %}

Looking at the `f4 x` call the instruction I've marked with `*****` locates its typeclass dictionary entry at `[rbx+0x1f]`, with `0x1f = sizeof(pointer) * 4 - 1`. This demonstrates that most typeclass dictionaries in the above example are passed via pointer.

{% highlight bash %}
# ghc -ddump-cmm -fPIC -dynamic -O2 X.hs
0000000000000150 <ClassX_f4_info>:
      150: lea    rax,[rbp-0x8]
      154: cmp    rax,r15
      157: jb     0x22
====> 159: lea    rax,[rip+0x30]        # 0x190
      160: mov    QWORD PTR [rbp-0x8],rax
      164: mov    rbx,r14
      167: add    rbp,0xfffffffffffffff8
      16b: test   bl,0x7
      16e: jne    0x40
      180: jmp    QWORD PTR [rbx]

0000000000000190 <c29T_info>:
***** 190: mov    rbx,QWORD PTR [rbx+0x1f]
      194: add    rbp,0x8
====> 198: jmp    19d <c29T_info+0xd>      # RELOCATION

Relocation section '.rela.text' at offset 0x2070 contains 37 entries:
  Offset          Info           Type           Sym. Value    Sym. Name + Addend
====> 000000000199  003700000002 R_X86_64_PC32     0000000000000000 stg_ap_0_fast - 4
{% endhighlight %}

To see that typeclass dictionaries are constructed at the entry point, notice this `TC_entry` function in GHC's intermediate language `C--`:

{% highlight cpp %}
 ClassX.C:TC_entry() //  [R6, R5, R4, R3, R2]
         { info_tbl: [(c2eM,
                       label: ClassX.C:TC_info
                       rep:HeapRep static {
                             Fun {arity: 8
                                  fun_type: ArgGen [False, False, False, False, False, False, False,
                                                    False]} })]
           stack_info: arg_space: 32 updfr_space: Just 8
         }
     {offset
       c2eM: // global
           Hp = Hp + 72;
           if (Hp > HpLim) (likely: False) goto c2eQ; else goto c2eP;
       c2eQ: // global
           HpAlloc = 72;
           R1 = ClassX.C:TC_closure;
           P64[Sp - 40] = R2;
           P64[Sp - 32] = R3;
           P64[Sp - 24] = R4;
           P64[Sp - 16] = R5;
           P64[Sp - 8] = R6;
           Sp = Sp - 40;
           call (stg_gc_fun)(R1) args: 72, res: 0, upd: 8;
        c2eP: // global
           I64[Hp - 64] = ClassX.C:TC_con_info;
           P64[Hp - 56] = R2;
           P64[Hp - 48] = R3;
           P64[Hp - 40] = R4;
           P64[Hp - 32] = R5;
           P64[Hp - 24] = R6;
           P64[Hp - 16] = P64[Sp];
           P64[Hp - 8] = P64[Sp + 8];
           P64[Hp] = P64[Sp + 16];
           R1 = Hp - 63;
           Sp = Sp + 24;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;          
{% endhighlight %}

The caller into code with a typeclass constraint needs to provide implementations of every function in the typeclass. The typeclass dictionary is passed via the System V ABI to this entry function, which frees up the registers by allocating heap memory and copying them to it.

Haskell keeps a "heap pointer" for efficient heap allocations, just like a stack pointer is used. If the program is out of heap space, it calls into the garbage collector. Otherwise, it's a few simple pointer copies followed by a jump into the actual typeclass-using code.

Looking at the disassembly, `r12` has the heap pointer. One weird point is that heap references are often "off by 1". The `*****` entries above and below are not aligned to an 8-byte boundary. It doesn't matter, because `-0x3f + 0x1f = 0x20`, which is the 8-byte aligned offset for the 4th typeclass entry `f4`.

{% highlight bash %}
0000000000000900 <ClassX_CZCTC_info>:
      900: add    r12,0x48
      904: cmp    r12,QWORD PTR [r13+0x358]
      90b: ja     958 <ClassX_CZCTC_info+0x58>
      90d: lea    rax,[rip+0x0]                # RELOCATION
      914: mov    QWORD PTR [r12-0x40],rax
      919: mov    QWORD PTR [r12-0x38],r14
      91e: mov    QWORD PTR [r12-0x30],rsi
      923: mov    QWORD PTR [r12-0x28],rdi
      928: mov    QWORD PTR [r12-0x20],r8
      92d: mov    QWORD PTR [r12-0x18],r9
      932: mov    rax,QWORD PTR [rbp+0x0]
      936: mov    QWORD PTR [r12-0x10],rax
      93b: mov    rax,QWORD PTR [rbp+0x8]
      93f: mov    QWORD PTR [r12-0x8],rax
      944: mov    rax,QWORD PTR [rbp+0x10]
      948: mov    QWORD PTR [r12],rax
***** 94c: lea    rbx,[r12-0x3f]
      951: add    rbp,0x18
      955: jmp    QWORD PTR [rbp+0x0]
{% endhighlight %}

The typeclass dictionary is very similar to the C++ vtable data structure. It has no offset-to-top member, but I suspect the `TC_con_info` serves a similar purpose to the `type_info` field.

I would guess that GHC generates these data structures at runtime to prevent a combinatorial explosion of pregenerated typeclass dictionaries when using more advanced typeclass rules.

### Specialized Typeclasses

GHC can sometimes specialize a generic typeclass-using function. This causes the typeclass dictionary to be inlined, which removes the need for a separate entry constructor and allocation.

`all_tc_x` below is a __specialization__ of `all_tc` above.

{% highlight haskell %}
data X = X
instance TC X where
  f1 x = return ()
  f2 x = return ()
  f3 x = return ()
  f4 x = return ()
  f5 x = return ()
  f6 x = return ()
  f7 x = return ()
  f8 x = return ()

all_tc_x :: X -> IO ()
all_tc_x x = do
  all_tc x
  all_tc x
  all_tc x
  all_tc x
  all_tc x
  all_tc x
  all_tc x
  all_tc x
  all_tc x
  all_tc x

all_tc_x2 :: IO ()
all_tc_x2 = all_tc_x X

foreign export ccall "all_tc_x2" all_tc_x2 :: IO ()
{% endhighlight %}

The specialized version inlines the definitions for the typeclass dictionary, which enables GHC to reduce the entire output to an empty tuple allocation. That's just 4 instructions, ignoring the FFI wrapper code.

{% highlight bash %}
0000000000000730 <all_tc_x2>:
      730: sub    rsp,0x18
      734: call   739 <all_tc_x2+0x9>
====> 739: mov    rdx,QWORD PTR [rip+0x0]  # RELOCATION
      740: mov    rsi,QWORD PTR [rip+0x0]  # RELOCATION
      747: mov    rdi,rax
      74a: mov    QWORD PTR [rsp],rax
      74e: call   753 <all_tc_x2+0x23>     # RELOCATION
      753: lea    rdx,[rsp+0x8]
      758: mov    rsi,rax
      75b: mov    rdi,rsp
      75e: call   763 <all_tc_x2+0x33>     # RELOCATION
      763: mov    rsi,QWORD PTR [rsp]
      767: lea    rdi,[rip+0x0]            # RELOCATION
      76e: call   773 <all_tc_x2+0x43>
      773: mov    rdi,QWORD PTR [rsp]
      777: call   77c <all_tc_x2+0x4c>     # RELOCATION
      77c: add    rsp,0x18
      780: ret   

Relocation section '.rela.text' at offset 0xd50 contains 39 entries:
      Offset       Sym. Name + Addend
====> 00000000073c ClassX_zdfstableZZC0ZZ - 4

0000000000000610 <ClassX_zdfstableZZC0ZZCmainZZCClassXZZCallzzutczzux2_info>:
====> 610: jmp    5c8 <ClassX_zdfstableZZC0ZZCmainZZCClassXZZCallzzutczzux1_info>

00000000000005c8 <ClassX_zdfstableZZC0ZZCmainZZCClassXZZCallzzutczzux1_info>:
====> 5c8: mov    rax,QWORD PTR [rip+0x0]     # RELOCATION
      5cf: lea    rbx,[rax+0x1]
      5d3: jmp    QWORD PTR [rbp+0x0]
      
Relocation section '.rela.text' at offset 0xd50 contains 39 entries:
      Offset       Sym. Name + Addend
====> 0000000005cb ghczmprim_GHCziTuple_Z - 4
{% endhighlight %}

You can write specialized versions like above, or use the [`SPECIALIZE` pragma](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#specialize-pragma) to generate them. Using a separate specialized function may make it easier to see that specialization is happening, however.

## Conclusion

Both abstract classes and typeclasses carry along an implicit array of function pointers. C++ generates this array at compile-time, while Haskell generates it at runtime. Once generated, it's passed along as a simple pointer that generic code can index into to find an overridable implementation for a function.

Future programming language implementation articles may cover:
* Multi-parameter typeclasses, typeclass hierarchies, and typeclasses over recursive higher-order types
* Dependent types
* JIT compilation
* Garbage collectors
* Type-checking
