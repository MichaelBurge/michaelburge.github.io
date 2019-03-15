---
layout: post
author: Michael Burge
title: "Implementing a NES Emulator in Rust"
started_date: 2019-03-06 17:34:00
date: 2019-03-06 17:34:00
tags:
  - rust
  - nes
  - emulator
---

Recently, I made an [emulator](https://github.com/MichaelBurge/nes-emulator) for the **Nintendo Entertainment Console**(NES) - a game console first released in 1983.

In this article, I'll talk about how I used [Rust](https://www.rust-lang.org/) to develop the emulator. I'll cover:

* Result: What features does the emulator support? What games can it play?
* Problem Domain: How did I approach the problem of emulating the NES?
* Language: Did Rust's type system or borrow checker interfere? Were there performance issues?

## Result

Super Mario Bros is beatable on my emulator:
#### TODO - Include a video of Super Mario Bros

Features:
* Runs at a stable 60 FPS(can go up to 430 FPS in "headless" mode)
* Use an Xbox 360 controller for game input
* Savestates allow you to save and load the game at any time
* Video recording remembers and plays back controller inputs.

Games tested:
* Donkey Kong
* Super Mario Bros

A few remaining tasks would make the emulator much more widely usable:
* Support for more [mappers](https://wiki.nesdev.com/w/index.php/Mapper) will allow it to play more games
* The keyboard should be usable as an input device

I think Rust was a fine language choice for my emulator. People frequently want to run emulators on [strange embedded systems](https://arstechnica.com/gaming/2018/07/nintendo-hid-a-load-your-own-nes-emulator-inside-a-gamecube-classic/), and C is probably still a better choice for those cases.

But Rust seems usable in any project where C++ is a viable language. Using features like iterators and traits didn't slow down the program.

## Problem Domain

I emulated the NES by emulating its individual hardware components. These include:

* [MOS Technology 6502](https://en.wikipedia.org/wiki/MOS_Technology_6502) CPU
* Custom [Picture Processing Unit(PPU)](https://wiki.nesdev.com/w/index.php/PPU)
* Custom [Audio Processing Unit(APU)](https://wiki.nesdev.com/w/index.php/APU)
* Writable memory(RAM) or read-only memory(ROM)
* [Controller](https://wiki.nesdev.com/w/index.php/Standard_controller)
* Cartridges with [custom circuitry](https://wiki.nesdev.com/w/index.php/Mapper)

These components are either **Clocked** or are mapped to one of two **Address Spaces**. I define each component as a [C Struct](https://doc.rust-lang.org/rust-by-example/custom_types/structs.html), and use Rust's [Traits](https://doc.rust-lang.org/rust-by-example/trait.html) to specify which of these two ways the component is used in the overall system.

For video/audio/controller IO with the host OS, I used the [SDL](https://github.com/Rust-SDL2/rust-sdl2) library.

Here is a table with all structures in my program:
| Structure            | Clocked? | Address Space? | Description |
| ---                  | ---      | ---            | ---         |
| Nes                  | T        | F              | Top-level hardware component. Has all others(except game controllers) as members |
| C6502                | T        | T              | The CPU |
| Ppu                  | T        | T              | Produces a 256x240 pixel display |
| PpuRegisters         | F        | F              | Hides some tricky internal PPU state |
| PaletteControl       | F        | T              | Used in the PPU. Stores which 13 colors have been chosen of 64 possible |
| CpuPpuInterconnect   | F        | T              | Maps certain PPU registers to CPU address space |
| Apu                  | T        | T              | Generates audio samples |
| Ram                  | F        | T              | A fixed-size block of readable and writable memory |
| Rom                  | F        | T              | A fixed-size block of read-only memory |
| MirroredAddressSpace | F        | T              | Makes another `AddressSpace` appear in multiple regions. See [Memory Mirroring](https://wiki.nesdev.com/w/index.php/Mirroring#Memory_Mirroring) |
| NullAddressSpace     | F        | T              | Represents unmapped address space. Returns 0 when read, and does nothing on write. |
| Mapper               | F        | T              | Splits the address space into regions, which are assigned to other `AddressSpace` types |
| Joystick             | F        | T              | Communicates controller inputs to the CPU |
| Ines                 | F        | F              | A game cartridge, represented in [iNES](https://wiki.nesdev.com/w/index.php/INES) format |

In this section, I'll talk about the most important of these, and each of the traits that link them together.

### Clocked

{% highlight rust %}
pub trait Clocked {
    fn clock(&mut self);
}

{% endhighlight %}

A clock cycle is the smallest discrete step that a device can take, and all of the device's work should be done faster than the devices **Clock Speed**.

The CPU is an example of a clocked component. A single CPU instruction might:
* Request an 8-bit value at an address
* Attempt to store an 8-bit value at an address
* Calculate an addition using the Arithmetic Logic Unit(ALU)

A clock-accurate emulation of a clocked component is the most accurate[1], but programmers - even rugged ones in the 1980s - generally don't depend on the clock-by-clock details of the CPU. They only use that e.g. a bitwise AND instruction takes 6 clock cycles, but not that it does a memory read on clock #2 and a memory write on clock #6. So it can be an acceptable loss of accuracy to run the entire CPU instruction in one clock cycle, and then do nothing for the next 5.

The NES has a **Master Clock**, and all other Clocked components run at an integer fraction of its speed:
| Component | Clock Speed |
| Master | 1:1 = 21.477272 Mhz |
| CPU | 1:12 |
| PPU | 1:4 |
| APU | 1:24 |

{:.spoiler}
```
There are [digital logic simulators](https://wiki.nesdev.com/w/index.php/Visual_2C02) created from special photographs of the CPU. These are not more accurate than a correctly-implemented cycle-accurate emulator, but are useful debugging tools to determine what the correct behavior should be. There are [rare cases](http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_%28XAA,_ANE%29) where even this is not enough to explain the precise behavior of the CPU, but no published NES games make use of this detail.
```

### Address Space

When a component wants to read or write a value at an address, it places the address on an **Address Bus**. Other components listen for specific addresses that they have been assigned.

An Address Space is an assignment of addresses to actions that components take.

{% highlight rust %}
pub trait AddressSpace {
    fn peek(&mut self, ptr: u16) -> u8;
    fn poke(&mut self, ptr: u16, value: u8);
}
{% endhighlight %}

The NES has two different address buses, primarily used by the CPU and PPU respectively:
* [CPU Memory Map](https://wiki.nesdev.com/w/index.php/CPU_memory_map)
* [PPU Memory Map](https://wiki.nesdev.com/w/index.php/PPU_memory_map)

The CPU mostly handles game logic, while the PPU's address space stores sprites, tiles, and colors.

Cartridges listen on both address buses. They are not a simple list of bytes, since they can have arbitrary circuitry embedded within them. Games can choose to include extra RAM, coprocessors for specialized calculations, or control registers for changing the address space.

For example, Super Mario Bros 3 alternates between two different tilemaps when a control register is written, which animates the background.

#### TODO - Include screenshot of Super Mario Bros 3 using bank-switching to animate background

### CPU

The CPU is the most important component, because it has the most variety of responsibilities.

It repeatedly fetches, decodes, and executes an instruction located at the current **Program Counter** - a pointer in CPU address space - which is then incremented to the next instruction.

The CPU can only perform three actions:
* Request a read(in CPU address space)
* Request a write
* Change its internal registers

All instructions are combinations of these primitive actions. For example, the [Arithmetic Shift Left, with Absolute,X addressing mode](http://obelisk.me.uk/6502/reference.html) instruction(`0x1E`) takes 7 clock cycles and causes the CPU to perform the following 7 actions:
* (1) Fetch the opcode byte `0x1E` at the current program counter
* (2,3) Fetch a two-byte value `V` immediately after the opcode byte containing a 16-bit address
* (4) Adds the `X` register to `V`
* (5) Fetch a one-byte value `a` located at address `X+V`.
* (6) Calculate the value `b = a << 1`, updating several status flags
* (7) Write the value `b` to address `X+V`.

It's a complex operation, but the individual steps are simple: 4 memory fetches, 2 calculations using the ALU, and 1 memory write.

I implemented each instruction in terms of the three primitive operations, generated an instruction-by-instruction log using the [`nestest`](https://wiki.nesdev.com/w/index.php/Emulator_tests) test ROM, and verified that it matched the reference log.

### PPU

At 60 Frames per Second(FPS) the NES' CPU can execute `29780` clock cycles per frame(for NTSC video), but there are `61440` different pixels to draw. So the CPU is too slow to draw even a blank screen.

A separate "Picture Processing Unit" runs at triple the clock speed, emitting one pixel per cycle. There are more cycles available than pixels to draw, so the idle time("Vertical Blank") is used by the CPU to configure the PPU for the next frame.

The PPU mainly draws two things: Background tiles and sprites. Up to 32 x 30 tiles and 64 sprites can be displayed at once, and these share a pool of 512 different 8x8 patterns.

There are 4 different tables that store tile and sprite information:
* Nametable: A table of bytes which each specify an 8x8 pattern
* Attribute table: Specifies which palette is used for each 16x16 group of tiles
* Object Attribute Memory(OAM): Stores the position, palette, and status of 64 sprites
* Palette: There are 4 different palettes, which each choose 3 of 64 different colors.

Even though they use separate address spaces, there are three communication channels between the PPU and CPU:
* There are 8 PPU registers mapped in the CPU's address space.
* The PPU triggers two CPU interrupts: End-of-scanline and Vertical Blank.
* The cartridge itself can choose to modify its assigned PPU address space based on reads or writes to the CPU address space.

There are 5 primitive actions that the PPU can take on each clock cycle:
* Request a read( in PPU address space)
* Request a write
* Change its internal registers
* Emit a single pixel as an NTSC or PAL video signal
* Trigger a CPU interrupt

There are actually two slightly different PPUs, depending on the region the NES was sold in: NTSC and PAL.

It is more difficult to test the PPU than the CPU: Much of the PPU must already be functioning in order to get any output at all. I recommend printing out the 4 tables separately - and once that data is confirmed correct, then test whether these are combined correctly for any particular pixel.

### Save States

A common feature in emulators is to save and load a game at any point. Every component implements a `Savable` trait that replaces its data with incoming data from a filehandle:

{% highlight rust %}
pub trait Savable {
    fn save(&self, fh: &mut Write);
    fn load(&mut self, fh: &mut Read);
}
{% endhighlight %}

There are two properties that make this model useful:
* My emulator doesn't allocate memory, so every object can be written in-place.
* Loading a ROM always creates the same types of components, so the values saved from a previous program execution can be assumed to be compatible.

The first property guarantees that pointers to existing components aren't affected by a savestate restore. If a component is dynamically-allocated and a savestate from before or after its lifetime is loaded, then any pointers to it would need to be changed. This would require keeping track of which objects have pointers to which other objects.

If this second property failed(say, if today the NES has a standard NES controller, but tomorrow it has a SNES controller), then the savestate files would need to include type information so the correct `load` method is called.

However, given these restrictions, every NES component has very straightforward serialization code:

{:.spoiler}
{% highlight rust %}
impl Savable for C6502 {
    fn save(&self, fh: &mut Write) {
        self.acc.save(fh);
        self.x.save(fh);
        self.y.save(fh);
        self.pc.save(fh);
        self.sp.save(fh);
        self.carry.save(fh);
        self.zero.save(fh);
        self.interruptd.save(fh);
        self.decimal.save(fh);
        self.overflow.save(fh);
        self.negative.save(fh);
        self.mapper.save(fh);
        self.counter.save(fh);
        self.clocks.save(fh);
        self.is_tracing.save(fh);
        self.clocks_to_pause.save(fh);
    }
    fn load(&mut self, fh: &mut Read) {
        self.acc.load(fh);
        self.x.load(fh);
        self.y.load(fh);
        self.pc.load(fh);
        self.sp.load(fh);
        self.carry.load(fh);
        self.zero.load(fh);
        self.interruptd.load(fh);
        self.decimal.load(fh);
        self.overflow.load(fh);
        self.negative.load(fh);
        self.mapper.load(fh);
        self.counter.load(fh);
        self.clocks.load(fh);
        self.is_tracing.load(fh);
        self.clocks_to_pause.load(fh);
    }
}
{% endhighlight %}

The primitives like `bool` or `u32` are similarly pretty simple:

{% highlight rust %}
impl Savable for u32 {
    fn save(&self, fh: &mut Write) {
        let bytes = [
            ((*self >> 0 ) & 0xff) as u8,
            ((*self >> 8 ) & 0xff) as u8,
            ((*self >> 16) & 0xff) as u8,
            ((*self >> 24) & 0xff) as u8,
        ];
        fh.write_all(&bytes);
    }
    fn load(&mut self, fh: &mut Read) {
        let mut bytes = [0u8; 4];
        fh.read_exact(&mut bytes);
        *self = 0;
        *self |= (bytes[0] as u32) << 0;
        *self |= (bytes[1] as u32) << 8;
        *self |= (bytes[2] as u32) << 16;
        *self |= (bytes[3] as u32) << 24;
    }
}
{% endhighlight %}

I think Rust's traits were particularly useful for implementing savestates. The compiler infers which `save` or `load` methods are needed for each type, so the code looks very uniform.

The video playback feature builds on top of savestates by storing 1 byte per frame for inputs. When a savestate is restored, it also restores the input list.

## Language

Having modeled the NES at a relatively high-level, it's worth asking if the Rust language gave any problems when this was translated into code.

### Integer overflow

By default, Rust(with the `debug` flag enabled) will throw an exception if any arithmetic operation overflows. This actually caught a fair number of bugs during testing, because the [wrapping_*](https://doc.rust-lang.org/std/primitive.u32.html#method.wrapping_add) versions require explicit type information and it's important whether a number wraps as a 16-bit value or an 8-bit value.

I used the `wrapping_*` functions in my emulator, but there are also [Wrapping](https://doc.rust-lang.org/std/num/struct.Wrapping.html) types that imply wrapping arithmetic operations.

### Single-ownership

In Rust, mutable values must have a single owning variable. Other variables can **borrow** it, but only one mutable reference is allowed at a single time.

The CPU address space has several PPU registers mapped. So the CPU must have a mutable reference to the PPU, but because both are sibling components neither owns the other.

I worked around this by using [Box](https://doc.rust-lang.org/std/boxed/struct.Box.html) to assign fixed memory addresses to every value, and then using "unsafe" pointer dereferences when needed.

{:.spoiler}
{% highlight rust %}
pub struct Nes {
    pub cpu: Box<C6502>,
    pub apu: Box<Apu>,
    pub ppu: Box<Ppu>,
}

pub struct CpuPpuInterconnect {
    ppu: *mut Ppu,
    cpu: *mut C6502,
}

impl AddressSpace for CpuPpuInterconnect {
    fn peek(&mut self, ptr:u16) -> u8 {
        let ppu:&mut Ppu = unsafe { &mut *self.ppu };
        // Games aren't supposed to read some of these, but
        // if they do, the "open bus" is whatever value was last written
        // to any PPU register.
        match map_ppu_port(ptr) {
            Some(PPUCTRL)   => ppu.open_bus,
            Some(PPUMASK)   => ppu.open_bus,
            Some(PPUSTATUS) => ppu.read_status(),
            Some(OAMADDR)   => ppu.open_bus,
            Some(OAMDATA)   => ppu.read_oam_data(),
            Some(PPUSCROLL) => ppu.open_bus,
            Some(PPUADDR)   => ppu.open_bus,
            Some(PPUDATA)   => ppu.read_data(),
            Some(OAMDMA)    => ppu.open_bus,
            port            => panic!("INVALID PPU PORT READ {:?} {:x}", port, ptr),
        }
    }
    fn poke(&mut self, ptr:u16, value:u8) {
        let ppu:&mut Ppu = unsafe { &mut *self.ppu };
        ppu.open_bus = value;
        match map_ppu_port(ptr) {
            Some(PPUCTRL)   => ppu.write_control(value),
            Some(PPUMASK)   => ppu.write_mask(value),
            Some(PPUSTATUS) => {},
            Some(OAMADDR)   => ppu.write_oam_address(value),
            Some(OAMDATA)   => ppu.write_oam_data(value),
            Some(PPUSCROLL) => ppu.write_scroll(value),
            Some(PPUADDR)   => ppu.write_address(value),
            Some(PPUDATA)   => ppu.write_data(value),
            Some(OAMDMA)    => {
                let cpu = unsafe { &mut *self.cpu };
                let ptr_base = (value as u16) << 8;
                for i in 0..=255 {
                    let addr = ptr_base + i;
                    let v = cpu.peek(addr);
                    ppu.oam[ppu.oam_ptr as usize] = v;
                    ppu.oam_ptr = ppu.oam_ptr.wrapping_add(1);
                }
            }
            port => panic!("INVALID PPU PORT WRITE {:?} {:x} {:x}", port, ptr, value),
        }
    }
}
{% endhighlight %}

This use of `unsafe` means that my emulator is not thread-safe. If both the PPU and CPU ran in separate threads, they could both issue writes to the same address or read a value while it is being written.

If the `unsafe` blocks are undesirable, a [Mutex](https://doc.rust-lang.org/beta/book/ch16-03-shared-state.html) might be a good solution here. The lock would be held for a relatively small amount of time, so it shouldn't cause much of a performance issue.

### Mutable references

A mutable reference to a variable creates a sort of exclusive lock against any other references. There are certain optimizations that can be made if

{% highlight rust %}
'a: {
    let foo = Box::new(5);
    let pfoo:*mut foo = &mut foo;
    consume_ownership(*foo);
}
{% endhighlight %}

### Performance

In a later article, I plan to train a Reinforcement Learning(RL) agent to play Mario. A faster emulator allows me to gather more sample data, so I spent a few hours ensuring my emulator was comparable to other NES emulators used for RL research.

My benchmark runs 10,000 frames of Mario in a "headless" mode with no video or sound, and then writes a screenshot.

Before I started optimizing it, my emulator got about 350 frames-per-second(FPS). I saw RL people getting between 200 and 450 FPS on various emulators, so I made a few small changes and ended up at about 430 FPS.

I found three optimizations, which each gave a 10% FPS increase.

#### Division

Compilers try to avoid emitting division instructions, since they are slow. However, division by a non-constant defeats many of the optimizations it uses.

The code

{% highlight rust %}
impl Clocked for FrameCounter {
    fn clock(&mut self) {
        self.step += 1;
        let cap = if self.mode { 18641 } else { 14915 };
        self.step %= cap;
    }
}
{% endhighlight %}

compiles to

{% highlight assembly %}
add          $0x1, %eax         ; self.step += 1
cmpb         $0x0, 0x33(%rbx)   ; self.mode
mov          $0x3a43,%ecx       ; 14915
mov          $0x46d1,%edi       ; 18641
cmove        %ecx,%edi          ; let cap = if self.mode ...
xor          %edx,%edx
div          %di                ; self.step %= cap
{% endhighlight %}

. Since the `FrameCounter` is clocked every other CPU cycle, it executes about `14914` `div` instructions per NES frame, or about 6.4 million per second at the accelerated rate of 430 FPS.

Given the following two conditions:
* The dividend never decreases
* The dividend never increases by more than the divisor between divisions

The above code can be replaced with a conditional subtraction:

{% highlight rust %}
impl Clocked for FrameCounter {
    fn clock(&mut self) {
        self.step += 1;
        let cap = if self.mode { 18641 } else { 14915 };
        if self.step >= cap {
            self.step -= cap;
        }
    }
}
{% endhighlight %}

The branch is only taken once every 15,000 clocks, so this bottleneck is completely removed and the emulator as a whole processes 10% more frames.

#### Iterators

Consider the following two implementations of the same function:
{% highlight rust %}
#![crate_type="lib"]
#![no_std]

#[no_mangle]
pub fn foo(xs: &[usize]) -> usize {
    let mut acc:usize = 0;
    for (x, idx) in xs.iter().zip(0..xs.len()) {
        acc = acc.wrapping_add(*x);
        acc = acc.wrapping_mul(idx);
    }
    return acc;
}

#[no_mangle]
pub fn bar(xs: &[usize]) -> usize {
    let mut acc:usize = 0;
    for idx in 0..xs.len() {
        let x = xs[idx];
        acc = acc.wrapping_add(x);
        acc = acc.wrapping_mul(idx);
    }
    return acc;
}

{% endhighlight %}

The function `foo` is a fairly direct translation of the original source:

{:.spoiler}
{% highlight assembly %}
0000000000000000 <foo>:
   0:   48 85 f6                test   rsi,rsi
   3:   74 33                   je     38 <foo+0x38>
   5:   48 8d 0c f5 00 00 00    lea    rcx,[rsi*8+0x0]
   c:   00
   d:   31 c0                   xor    eax,eax
   f:   31 d2                   xor    edx,edx
  11:   66 2e 0f 1f 84 00 00    nop    WORD PTR cs:[rax+rax*1+0x0]
  18:   00 00 00
  1b:   0f 1f 44 00 00          nop    DWORD PTR [rax+rax*1+0x0]
  20:   48 39 f2                cmp    rdx,rsi
  23:   73 12                   jae    37 <foo+0x37>
  25:   48 03 04 d7             add    rax,QWORD PTR [rdi+rdx*8]
  29:   48 0f af c2             imul   rax,rdx
  2d:   48 8d 52 01             lea    rdx,[rdx+0x1]
  31:   48 83 c1 f8             add    rcx,0xfffffffffffffff8
  35:   75 e9                   jne    20 <foo+0x20>
  37:   c3                      ret
  38:   31 c0                   xor    eax,eax
  3a:   c3                      ret
{% endhighlight %}

The variables are mapped as follows:
* `rcx` is the remaining number of bytes in the slice `xs`
* `rdx` is the variable `idx`
* `rax` is the variable `acc`
* `rdi` points to the beginning of the data in `xs`

And the loop executes `add` and `imul` instructions until `rcx` is 0.

However, the function `bar` unrolls its loop 4 times:

{:.spoiler}
{% highlight assembly %}
0000000000000000 <bar>:
   0:   48 85 f6                test   rsi,rsi
   3:   74 1c                   je     21 <bar+0x21>
   5:   48 8d 46 ff             lea    rax,[rsi-0x1]
   9:   41 89 f0                mov    r8d,esi
   c:   41 83 e0 03             and    r8d,0x3
  10:   48 83 f8 03             cmp    rax,0x3
  14:   73 0e                   jae    24 <bar+0x24>
  16:   31 c0                   xor    eax,eax
  18:   31 d2                   xor    edx,edx
  1a:   4d 85 c0                test   r8,r8
  1d:   75 4e                   jne    6d <bar+0x6d>
  1f:   eb 60                   jmp    81 <bar+0x81>
  21:   31 c0                   xor    eax,eax
  23:   c3                      ret
  24:   4c 29 c6                sub    rsi,r8
  27:   31 c0                   xor    eax,eax
  29:   31 d2                   xor    edx,edx
  2b:   0f 1f 44 00 00          nop    DWORD PTR [rax+rax*1+0x0]
  30:   48 03 04 d7             add    rax,QWORD PTR [rdi+rdx*8]
  34:   48 0f af c2             imul   rax,rdx
  38:   48 03 44 d7 08          add    rax,QWORD PTR [rdi+rdx*8+0x8]
  3d:   48 8d 4a 01             lea    rcx,[rdx+0x1]
  41:   48 0f af c1             imul   rax,rcx
  45:   48 03 44 d7 10          add    rax,QWORD PTR [rdi+rdx*8+0x10]
  4a:   48 8d 4a 02             lea    rcx,[rdx+0x2]
  4e:   48 0f af c1             imul   rax,rcx
  52:   48 03 44 d7 18          add    rax,QWORD PTR [rdi+rdx*8+0x18]
  57:   48 8d 4a 03             lea    rcx,[rdx+0x3]
  5b:   48 8d 52 04             lea    rdx,[rdx+0x4]
  5f:   48 0f af c1             imul   rax,rcx
  63:   48 39 d6                cmp    rsi,rdx
  66:   75 c8                   jne    30 <bar+0x30>
  68:   4d 85 c0                test   r8,r8
  6b:   74 14                   je     81 <bar+0x81>
  6d:   49 f7 d8                neg    r8
  70:   48 03 04 d7             add    rax,QWORD PTR [rdi+rdx*8]
  74:   48 0f af c2             imul   rax,rdx
  78:   48 8d 52 01             lea    rdx,[rdx+0x1]
  7c:   49 ff c0                inc    r8
  7f:   75 ef                   jne    70 <bar+0x70>
  81:   c3                      ret
{% endhighlight %}

There are two loops:
* The loop beginning at `30:` processes 4 elements from `xs`
* The loop beginning at `70:` processes one element from `xs`

The variables are assigned as follows:

* `r8d` contains the remaining number of elements to process after the `30:` loop has finished.
* `rax` is the `acc` variable
* `rdi` points to the beginning of the data for `xs`
* `rdx` is the `idx` variable, updated once per loop iteration
* `rcx` is also the `idx` variable, but updated more frequently in the unrolled `30:` loop.
* `rsi` points to the first data that the `30:` loop is unable to process.

The main difference seems to be that `foo` counts bytes while `bar` counts elements. If I had to guess, I would say `rustc` makes an early determination to count bytes rather than elements, because the tuple `(x, idx)` has size 16 bytes and the addressing mode `QWORD PTR [rdi+rdx*8+C]` has a maximum scale of `8`.

In any case, changing one of my loops to use `bar`'s style improved the emulator's FPS by 10%.

#### Search order

Recall that the CPU Address Space consists of many components which each listen for reads and writes to specific addresses.

The **Mapper** takes an address and does a linear search through all of them looking for the responsible component.

When I first wrote the code, I mapped the address space from first-to-last:
{% highlight rust %}
fn map_nes_cpu(&mut self, joystick1: Box<AddressSpace>, joystick2: Box<AddressSpace>, cartridge: Box<AddressSpace>) {
    let mut mapper:Mapper = Mapper::new();
    let cpu_ram:Ram = Ram::new(0x800);
    let cpu_ppu:CpuPpuInterconnect = CpuPpuInterconnect::new(self.ppu.deref_mut(), self.cpu.deref_mut());
    let apu = self.apu.deref_mut() as *mut Apu;
    // https://wiki.nesdev.com/w/index.php/CPU_memory_map
    mapper.map_mirrored(0x0000, 0x07ff, 0x0000, 0x1fff, Box::new(cpu_ram), false);
    mapper.map_mirrored(0x2000, 0x2007, 0x2000, 0x3fff, Box::new(cpu_ppu), true);
    mapper.map_address_space(0x4000, 0x4013, Box::new(apu), true);;
    mapper.map_address_space(0x4014, 0x4014, Box::new(cpu_ppu), true);
    mapper.map_address_space(0x4015, 0x4015, Box::new(apu), true);
    mapper.map_address_space(0x4016, 0x4016, joystick1, false);
    mapper.map_address_space(0x4017, 0x4017, Box::new(apu), true); // TODO - 0x4017 is also mapped to joystick2

    mapper.map_null(0x4018, 0x401F); // APU test mode
    mapper.map_address_space(0x4020, 0xFFFF, cartridge, true);

    self.cpu.mapper = Box::new(mapper);
    self.cpu.initialize();
}
{% endhighlight %}

However, this puts the `cartridge` at the very end of the linear search. The cartridge contains all of the game's code, so every new instruction fetch was causing a worst-case linear-search.

I experimented with an LRU cache, but found the best improvement by simply reordering those statements to put the most frequently-accessed components first.

This also gave a 10% improvement in FPS.

## Conclusion

If you're interested in NES development, you might like these links:

References:

* [Nesdev Wiki](https://wiki.nesdev.com/w/index.php/Nesdev_Wiki)
* [Reverse Emulation](https://www.youtube.com/watch?v=ar9WRwCiSr0) shows how cartridges can even have a modern processor in them.

Future articles may cover:
* Improving the performance of the emulator
* Training Neural Networks to play Mario
* Developing emulators for other systems
* Compiling the emulator to Javascript to run in-browser
* Compatibility issues with less-common games
* How specific NES games worked internally
* Porting the emulator to run on a GPU[2]
* Writing an NES game in Rust
* Making new optimization passes for `rustc` or LLVM
* Accelerating an emulator with a JIT compiler

{:.spoiler}```My emulator only depends on SDL, and the headless version might not need the standard library. So it should be possible to port it to [exotic platforms](https://www.michaelburge.us/2017/09/10/injecting-shellcode-to-speed-up-amazon-redshift.html).```