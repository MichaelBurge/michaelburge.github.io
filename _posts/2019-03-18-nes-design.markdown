---
layout: post
author: Michael Burge
title: "Implementing a NES Emulator in Rust"
started_date: 2019-03-06 17:34:00
date: 2019-03-18 13:34:00
tags:
  - rust
  - nes
  - emulator
---

Recently, I made an [emulator](https://github.com/MichaelBurge/nes-emulator) for the **Nintendo Entertainment Console**(NES) - a game console first released in 1983.

In this article, I'll talk about how I used [Rust](https://www.rust-lang.org/) to develop the emulator. I'll cover questions like:

* What features does the emulator support? What games can it play?
* How did I approach the problem of emulating the NES?
* Did Rust's type system or borrow checker interfere? Were there performance issues?

<!-- -->

Table of Contents:
* This list is replaced with the Table of Contents during page generation
{:toc}


## Result

Super Mario Bros is beatable on my emulator:
<iframe width="560" height="315" src="https://www.youtube.com/embed/PiHsOFmj8ts" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

Features:
* Runs at a stable 60 FPS(can go up to 430 FPS in "headless" mode)
* Use an Xbox 360 controller for game input
* Savestates allow you to save at any time. Impress your friends with flawless Goomba-stomping
* Video recording works with savestates to remember a single chain of controller inputs.

Games tested:
* Donkey Kong
* Super Mario Bros

There are a few remaining tasks that would make it more usable:
* Support for more [mappers](https://wiki.nesdev.com/w/index.php/Mapper) will allow it to play more games
* The keyboard should be usable as an input device
* The audio sounds different, though I don't know the words to describe the problem

Rust seems like it was a fine choice for this project. Using features like iterators and traits generally didn't slow down the program(though see below for one exception). Since an NES is a fixed hardware device, no dynamic allocation[^3] should be necessary and Rust makes it easy to reason about already-allocated memory with its ownership model.

People frequently want to run emulators on [strange embedded systems](https://arstechnica.com/gaming/2018/07/nintendo-hid-a-load-your-own-nes-emulator-inside-a-gamecube-classic/). C is probably still a better choice for those cases: It's more difficult to find someone who can implement a Rust compiler than a C compiler.

But Rust seems usable in any project where C++ is a viable language.

## Problem Domain

I emulated the NES mostly by emulating each individual component separately. These include:

* [MOS Technology 6502](https://en.wikipedia.org/wiki/MOS_Technology_6502) CPU
* Custom [Picture Processing Unit(PPU)](https://wiki.nesdev.com/w/index.php/PPU)
* Custom [Audio Processing Unit(APU)](https://wiki.nesdev.com/w/index.php/APU)
* Writable memory(RAM) or read-only memory(ROM)
* [Controller](https://wiki.nesdev.com/w/index.php/Standard_controller)
* Cartridges with [custom circuitry](https://wiki.nesdev.com/w/index.php/Mapper)

These components are either **Clocked** or are mapped to one of two **Address Spaces**. I define each component as a [C Struct](https://doc.rust-lang.org/rust-by-example/custom_types/structs.html), and implement one of two [Traits](https://doc.rust-lang.org/rust-by-example/trait.html) to specify how it interacts with other components.

For video/audio/controller IO with the host OS, I used the [SDL](https://github.com/Rust-SDL2/rust-sdl2) library.

Here is a table with all structures in my program. They generally correspond to actual hardware components like RAM or internal timers.

{::options parse_block_html="true" /}
{:.spoiler}
<div>

| Structure Name       | Component | Clocked? | Address Space? | Description |
| :---:                | :---:      | :---:            | :---:         |
| [Nes](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/nes.rs#L27)                  | | T        | F              | Top-level hardware component. Has all others as members |
| [C6502](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/c6502.rs#L27)                | CPU | T        | T              | The CPU |
| [Ppu](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/ppu.rs#L44)                  | PPU | T        | T              | Produces a 256x240 pixel display |
| [PpuRegisters](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/ppu.rs#L163)         | PPU | F        | F              | Hides some tricky internal PPU state |
| [PaletteControl](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/ppu.rs#L534)       | PPU | F        | T              | Stores which 13 colors have been chosen of 64 possible |
| [CpuPpuInterconnect](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/ppu.rs#L452)   | PPU | F        | T              | Maps certain PPU registers to CPU address space |
| [Sprite](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/ppu.rs#L419) | PPU | F | F | Represents a 4-byte entry in the OAM table |
| [Apu](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/apu.rs#L116)                  | APU | T        | T              | Generates audio samples |
| [Frame Counter](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/apu.rs#L61) | APU | T | F | Generates a clock signal every quarter or half frame that other audio components react to. |
| [Length Counter](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/apu.rs#L273) | APU | T | F | Silences an audio channel after a certain number of clock cycles. |
| [Linear Counter](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/apu.rs#L322) | APU | T | F | Silences an audio channel according to a timer. Has slightly different timing than the length counter |
| [Triangle](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/apu.rs#L371) | APU | T | F | Audio channel for a triangle wave |
| [Sweep](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/apu.rs#L451) | APU | T | F | Dynamically changes the pitch of an audio channel |
| [Envelope](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/apu.rs#L542) | APU | T | F | Dynamically changes the volume of an audio channel |
| [Pulse](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/apu.rs#L603) | APU | T | F | Audio channel for a pulse/square wave |
| [Noise](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/apu.rs#L708) | APU | T | F | Audio channel for pseudo-random noise |
| [Dmc](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/apu.rs#L783) | APU | T | F | Audio channel for premade audio samples |
| [Ram](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/mapper.rs#L36)                  | | F        | T              | A fixed-size block of readable and writable memory |
| [Rom](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/mapper.rs#L82)                  | | F        | T              | A fixed-size block of read-only memory |
| [MirroredAddressSpace](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/mapper.rs#L112) | | F        | T              | Makes another `AddressSpace` appear in multiple regions. See [Memory Mirroring](https://wiki.nesdev.com/w/index.php/Mirroring#Memory_Mirroring) |
| [NullAddressSpace](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/mapper.rs#L164) | | F        | T              | Represents unmapped address space. Returns 0 when read, and does nothing on write. |
| [Mapper](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/mapper.rs#L214) | | F        | T              | Splits the address space into regions, which are assigned to other `AddressSpace` types |
| [Joystick](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/joystick.rs#L14) | Input | F        | T              | Communicates controller inputs to the CPU |
| [Ines](https://github.com/MichaelBurge/nes-emulator/blob/ce768d7b090688a68f4ef732f9d1f18f1d29542a/src/nes.rs#L52) | Cartridge | F | F              | A game cartridge, represented in [iNES](https://wiki.nesdev.com/w/index.php/INES) format |

</div>

In this section, I'll talk about the most important of these, and each of the traits that link them together.

### Clocked

{% highlight rust %}
pub trait Clocked {
    fn clock(&mut self);
}

{% endhighlight %}

A clock cycle is the smallest discrete step that a component can make: There should be no observable changes outside of a clock cycle.

The CPU is an example of a clocked component. A single CPU instruction might:
* Request an 8-bit value at an address
* Attempt to store an 8-bit value at an address
* Calculate an addition using the Arithmetic Logic Unit(ALU)

Generally, an emulation that does more work at once is faster than a component that simulates each individual clock cycle.

A clock-accurate emulation of a clocked component is the most accurate[^1], but programmers - even rugged ones in the 1980s - generally don't depend on the clock-by-clock details of the CPU. It's common to assume that a bitwise AND instruction takes 6 clock cycles, but not that it does a memory read on clock #2 and a memory write on clock #6. So it can be an acceptable loss of accuracy to run the entire CPU instruction in one clock cycle, and then do nothing for the next 5.

The NES has a **Master Clock**, and all other Clocked components run at an integer fraction of its speed:

| Component | Clock Speed |
| --- | --- |
| Master | 1:1 = 21.477272 Mhz |
| CPU | 1:12 |
| PPU | 1:4 |
| APU | 1:24 |

The APU has components that act on two separate clock signals: One from the APU clock, and one from an internal `FrameCounter` that sends a signal every half or quarter frame.

### Address Space

When a component wants to read or write a value at an address, it places the address on an **Address Bus**. There is a separate **Data Bus** that holds the value being read or written. Other components listen for specific addresses that they have been assigned.

An emulated Address Space is an assignment of addresses to actions that components take.

{% highlight rust %}
pub trait AddressSpace {
    fn peek(&mut self, ptr: u16) -> u8;
    fn poke(&mut self, ptr: u16, value: u8);
}
{% endhighlight %}

The NES has two different address spaces, primarily used by the CPU and PPU respectively:
* [CPU Memory Map](https://wiki.nesdev.com/w/index.php/CPU_memory_map)
* [PPU Memory Map](https://wiki.nesdev.com/w/index.php/PPU_memory_map)

The CPU mostly handles game logic, while the PPU's address space stores sprites, tiles, and colors.

Cartridges listen on both address buses. They are not a simple ROM holding a list of bytes, since they can have arbitrary circuitry[^4]. Games can choose to include extra RAM, coprocessors for specialized calculations, or control registers for changing the address space.

Super Mario Bros 3 animates its background by telling the cartridge to suddenly switch between two different background tile patterns. When the PPU reads from the relevant addresses, the cartridge suddenly starts returning different tile data. Its otherwise not possible to update the entire background in a single frame.

![Bank Switching](/assets/articles/20190318-nes-emulator/bankswitch.gif)

(Original image from [this article](https://n3s.io/index.php?title=How_It_Works))
{: style="text-align: center;"}

### CPU

The CPU handles the game logic: What happens when Mario jumps, stomps on a Goomba, or falls in a pit?

It repeatedly fetches, decodes, and executes an instruction at the current **Program Counter** - a pointer in CPU address space - which is then incremented to the next instruction.

The CPU can only perform three primitive actions:
* Request a read(in CPU address space)
* Request a write
* Change its internal registers

All instructions are combinations of these. The [Arithmetic Shift Left, with Absolute,X addressing mode](http://obelisk.me.uk/6502/reference.html) instruction(`0x1E`) takes 7 clock cycles and causes the CPU to perform the following 7 actions[^5]:
* (1) Fetch the opcode byte `0x1E` at the current program counter
* (2,3) Fetch a two-byte value `V` immediately after the opcode byte containing a 16-bit address
* (4) Adds the `X` register to `V`
* (5) Fetch a one-byte value `a` located at address `X+V`.
* (6) Calculate the value `b = a << 1`, updating several status flags
* (7) Write the value `b` to address `X+V`.

It's a complex operation, but the individual steps are simple: 4 memory fetches, 2 calculations using the ALU, and 1 memory write.

I implemented each instruction in terms of the three primitive operations, generated an instruction-by-instruction log using the [`nestest`](https://wiki.nesdev.com/w/index.php/Emulator_tests) test ROM, and verified that it matched the reference log.

### PPU

At 60 **Frames per Second**(FPS) the NES' CPU can execute `29780` clock cycles per frame[^6], but there are `61440` different pixels to display. So the CPU is too slow to draw even a blank screen.

A **Picture Processing Unit**(PPU) runs at triple the CPU's clock speed, emitting one pixel per cycle. It has more cycles available than pixels to emit, so the idle time("Vertical Blank") is used by the CPU to configure the PPU for the next frame.

The PPU draws two things: Background tiles and sprites. Up to 32 x 30 tiles and 64 sprites can be displayed at once, and these share a pool of 512 different 8x8 4-color patterns.

There are 4 tables in PPU address space that configure these:
* **Nametable**: A table of 32x30 bytes that specify which 8x8 pattern to use
* **Attribute Table**: Specifies which 4-color palette is used for each 16x16 group of tiles
* **Object Attribute Memory**(OAM): Stores the position, palette, and status of 64 sprites
* **Palette**: There are 8 different 4-color palettes. The first color is always transparent, and the other 3 choose from 64 different **System Colors**.

The PPU and CPU have different address spaces, but they are not isolated. There are three communication channels between them:
* There are 8 PPU registers mapped in the CPU's address space.
* The PPU triggers two CPU interrupts: End-of-scanline and Vertical Blank.
* The cartridge itself can choose to modify its assigned PPU address space based on reads or writes to the CPU address space.

There are 5 primitive actions that the PPU can take on each clock cycle:
* Request a read( in PPU address space)
* Request a write
* Change its internal registers
* Emit a single pixel
* Trigger a CPU interrupt

It is more difficult to test the PPU than the CPU: Much of the PPU must already be functioning in order to get any output at all. I recommend printing out the 4 tables separately - and once that data is confirmed correct, then test whether these are combined correctly for any particular pixel.

### Save States

A common feature in emulators is to save and load a game at any time. Every component implements the `Savable` trait:

{% highlight rust %}
pub trait Savable {
    fn save(&self, fh: &mut Write);
    fn load(&mut self, fh: &mut Read);
}
{% endhighlight %}

There are two properties that make this model useful:
* Once a ROM has been loaded, my emulator doesn't allocate memory. So every object can be written in-place.
* Loading a ROM always creates the same types of components, so the values saved from a previous program execution can be assumed compatible.

The first property guarantees that pointers aren't affected by a savestate restore. If a component is dynamically-allocated and a savestate from before or after its lifetime is loaded, then any pointers to it are no longer valid. This would require keeping track of which objects have pointers to which other objects.

If this second property failed(say, if someone unplugged a standard NES controller and plugged in a new SNES controller), then the savestate files would need to include type information so the correct `load` method is called.

However, assuming these restrictions makes serialization straightforward. For example:

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

Every component except for the primitives like `bool` or `u32` look like that. The primitives aren't too difficult either:

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

Rust's traits were useful for implementing savestates. The compiler infers which `save` or `load` methods are needed for each type, so the code is uniform.

The video playback feature builds on top of savestates by storing 8 bits per frame - one for whether each controller button was pressed. When a savestate is restored, it also restores the active input list.

## Rust Language

The previous section gave a high-level overview of how I designed my NES emulator. In this section, I'll talk about the Rust language itself.

### Integer overflow

By default, Rust will throw an exception if any arithmetic operation overflows. This caught a fair number of bugs during testing, because the [wrapping_*](https://doc.rust-lang.org/std/primitive.u32.html#method.wrapping_add) functions require explicit type information and it's important whether a number wraps as a 16-bit value or an 8-bit value.

I used functions like `wrapping_add` in my emulator, but there are also [Wrapping](https://doc.rust-lang.org/std/num/struct.Wrapping.html) types that imply wrapping arithmetic operations.

### Single-ownership

In Rust, mutable values must have a single owning variable. Other variables can **borrow** it, but only one mutable reference is allowed at a single time.

The CPU address space has several PPU registers mapped. So the CPU maintains a permanent mutable reference to the PPU. But the top-level `Nes` object also owns the PPU.

I worked around this using [Box](https://doc.rust-lang.org/std/boxed/struct.Box.html) to assign fixed memory addresses to values, and then "unsafe" pointer dereferences when needed.

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

A [Mutex](https://doc.rust-lang.org/beta/book/ch16-03-shared-state.html) might be a solution here. The lock would be held for a small amount of time, so it shouldn't cause much of a performance issue.

### Performance

In a later article, I plan to train a **Reinforcement Learning**(RL) agent to play Mario. A faster emulator allows me to gather more sample data, so I spent a few hours ensuring my emulator was comparable to other NES emulators used for RL research.

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

The main difference seems to be that `foo` counts bytes while `bar` counts elements. Perhaps the tuple `(x, idx)` has size 16 bytes which is larger than the maximum scale of `8` in addressing modes like `QWORD PTR [rdi+rdx*8+C]`. `rustc` might choose its loop strategy before later optimizations make them equivalent. The difference goes away if `foo` and `bar` are changed to take `u8`s rather than `usize`s.

In any case, changing one of my loops to use `bar`'s style improved the emulator's FPS by 10%.

#### Search order

The CPU Address Space consists of many components which each listen for reads and writes to specific addresses.

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
    mapper.map_address_space(0x4017, 0x4017, joystick2, false); // TODO - joystick2 isn't used, but this transfers ownership so it isn't deallocated(since it is updated through pointers)

    mapper.map_null(0x4018, 0x401F); // APU test mode
    mapper.map_address_space(0x4020, 0xFFFF, cartridge, true);

    self.cpu.mapper = Box::new(mapper);
    self.cpu.initialize();
}
{% endhighlight %}

However, this puts the `cartridge` at the very end. The cartridge contains all of the game's code, so every time the CPU fetched a new instruction it got a worst-case linear-search.

I experimented with an LRU cache, but found the best improvement by simply reordering those statements to put the most frequently-accessed components first.

This also gave a 10% improvement in FPS.

## Conclusion

This article discussed an NES emulator I developed using the Rust programming language.

If you're interested in learning more about NES development, I recommend the [Nesdev Wiki](https://wiki.nesdev.com/w/index.php/Nesdev_Wiki) for technical details needed to make emulators or games.

Future articles on this subject may cover:
* Training an agent to play Mario
* Improving the performance of the emulator
* Developing emulators for other systems
* Compiling the emulator to Javascript to run in-browser
* Compatibility issues with less-common games
* How specific NES games worked internally
* Porting the emulator to run on a GPU[^2]
* Writing an NES game in Rust
* Making new optimization passes for `rustc` or LLVM
* Accelerating an emulator with a JIT compiler

### Footnotes

[^1]: There are [digital logic simulators](https://wiki.nesdev.com/w/index.php/Visual_2C02) created from special photographs of the CPU. These are not more accurate than a correctly-implemented cycle-accurate emulator, but are useful debugging tools to determine what the correct behavior should be. There are [rare cases](http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_%28XAA,_ANE%29) where even this is not enough to explain the precise behavior of the CPU, but no published NES games make use of this detail.
[^2]: The visible part of the emulator only depends on SDL, and the headless version might not even need the standard library. So it should be possible to port it to [exotic platforms](https://www.michaelburge.us/2017/09/10/injecting-shellcode-to-speed-up-amazon-redshift.html).
[^3]: My emulator actually uses quite a few [`Box`](https://doc.rust-lang.org/std/boxed/struct.Box.html) objects. Mostly this isn't to allocate memory, but to assign data a fixed memory location so I can safely take mutable pointers to it. I think all dynamic allocation is removable except for the choice of mapper when loading a cartridge. Since each game can have different components inside, dynamic allocation seems necessary there.
[^4]: See this video on [Reverse Emulation](https://www.youtube.com/watch?v=ar9WRwCiSr0). It's possible to put a modern processor inside an NES cartridge to do some neat tricks.
[^5]: I haven't confirmed with the digital logic simulator that the 7 clock cycles map precisely to the 7 actions I mentioned. It's an educated guess that serves to illustrate the difference between instructions and clock cycles.
[^6]: These numbers are simplified and assume the NTSC video standard. NES consoles sold in regions that use the PAL standard have slightly different timings to generate video. Even/odd frames may take an extra clock cycle. Refer to the Nesdev Wiki for exact timing details.