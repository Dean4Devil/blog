---
title: 'How to write readable (MIPS) Assembly - An exercise in futility.'
abstract: 'Programming is about managing complexity; Computers are infinitely stupid but Programmers are only finitely smart.'
...

Programming is about managing complexity; Computers are infinitely stupid but
Programmers are only finitely smart.

There are two really good ways of managing complexity:

Abstraction and Modularization.

High-Level languages are all about abstraction. When you call a function in C
you don't think about how the stack looks, when you instanciate a class in
Python you have already forgotten what machine you're running on and when you
apply a Functor to a Monoid in Haskell you are so wrapped up in the mathematical
fabric of space that you probably can't even make out earth from up there.

In assembly however you don't get to abstract much. Yes you can write
subroutines and functions and therelike but you can't abstract away a problem
further and further until you can wrap your head around it.

Modularization is one of those things you get for basically free when you write
high-level languages. You split code into functions, functions into traits,
interfaces, classes (both the Haskell & Python kind), traits, interfaces,
classes into hierarchies of traits, interfaces, classes until you end up with
building blocks of code that you just glue together to build the program.

In assembly modularization is the holy grail. Your highest goal of all
assembly code you write should be to be as modular and reusable as humanly
possible. If you are writing assembly and you actually know that other goals
are more important to you, be it size, be it speed - this guide contains nothing
it can teach you anymore.

Whatever this text may be, it is not a tutorial. It is not there to teach you
assembly. It is not an authorative document either. If anything, it's a loosely
coupled string of ideas to make the next piece of assembly code you write an
iota more readable and an iota more reusable.

## Calling conventions

Calling conventions are one of the most important tools in managing complexity;
as in: It'll quickly be the only thing keeping you sane. Calling
conventions dictate some rules functions have to follow when they are called,
what registers may be used for what and what guarantees you can rely on.

In MIPS the most widespread calling convention is the `O32 ABI`. It seperates
the registers as follows:

| Register              | Description                                                                            |
| --------------------- | -------------------------------------------------------------------------------------- |
| $0        / $zero     | Always 0. Can't be written into so no use saving                                       |
| $1        / $at       | Temporary register used by the assembler. No touchy.                                   |
| $2  - $3  / $v0 - $v1 | Return values of functions. $v0 is the primary, $v1 the secondary output               |
| $4  - $7  / $a0 - $a3 | Parameters for functions                                                               |
| $8  - $15 / $t0 - $t7 | Temporary registers. A function doesn't need to save or restore these.                 |
| $16 - $23 / $s0 - $s7 | Saved temporary registers. They need to be saved by a called function.                 |
| $24 - $25 / $t8 - $t9 | Temporaries. See above.                                                                |
| $26 - $27 / $k0 - $k1 | Reserved for the Kernel. No touchy!                                                    |
| $28       / $gp       | Global pointer. NO TOUCHY!                                                             |
| $29       / $sp       | Stack pointer. A function **must** save and reset this or everything breaks horribly.  |
| $30       / $fp       | Frame pointer. Seriously though, no touchy.                                            |
| $31       / $ra       | Return address. You need it all the time so save it.                                   |

In Plain english:

* When you **call** a function, the parameters go into the $a0-$a3 registers. If you have more then 4 arguments for a function put the additional parameters on the stack, also you're doing it wrong.
* When you **call** a function it's *your* job to save the $t0-$t9 registers.
* When you **write** a function return values in the $v0 and $v1 registers. It makes a lot of sense to have one be an error/success value (0 = SUCCESS, all other errors of some kind) and the actual result in the other one. Chose which one, but **BE. CONSISTENT.**
* When you **write** a function it's *your* job to save the $s0-$s7 registers and restore them before you return. However, feel welcome to fuck with the $t0-$t9 registers and not clean up afterwards.
* $sp has the same value after your function has been called compared to before. NO. EXCEPTIONS. (Does **not** apply to subroutines.)
* If you want to return some block of memory from a function; guess what: that's what heap is for!

## Subroutine? Function? Difference?

You see, Function is spelled "Function" and Subroutine is spelled "I will fuck
up your program."

In short: A subroutine is not a function. It doesn't give you any of the
guarantees of a function. It won't save the saved registers, it won't restore
the stack pointer, it may make you a sandwich that tastes like utter
dissappointment.

However there are situations where subroutines are really useful:

```mips
random_function:
    addi $sp, $sp, -4
    sw $ra, ($sp)

    addi $sp, $sp, -32
    sw $s0, 28($sp)
    sw $s1, 24($sp)
    sw $s2, 20($sp)
    sw $s3, 14($sp)
    sw $s4, 12($sp)
    sw $s5,  8($sp)
    sw $s6,  4($sp)
    sw $s7,  0($sp)

    # Random bullshit your function does

    lw $s0, 28($sp)
    lw $s1, 24($sp)
    lw $s2, 20($sp)
    lw $s3, 14($sp)
    lw $s4, 12($sp)
    lw $s5,  8($sp)
    lw $s6,  4($sp)
    lw $s7,  0($sp)
    addi $sp, $sp, -32

    addi $sp, $sp, 4
    lw $ra, ($sp)

    jr $ra

even_more_random_function:
    addi $sp, $sp, -4
    sw $ra, ($sp)

    addi $sp, $sp, -32
    sw $s0, 28($sp)
    sw $s1, 24($sp)
    sw $s2, 20($sp)
    sw $s3, 14($sp)
    sw $s4, 12($sp)
    sw $s5,  8($sp)
    sw $s6,  4($sp)
    sw $s7,  0($sp)

    # Even more random bullshit your function does

    lw $s0, 28($sp)
    lw $s1, 24($sp)
    lw $s2, 20($sp)
    lw $s3, 14($sp)
    lw $s4, 12($sp)
    lw $s5,  8($sp)
    lw $s6,  4($sp)
    lw $s7,  0($sp)
    addi $sp, $sp, -32

    addi $sp, $sp, 4
    lw $ra, ($sp)

    jr $ra
```

Compare that to:

```mips
store_s_registers:
    addi $sp, $sp, -32
    sw $s0, 28($sp)
    sw $s1, 24($sp)
    sw $s2, 20($sp)
    sw $s3, 14($sp)
    sw $s4, 12($sp)
    sw $s5,  8($sp)
    sw $s6,  4($sp)
    sw $s7,  0($sp)
    jr $ra

load_s_registers:
    lw $s0, 28($sp)
    lw $s1, 24($sp)
    lw $s2, 20($sp)
    lw $s3, 14($sp)
    lw $s4, 12($sp)
    lw $s5,  8($sp)
    lw $s6,  4($sp)
    lw $s7,  0($sp)
    addi $sp, $sp, -32
    jr $ra

random_function:
    addi $sp, $sp, -4
    sw $ra, ($sp)
    jal store_s_registers

    # Random bullshit your function does

    jal load_s_registers
    addi $sp, $sp, 4
    lw $ra, ($sp)

    jr $ra

random_function:
    addi $sp, $sp, -4
    sw $ra, ($sp)
    jal store_s_registers

    # Even more random bullshit your function does

    jal load_s_registers
    addi $sp, $sp, 4
    lw $ra, ($sp)

    jr $ra
```

So, which one of the two code samples is more readable?

There are two answers to that question; One is "the second one", the other one
is wrong.

To close the circle to the beginning of this point: How do you see that
`store_s_registers` and `load_s_registers` aren't functions?

When they jump back `$sp` is not the same. Usually this will bite you in the
ass in the most gruesome way but those two subroutines happen to mess with `$sp`
contrary to each other so they happen to un-fuck each other's fuck-ups when you
call them in pairs.

## Comments

No:
```mips
addi $sp, $sp, -4 # Substract 4 from $sp
sw $ra, ($sp)     # store $ra
```

Yes:
```mips
addi $sp, $sp, -4 # Make space on the stack
sw $ra, ($sp)     # Store the return address on the stack so we can restore
```

Don't explain *what* you do but *why* you do it.
Also, don't explain the obvious. Please.

Keep one thing in mind:
**Wrong comments are much worse than no comments at all.**


## Syscalls

Syscalls in SPIM are terribly implemented. They for the most part don't tell you
about errors, are inconsistent and most of them shouldn't be syscalls in the
first place. All of the read and write to terminal for example, they should be
implemented over a file with handle 0, 1, or 2. In short: if you write actual
real-life code, you'll enjoy syscalls much more than you will with SPIM.

So, make the best of what you have (which is - arguably - not much).

One thing you can do if you want to do some extra work but have code that
resembles the real world much more is enable mapped io. In that mode you don't
get some half-assed syscalls to do stdio but use memory-mapped IO registers for
that purpose.

Even then file based IO (which plays a much bigger part of this homework)
has to be done using syscalls.

Use EQUs, don't consider the cost of syscalls and just reopen files and please
stop thinking why SPIMs filehandles don't support seeking.

## EQU's

Surprisingly enough, SPIM's assembler in all its shitty-ness still knows an EQU statement.

EQU's are quite similar to `#define` statements in C. I think the best way to explain them is with an example:

Without EQU
```mips
li $v0, 13
la $a0, path
li $a1, 0
syscall
```

With EQU's
```mips
SYS_FOPEN = 13
O_RDONLY = 0

li $v0, SYS_FOPEN
la $a0, path
li $a1, O_RDONLY
syscall
```

I don't think I need to tell you which one is more readable.

EQU's can be pretty much everywhere. But don't spread them. Unless you have a
really really good reason to do otherwise (hint: You don't.) put them all at the
top of your file, before the .data section.

## Structure

The code you write in modern assembly closely resembles the structure of ELF
binaries (Google it, I won't tell you.).

Do yourself a favour and just use this structure:

1. EQUs
2. `.data`
3. `.text`
4. `main`

## Appendix A: Delay instructions

MIPS is a pipelines architecture and one of the sideeffects of it's design are
the so called delay instructions.

In short: The instruction directly after a jump or a branch will be executed
before the jump or branch happens.

This might seem counter-intuitive but trust me when you look into it it makes
sense.

Here, read this code:
```mips
main:
    li $t0, 1
    jal subr    ; Jump to label subr
    li $t1, 1   ; Delay Slot
    ; Discontinuity - Jump takes effect
    li $t4, 1   ; Returned jump, past the delay slot
    li $t5, 1
    j end       ; Jump to another label
    li $t6, 1
    ; Discontinuity - Jump takes effect

subr:
    li $t2, 1
    jr $ra
    li $t3, 1
    ; Discontinuity - Jump takes effect

end:
    li $t7, 1
```

If you want, load it in QtSPIM. If you do, go to `Simulator` > `Settings` > `MIPS` and enable 'Delayed Branches'.

Step through the code line for line one time with those settings enabled and once without.

#### Do you need to care about this?

Short answer: **No.**

Long answer: Delay instructions allow for some quirky optimizations and other
fun stuff. If you want make your program compatible with them and even abuse
their power.

