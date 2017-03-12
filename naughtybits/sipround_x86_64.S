/*****************************************************************************
*
*  Raw x86_64 assembly implementations of a SIPROUND:
*
*    https://github.com/veorq/SipHash/blob/93ca99dcfa6a32b1b617e9a5c3c044685254ce8e/siphash.c#L44
*
*  We go through all this only because GHC can't yet emit rotate instructions.
*  The translation from C to assembly would be trivial, but we use gcc's output
*  below which (arbitrarily?) compiles to constant right rotates instead of
*  left, and does some reordering of the instructions, presumably for better
*  pipelining. Interestingly clang does no reordering.
*
*  I didn't go through the excercise of measuring how much worse performance
*  would be if I had to allocate a struct to return the four values here and
*  peek them in haskell-land in the usual way, but we measure 6ns per iteration
*  of the haskell function wrapping `sipRound_s_x1`. 
*
 ****************************************************************************/


/* 
*  Here we avoid hard-coding the particulars of GHC's calling convention
*  everywhere below. See:
*
*    https://github.com/ghc/ghc/blame/master/includes/stg/MachRegs.h#L159
*
*  These appear unlikely to change, and when they do we'll hopefully notice and
*  do some conditional stuff. I think we could also get these by including
*  "Rts.h", and then e.g. we could give nicer names to REG_R1, etc. if we want.
*
*  The registers here correspond place-wise to the elements of the tuple you
*  pass in and return from the foreign wrapper function you create, e.g.
*
*      (REG_R1, REG_R2) -> (REG_R1, REG_R2, REG_R3)
*
*  I haven't looked into which other registers (other than the ones that hold
*  arguments) are caller-saves and safe to work with, or generally what happens
*  in situations less straightforward than the one we're doing here:
*/
#define REG_R1     %rbx
#define REG_R2     %r14
#define REG_R3     %rsi
#define REG_R4     %rdi
#define REG_R5     %r8
#define REG_R6     %r9
#define REG_Sp %rbp
/* 
*  We have to end every procedure with this to get back to haskell land.
*/
.macro returnHask
        jmp     *(REG_Sp)
.endm


/* 
*  We can do a little abstraction with ASM macros. There might be a better way
*  to do this
*/
.macro sipRound
        addq     REG_R2,  REG_R1
        addq     REG_R4,  REG_R3
        rorq    $51,      REG_R2
        rorq    $48,      REG_R4
        xorq     REG_R1,  REG_R2
        xorq     REG_R3,  REG_R4
        rorq    $32,      REG_R1
        addq     REG_R2,  REG_R3
        addq     REG_R4,  REG_R1
        rorq    $47,      REG_R2
        rorq    $43,      REG_R4
        xorq     REG_R3,  REG_R2
        xorq     REG_R1,  REG_R4
        rorq    $32,      REG_R3
.endm


/*
*  Exported stuff. The benefits to wrapping the hand-unrolled iterations here
*  are significant:
*
*     12x sipRound_s_x1 = 73 ns
*      4x sipRound_s_x3 = 45 ns
*/
.global sipRound_s_x1
sipRound_s_x1:
        sipRound
        returnHask

.global sipRound_s_x2
sipRound_s_x2:
        sipRound
        sipRound
        returnHask

.global sipRound_s_x3
sipRound_s_x3:
        sipRound
        sipRound
        sipRound
        returnHask

.global sipRound_s_x4
sipRound_s_x4:
        sipRound
        sipRound
        sipRound
        sipRound
        returnHask
