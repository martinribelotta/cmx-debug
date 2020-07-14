/* Instruction printing code for the ARM
   Copyright (C) 2020 Martin Ribelotta.
   Code modified from gnu binutils-gdb at <root>/opcodes/arm-dis.c

   Original Copyrigth:

   Copyright (C) 1994-2020 Free Software Foundation, Inc.
   Contributed by Richard Earnshaw (rwe@pegasus.esprit.ec.org)
   Modification by James G. Smith (jsmith@cygnus.co.uk)

   This file is part of libopcodes.

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   It is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#include "disassembler.h"

#include <stdbool.h>
#include <stdio.h>

typedef uint32_t bfd_vma;
typedef bool bfd_boolean;

#define FALSE false
#define TRUE true

#define W_BIT 21
#define I_BIT 22
#define U_BIT 23
#define P_BIT 24

#define WRITEBACK_BIT_SET   (given & (1 << W_BIT))
#define IMMEDIATE_BIT_SET   (given & (1 << I_BIT))
#define NEGATIVE_BIT_SET   ((given & (1 << U_BIT)) == 0)
#define PRE_BIT_SET         (given & (1 << P_BIT))

void abort()
{
   printf("Abort decode\n");
}

#define UNDEFINED_INSTRUCTION "<und>"
#define UNPREDICTABLE_INSTRUCTION "<unpr>"

enum {
   FLAGS_NONE = 0,
   FLAGS_MEMORY_ACCESS = (1 << 0),
};

struct opcode32 {
   uint8_t flags;             /* Instruction flags */
   unsigned long value;		   /* If arch == 0 then value is a sentinel.  */
   unsigned long mask;		   /* Recognise insn if (op & mask) == value.  */
   const char *  assembler;	/* How to disassemble this insn.  */
};

struct opcode16 {
   uint8_t flags;             /* Instruction flags */
   unsigned short value, mask;	/* Recognise insn if (op & mask) == value.  */
   const char *assembler;	/* How to disassemble this insn.  */
};

/* print_insn_thumb16 recognizes the following format control codes:

   %S                   print Thumb register (bits 3..5 as high number if bit 6 set)
   %D                   print Thumb register (bits 0..2 as high number if bit 7 set)
   %<bitfield>I         print bitfield as a signed decimal
   				(top bit of range being the sign bit)
   %N                   print Thumb register mask (with LR)
   %O                   print Thumb register mask (with PC)
   %M                   print Thumb register mask
   %b			print CZB's 6-bit unsigned branch destination
   %s			print Thumb right-shift immediate (6..10; 0 == 32).
   %c			print the condition code
   %C			print the condition code, or "s" if not conditional
   %x			print warning if conditional an not at end of IT block"
   %X			print "\t; unpredictable <IT:code>" if conditional
   %I			print IT instruction suffix and operands
   %W			print Thumb Writeback indicator for LDMIA
   %<bitfield>r		print bitfield as an ARM register
   %<bitfield>d		print bitfield as a decimal
   %<bitfield>H         print (bitfield * 2) as a decimal
   %<bitfield>W         print (bitfield * 4) as a decimal
   %<bitfield>a         print (bitfield * 4) as a pc-rel offset + decoded symbol
   %<bitfield>B         print Thumb branch destination (signed displacement)
   %<bitfield>c         print bitfield as a condition code
   %<bitnum>'c		print specified char iff bit is one
   %<bitnum>?ab		print a if bit is one else print b.  */

static const struct opcode16 thumb_opcodes[] = {
   /* Thumb instructions.  */

   /* ARM V6K no-argument instructions.  */
   { FLAGS_NONE, 0xbf00, 0xffff, "nop%c"},
   { FLAGS_NONE, 0xbf10, 0xffff, "yield%c"},
   { FLAGS_NONE, 0xbf20, 0xffff, "wfe%c"},
   { FLAGS_NONE, 0xbf30, 0xffff, "wfi%c"},
   { FLAGS_NONE, 0xbf40, 0xffff, "sev%c"},
   { FLAGS_NONE, 0xbf00, 0xff0f, "nop%c\t{%4-7d}"},

   /* ARM V6T2 instructions.  */
   { FLAGS_NONE, 0xb900, 0xfd00, "cbnz\t%0-2r, %b%X"},
   { FLAGS_NONE, 0xb100, 0xfd00, "cbz\t%0-2r, %b%X"},
   { FLAGS_NONE, 0xbf00, 0xff00, "it%I%X"},

   /* ARM V6.  */
   { FLAGS_NONE, 0xb660, 0xfff8, "cpsie\t%2'a%1'i%0'f%X"},
   { FLAGS_NONE, 0xb670, 0xfff8, "cpsid\t%2'a%1'i%0'f%X"},
   { FLAGS_NONE, 0x4600, 0xffc0, "mov%c\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0xba00, 0xffc0, "rev%c\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0xba40, 0xffc0, "rev16%c\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0xbac0, 0xffc0, "revsh%c\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0xb650, 0xfff7, "setend\t%3?ble%X"},
   { FLAGS_NONE, 0xb200, 0xffc0, "sxth%c\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0xb240, 0xffc0, "sxtb%c\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0xb280, 0xffc0, "uxth%c\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0xb2c0, 0xffc0, "uxtb%c\t%0-2r, %3-5r"},

   /* ARM V5 ISA extends Thumb.  */
   { FLAGS_NONE, 0xbe00, 0xff00, "bkpt\t%0-7x"}, /* Is always unconditional.  */
   /* This is BLX(2).  BLX(1) is a 32-bit instruction.  */
   { FLAGS_NONE, 0x4780, 0xff87, "blx%c\t%3-6r%x"},	/* note: 4 bit register number.  */
   /* ARM V4T ISA (Thumb v1).  */
   { FLAGS_NONE, 0x46C0, 0xFFFF, "nop%c\t\t\t; (mov r8, r8)"},
   /* Format 4.  */
   { FLAGS_NONE, 0x4000, 0xFFC0, "and%C\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x4040, 0xFFC0, "eor%C\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x4080, 0xFFC0, "lsl%C\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x40C0, 0xFFC0, "lsr%C\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x4100, 0xFFC0, "asr%C\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x4140, 0xFFC0, "adc%C\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x4180, 0xFFC0, "sbc%C\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x41C0, 0xFFC0, "ror%C\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x4200, 0xFFC0, "tst%c\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x4240, 0xFFC0, "neg%C\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x4280, 0xFFC0, "cmp%c\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x42C0, 0xFFC0, "cmn%c\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x4300, 0xFFC0, "orr%C\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x4340, 0xFFC0, "mul%C\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x4380, 0xFFC0, "bic%C\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x43C0, 0xFFC0, "mvn%C\t%0-2r, %3-5r"},
   /* format 13 */
   { FLAGS_NONE, 0xB000, 0xFF80, "add%c\tsp, #%0-6W"},
   { FLAGS_NONE, 0xB080, 0xFF80, "sub%c\tsp, #%0-6W"},
   /* format 5 */
   { FLAGS_NONE, 0x4700, 0xFF80, "bx%c\t%S%x"},
   { FLAGS_NONE, 0x4400, 0xFF00, "add%c\t%D, %S"},
   { FLAGS_NONE, 0x4500, 0xFF00, "cmp%c\t%D, %S"},
   { FLAGS_NONE, 0x4600, 0xFF00, "mov%c\t%D, %S"},
   /* format 14 */
   { FLAGS_NONE, 0xB400, 0xFE00, "push%c\t%N"},
   { FLAGS_NONE, 0xBC00, 0xFE00, "pop%c\t%O"},
   /* format 2 */
   { FLAGS_NONE, 0x1800, 0xFE00, "add%C\t%0-2r, %3-5r, %6-8r"},
   { FLAGS_NONE, 0x1A00, 0xFE00, "sub%C\t%0-2r, %3-5r, %6-8r"},
   { FLAGS_NONE, 0x1C00, 0xFE00, "add%C\t%0-2r, %3-5r, #%6-8d"},
   { FLAGS_NONE, 0x1E00, 0xFE00, "sub%C\t%0-2r, %3-5r, #%6-8d"},
   /* format 8 */
   { FLAGS_NONE, 0x5200, 0xFE00, "strh%c\t%0-2r, [%3-5r, %6-8r]"},
   { FLAGS_NONE, 0x5A00, 0xFE00, "ldrh%c\t%0-2r, [%3-5r, %6-8r]"},
   { FLAGS_NONE, 0x5600, 0xF600, "ldrs%11?hb%c\t%0-2r, [%3-5r, %6-8r]"},
   /* format 7 */
   { FLAGS_NONE, 0x5000, 0xFA00, "str%10'b%c\t%0-2r, [%3-5r, %6-8r]"},
   { FLAGS_NONE, 0x5800, 0xFA00, "ldr%10'b%c\t%0-2r, [%3-5r, %6-8r]"},
   /* format 1 */
   { FLAGS_NONE, 0x0000, 0xFFC0, "mov%C\t%0-2r, %3-5r"},
   { FLAGS_NONE, 0x0000, 0xF800, "lsl%C\t%0-2r, %3-5r, #%6-10d"},
   { FLAGS_NONE, 0x0800, 0xF800, "lsr%C\t%0-2r, %3-5r, %s"},
   { FLAGS_NONE, 0x1000, 0xF800, "asr%C\t%0-2r, %3-5r, %s"},
   /* format 3 */
   { FLAGS_NONE, 0x2000, 0xF800, "mov%C\t%8-10r, #%0-7d"},
   { FLAGS_NONE, 0x2800, 0xF800, "cmp%c\t%8-10r, #%0-7d"},
   { FLAGS_NONE, 0x3000, 0xF800, "add%C\t%8-10r, #%0-7d"},
   { FLAGS_NONE, 0x3800, 0xF800, "sub%C\t%8-10r, #%0-7d"},
   /* format 6 */
   { FLAGS_MEMORY_ACCESS, 0x4800, 0xF800, "ldr%c\t%8-10r, [pc, #%0-7W]\t; (%0-7a)"},  /* TODO: Disassemble PC relative "LDR rD,=<symbolic>" */
   /* format 9 */
   { FLAGS_MEMORY_ACCESS, 0x6000, 0xF800, "str%c\t%0-2r, [%3-5r, #%6-10W]"},
   { FLAGS_MEMORY_ACCESS, 0x6800, 0xF800, "ldr%c\t%0-2r, [%3-5r, #%6-10W]"},
   { FLAGS_MEMORY_ACCESS, 0x7000, 0xF800, "strb%c\t%0-2r, [%3-5r, #%6-10d]"},
   { FLAGS_MEMORY_ACCESS, 0x7800, 0xF800, "ldrb%c\t%0-2r, [%3-5r, #%6-10d]"},
   /* format 10 */
   { FLAGS_MEMORY_ACCESS, 0x8000, 0xF800, "strh%c\t%0-2r, [%3-5r, #%6-10H]"},
   { FLAGS_MEMORY_ACCESS, 0x8800, 0xF800, "ldrh%c\t%0-2r, [%3-5r, #%6-10H]"},
   /* format 11 */
   { FLAGS_MEMORY_ACCESS, 0x9000, 0xF800, "str%c\t%8-10r, [sp, #%0-7W]"},
   { FLAGS_MEMORY_ACCESS, 0x9800, 0xF800, "ldr%c\t%8-10r, [sp, #%0-7W]"},
   /* format 12 */
   { FLAGS_MEMORY_ACCESS, 0xA000, 0xF800, "add%c\t%8-10r, pc, #%0-7W\t; (adr %8-10r, %0-7a)"},
   { FLAGS_MEMORY_ACCESS, 0xA800, 0xF800, "add%c\t%8-10r, sp, #%0-7W"},
   /* format 15 */
   { FLAGS_NONE, 0xC000, 0xF800, "stmia%c\t%8-10r!, %M"},
   { FLAGS_NONE, 0xC800, 0xF800, "ldmia%c\t%8-10r%W, %M"},
   /* format 17 */
   { FLAGS_NONE, 0xDF00, 0xFF00, "svc%c\t%0-7d"},
   /* format 16 */
   { FLAGS_NONE, 0xDE00, 0xFF00, "udf%c\t#%0-7d"},
   { FLAGS_NONE, 0xDE00, 0xFE00, UNDEFINED_INSTRUCTION},
   { FLAGS_NONE, 0xD000, 0xF000, "b%8-11c.n\t%0-7B%X"},
   /* format 18 */
   { FLAGS_NONE, 0xE000, 0xF800, "b%c.n\t%0-10B%x"},

   /* The E800 .. FFFF range is unconditionally redirected to the
      32-bit table, because even in pre-V6T2 ISAs, BL and BLX(1) pairs
      are processed via that table.  Thus, we can never encounter a
      bare "second half of BL/BLX(1)" instruction here.  */
   { FLAGS_NONE, 0x0000, 0x0000, UNDEFINED_INSTRUCTION},
   { FLAGS_NONE, 0, 0, 0}
};

/* Thumb32 opcodes use the same table structure as the ARM opcodes.
   We adopt the convention that hw1 is the high 16 bits of .value and
   .mask, hw2 the low 16 bits.

   print_insn_thumb32 recognizes the following format control codes:

       %%		%

       %I		print a 12-bit immediate from hw1[10],hw2[14:12,7:0]
       %M		print a modified 12-bit immediate (same location)
       %J		print a 16-bit immediate from hw1[3:0,10],hw2[14:12,7:0]
       %K		print a 16-bit immediate from hw2[3:0],hw1[3:0],hw2[11:4]
       %H		print a 16-bit immediate from hw2[3:0],hw1[11:0]
       %S		print a possibly-shifted Rm

       %L		print address for a ldrd/strd instruction
       %a		print the address of a plain load/store
       %w		print the width and signedness of a core load/store
       %m		print register mask for ldm/stm

       %E		print the lsb and width fields of a bfc/bfi instruction
       %F		print the lsb and width fields of a sbfx/ubfx instruction
       %b		print a conditional branch offset
       %B		print an unconditional branch offset
       %s		print the shift field of an SSAT instruction
       %R		print the rotation field of an SXT instruction
       %U		print barrier type.
       %P		print address for pli instruction.
       %c		print the condition code
       %x		print warning if conditional an not at end of IT block"
       %X		print "\t; unpredictable <IT:code>" if conditional

       %<bitfield>d	print bitfield in decimal
       %<bitfield>W	print bitfield*4 in decimal
       %<bitfield>r	print bitfield as an ARM register
       %<bitfield>R	as %<>r but r15 is UNPREDICTABLE
       %<bitfield>S	as %<>R but r13 is UNPREDICTABLE
       %<bitfield>c	print bitfield as a condition code

       %<bitfield>'c	print specified char iff bitfield is all ones
       %<bitfield>`c	print specified char iff bitfield is all zeroes
       %<bitfield>?ab... select from array of values in big endian order

   With one exception at the bottom (done because BL and BLX(1) need
   to come dead last), this table was machine-sorted first in
   decreasing order of number of bits set in the mask, then in
   increasing numeric order of mask, then in increasing numeric order
   of opcode.  This order is not the clearest for a human reader, but
   is guaranteed never to catch a special-case bit pattern with a more
   general mask, which is important, because this instruction encoding
   makes heavy use of special-case bit patterns.  */
static const struct opcode32 thumb32_opcodes[] = {
   /* Instructions defined in the basic V6T2 set.  */
   { FLAGS_NONE, 0xf3af8000, 0xffffffff, "nop%c.w"},
   { FLAGS_NONE, 0xf3af8001, 0xffffffff, "yield%c.w"},
   { FLAGS_NONE, 0xf3af8002, 0xffffffff, "wfe%c.w"},
   { FLAGS_NONE, 0xf3af8003, 0xffffffff, "wfi%c.w"},
   { FLAGS_NONE, 0xf3af8004, 0xffffffff, "sev%c.w"},
   { FLAGS_NONE, 0xf3af8000, 0xffffff00, "nop%c.w\t{%0-7d}"},
   { FLAGS_NONE, 0xf7f0a000, 0xfff0f000, "udf%c.w\t%H"},

   { FLAGS_NONE, 0xf3bf8f2f, 0xffffffff, "clrex%c"},
   { FLAGS_NONE, 0xf3af8400, 0xffffff1f, "cpsie.w\t%7'a%6'i%5'f%X"},
   { FLAGS_NONE, 0xf3af8600, 0xffffff1f, "cpsid.w\t%7'a%6'i%5'f%X"},
   { FLAGS_NONE, 0xf3c08f00, 0xfff0ffff, "bxj%c\t%16-19r%x"},
   { FLAGS_NONE, 0xe810c000, 0xffd0ffff, "rfedb%c\t%16-19r%21'!"},
   { FLAGS_NONE, 0xe990c000, 0xffd0ffff, "rfeia%c\t%16-19r%21'!"},
   { FLAGS_NONE, 0xf3e08000, 0xffe0f000, "mrs%c\t%8-11r, %D"},
   { FLAGS_NONE, 0xf3af8100, 0xffffffe0, "cps\t#%0-4d%X"},
   { FLAGS_NONE, 0xe8d0f000, 0xfff0fff0, "tbb%c\t[%16-19r, %0-3r]%x"},
   { FLAGS_NONE, 0xe8d0f010, 0xfff0fff0, "tbh%c\t[%16-19r, %0-3r, lsl #1]%x"},
   { FLAGS_NONE, 0xf3af8500, 0xffffff00, "cpsie\t%7'a%6'i%5'f, #%0-4d%X"},
   { FLAGS_NONE, 0xf3af8700, 0xffffff00, "cpsid\t%7'a%6'i%5'f, #%0-4d%X"},
   { FLAGS_NONE, 0xf3de8f00, 0xffffff00, "subs%c\tpc, lr, #%0-7d"},
   { FLAGS_NONE, 0xf3808000, 0xffe0f000, "msr%c\t%C, %16-19r"},
   { FLAGS_MEMORY_ACCESS, 0xe8500f00, 0xfff00fff, "ldrex%c\t%12-15r, [%16-19r]"},
   { FLAGS_MEMORY_ACCESS, 0xe8d00f4f, 0xfff00fef, "ldrex%4?hb%c\t%12-15r, [%16-19r]"},
   { FLAGS_NONE, 0xe800c000, 0xffd0ffe0, "srsdb%c\t%16-19r%21'!, #%0-4d"},
   { FLAGS_NONE, 0xe980c000, 0xffd0ffe0, "srsia%c\t%16-19r%21'!, #%0-4d"},
   { FLAGS_NONE, 0xfa0ff080, 0xfffff0c0, "sxth%c.w\t%8-11r, %0-3r%R"},
   { FLAGS_NONE, 0xfa1ff080, 0xfffff0c0, "uxth%c.w\t%8-11r, %0-3r%R"},
   { FLAGS_NONE, 0xfa2ff080, 0xfffff0c0, "sxtb16%c\t%8-11r, %0-3r%R"},
   { FLAGS_NONE, 0xfa3ff080, 0xfffff0c0, "uxtb16%c\t%8-11r, %0-3r%R"},
   { FLAGS_NONE, 0xfa4ff080, 0xfffff0c0, "sxtb%c.w\t%8-11r, %0-3r%R"},
   { FLAGS_NONE, 0xfa5ff080, 0xfffff0c0, "uxtb%c.w\t%8-11r, %0-3r%R"},
   { FLAGS_MEMORY_ACCESS, 0xe8400000, 0xfff000ff, "strex%c\t%8-11r, %12-15r, [%16-19r]"},
   { FLAGS_MEMORY_ACCESS, 0xe8d0007f, 0xfff000ff, "ldrexd%c\t%12-15r, %8-11r, [%16-19r]"},
   { FLAGS_NONE, 0xfa80f000, 0xfff0f0f0, "sadd8%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfa80f010, 0xfff0f0f0, "qadd8%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfa80f020, 0xfff0f0f0, "shadd8%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfa80f040, 0xfff0f0f0, "uadd8%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfa80f050, 0xfff0f0f0, "uqadd8%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfa80f060, 0xfff0f0f0, "uhadd8%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfa80f080, 0xfff0f0f0, "qadd%c\t%8-11r, %0-3r, %16-19r"},
   { FLAGS_NONE, 0xfa80f090, 0xfff0f0f0, "qdadd%c\t%8-11r, %0-3r, %16-19r"},
   { FLAGS_NONE, 0xfa80f0a0, 0xfff0f0f0, "qsub%c\t%8-11r, %0-3r, %16-19r"},
   { FLAGS_NONE, 0xfa80f0b0, 0xfff0f0f0, "qdsub%c\t%8-11r, %0-3r, %16-19r"},
   { FLAGS_NONE, 0xfa90f000, 0xfff0f0f0, "sadd16%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfa90f010, 0xfff0f0f0, "qadd16%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfa90f020, 0xfff0f0f0, "shadd16%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfa90f040, 0xfff0f0f0, "uadd16%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfa90f050, 0xfff0f0f0, "uqadd16%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfa90f060, 0xfff0f0f0, "uhadd16%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfa90f080, 0xfff0f0f0, "rev%c.w\t%8-11r, %16-19r"},
   { FLAGS_NONE, 0xfa90f090, 0xfff0f0f0, "rev16%c.w\t%8-11r, %16-19r"},
   { FLAGS_NONE, 0xfa90f0a0, 0xfff0f0f0, "rbit%c\t%8-11r, %16-19r"},
   { FLAGS_NONE, 0xfa90f0b0, 0xfff0f0f0, "revsh%c.w\t%8-11r, %16-19r"},
   { FLAGS_NONE, 0xfaa0f000, 0xfff0f0f0, "sasx%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfaa0f010, 0xfff0f0f0, "qasx%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfaa0f020, 0xfff0f0f0, "shasx%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfaa0f040, 0xfff0f0f0, "uasx%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfaa0f050, 0xfff0f0f0, "uqasx%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfaa0f060, 0xfff0f0f0, "uhasx%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfaa0f080, 0xfff0f0f0, "sel%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfab0f080, 0xfff0f0f0, "clz%c\t%8-11r, %16-19r"},
   { FLAGS_NONE, 0xfac0f000, 0xfff0f0f0, "ssub8%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfac0f010, 0xfff0f0f0, "qsub8%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfac0f020, 0xfff0f0f0, "shsub8%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfac0f040, 0xfff0f0f0, "usub8%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfac0f050, 0xfff0f0f0, "uqsub8%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfac0f060, 0xfff0f0f0, "uhsub8%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfad0f000, 0xfff0f0f0, "ssub16%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfad0f010, 0xfff0f0f0, "qsub16%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfad0f020, 0xfff0f0f0, "shsub16%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfad0f040, 0xfff0f0f0, "usub16%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfad0f050, 0xfff0f0f0, "uqsub16%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfad0f060, 0xfff0f0f0, "uhsub16%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfae0f000, 0xfff0f0f0, "ssax%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfae0f010, 0xfff0f0f0, "qsax%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfae0f020, 0xfff0f0f0, "shsax%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfae0f040, 0xfff0f0f0, "usax%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfae0f050, 0xfff0f0f0, "uqsax%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfae0f060, 0xfff0f0f0, "uhsax%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfb00f000, 0xfff0f0f0, "mul%c.w\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfb70f000, 0xfff0f0f0, "usad8%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfa00f000, 0xffe0f0f0, "lsl%20's%c.w\t%8-11R, %16-19R, %0-3R"},
   { FLAGS_NONE, 0xfa20f000, 0xffe0f0f0, "lsr%20's%c.w\t%8-11R, %16-19R, %0-3R"},
   { FLAGS_NONE, 0xfa40f000, 0xffe0f0f0, "asr%20's%c.w\t%8-11R, %16-19R, %0-3R"},
   { FLAGS_NONE, 0xfa60f000, 0xffe0f0f0, "ror%20's%c.w\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xe8c00f40, 0xfff00fe0, "strex%4?hb%c\t%0-3r, %12-15r, [%16-19r]"},
   { FLAGS_NONE, 0xf3200000, 0xfff0f0e0, "ssat16%c\t%8-11r, #%0-4d, %16-19r"},
   { FLAGS_NONE, 0xf3a00000, 0xfff0f0e0, "usat16%c\t%8-11r, #%0-4d, %16-19r"},
   { FLAGS_NONE, 0xfb20f000, 0xfff0f0e0, "smuad%4'x%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfb30f000, 0xfff0f0e0, "smulw%4?tb%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfb40f000, 0xfff0f0e0, "smusd%4'x%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfb50f000, 0xfff0f0e0, "smmul%4'r%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xfa00f080, 0xfff0f0c0, "sxtah%c\t%8-11r, %16-19r, %0-3r%R"},
   { FLAGS_NONE, 0xfa10f080, 0xfff0f0c0, "uxtah%c\t%8-11r, %16-19r, %0-3r%R"},
   { FLAGS_NONE, 0xfa20f080, 0xfff0f0c0, "sxtab16%c\t%8-11r, %16-19r, %0-3r%R"},
   { FLAGS_NONE, 0xfa30f080, 0xfff0f0c0, "uxtab16%c\t%8-11r, %16-19r, %0-3r%R"},
   { FLAGS_NONE, 0xfa40f080, 0xfff0f0c0, "sxtab%c\t%8-11r, %16-19r, %0-3r%R"},
   { FLAGS_NONE, 0xfa50f080, 0xfff0f0c0, "uxtab%c\t%8-11r, %16-19r, %0-3r%R"},
   { FLAGS_NONE, 0xfb10f000, 0xfff0f0c0, "smul%5?tb%4?tb%c\t%8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xf36f0000, 0xffff8020, "bfc%c\t%8-11r, %E"},
   { FLAGS_NONE, 0xea100f00, 0xfff08f00, "tst%c.w\t%16-19r, %S"},
   { FLAGS_NONE, 0xea900f00, 0xfff08f00, "teq%c\t%16-19r, %S"},
   { FLAGS_NONE, 0xeb100f00, 0xfff08f00, "cmn%c.w\t%16-19r, %S"},
   { FLAGS_NONE, 0xebb00f00, 0xfff08f00, "cmp%c.w\t%16-19r, %S"},
   { FLAGS_NONE, 0xf0100f00, 0xfbf08f00, "tst%c.w\t%16-19r, %M"},
   { FLAGS_NONE, 0xf0900f00, 0xfbf08f00, "teq%c\t%16-19r, %M"},
   { FLAGS_NONE, 0xf1100f00, 0xfbf08f00, "cmn%c.w\t%16-19r, %M"},
   { FLAGS_NONE, 0xf1b00f00, 0xfbf08f00, "cmp%c.w\t%16-19r, %M"},
   { FLAGS_NONE, 0xea4f0000, 0xffef8000, "mov%20's%c.w\t%8-11r, %S"},
   { FLAGS_NONE, 0xea6f0000, 0xffef8000, "mvn%20's%c.w\t%8-11r, %S"},
   { FLAGS_MEMORY_ACCESS, 0xe8c00070, 0xfff000f0, "strexd%c\t%0-3r, %12-15r, %8-11r, [%16-19r]"},
   { FLAGS_NONE, 0xfb000000, 0xfff000f0, "mla%c\t%8-11r, %16-19r, %0-3r, %12-15r"},
   { FLAGS_NONE, 0xfb000010, 0xfff000f0, "mls%c\t%8-11r, %16-19r, %0-3r, %12-15r"},
   { FLAGS_NONE, 0xfb700000, 0xfff000f0, "usada8%c\t%8-11R, %16-19R, %0-3R, %12-15R"},
   { FLAGS_NONE, 0xfb800000, 0xfff000f0, "smull%c\t%12-15R, %8-11R, %16-19R, %0-3R"},
   { FLAGS_NONE, 0xfba00000, 0xfff000f0, "umull%c\t%12-15R, %8-11R, %16-19R, %0-3R"},
   { FLAGS_NONE, 0xfbc00000, 0xfff000f0, "smlal%c\t%12-15R, %8-11R, %16-19R, %0-3R"},
   { FLAGS_NONE, 0xfbe00000, 0xfff000f0, "umlal%c\t%12-15R, %8-11R, %16-19R, %0-3R"},
   { FLAGS_NONE, 0xfbe00060, 0xfff000f0, "umaal%c\t%12-15R, %8-11R, %16-19R, %0-3R"},
   { FLAGS_MEMORY_ACCESS, 0xe8500f00, 0xfff00f00, "ldrex%c\t%12-15r, [%16-19r, #%0-7W]"},
   { FLAGS_NONE, 0xf04f0000, 0xfbef8000, "mov%20's%c.w\t%8-11r, %M"},
   { FLAGS_NONE, 0xf06f0000, 0xfbef8000, "mvn%20's%c.w\t%8-11r, %M"},
   { FLAGS_NONE, 0xf810f000, 0xff70f000, "pld%c\t%a"},
   { FLAGS_NONE, 0xfb200000, 0xfff000e0, "smlad%4'x%c\t%8-11R, %16-19R, %0-3R, %12-15R"},
   { FLAGS_NONE, 0xfb300000, 0xfff000e0, "smlaw%4?tb%c\t%8-11R, %16-19R, %0-3R, %12-15R"},
   { FLAGS_NONE, 0xfb400000, 0xfff000e0, "smlsd%4'x%c\t%8-11R, %16-19R, %0-3R, %12-15R"},
   { FLAGS_NONE, 0xfb500000, 0xfff000e0, "smmla%4'r%c\t%8-11R, %16-19R, %0-3R, %12-15R"},
   { FLAGS_NONE, 0xfb600000, 0xfff000e0, "smmls%4'r%c\t%8-11R, %16-19R, %0-3R, %12-15R"},
   { FLAGS_NONE, 0xfbc000c0, 0xfff000e0, "smlald%4'x%c\t%12-15R, %8-11R, %16-19R, %0-3R"},
   { FLAGS_NONE, 0xfbd000c0, 0xfff000e0, "smlsld%4'x%c\t%12-15R, %8-11R, %16-19R, %0-3R"},
   { FLAGS_NONE, 0xeac00000, 0xfff08030, "pkhbt%c\t%8-11r, %16-19r, %S"},
   { FLAGS_NONE, 0xeac00020, 0xfff08030, "pkhtb%c\t%8-11r, %16-19r, %S"},
   { FLAGS_NONE, 0xf3400000, 0xfff08020, "sbfx%c\t%8-11r, %16-19r, %F"},
   { FLAGS_NONE, 0xf3c00000, 0xfff08020, "ubfx%c\t%8-11r, %16-19r, %F"},
   { FLAGS_NONE, 0xf8000e00, 0xff900f00, "str%wt%c\t%12-15r, %a"},
   { FLAGS_NONE, 0xfb100000, 0xfff000c0, "smla%5?tb%4?tb%c\t%8-11r, %16-19r, %0-3r, %12-15r"},
   { FLAGS_NONE, 0xfbc00080, 0xfff000c0, "smlal%5?tb%4?tb%c\t%12-15r, %8-11r, %16-19r, %0-3r"},
   { FLAGS_NONE, 0xf3600000, 0xfff08020, "bfi%c\t%8-11r, %16-19r, %E"},
   { FLAGS_NONE, 0xf8100e00, 0xfe900f00, "ldr%wt%c\t%12-15r, %a"},
   { FLAGS_NONE, 0xf3000000, 0xffd08020, "ssat%c\t%8-11r, #%0-4d, %16-19r%s"},
   { FLAGS_NONE, 0xf3800000, 0xffd08020, "usat%c\t%8-11r, #%0-4d, %16-19r%s"},
   { FLAGS_NONE, 0xf2000000, 0xfbf08000, "addw%c\t%8-11r, %16-19r, %I"},
   { FLAGS_NONE, 0xf2400000, 0xfbf08000, "movw%c\t%8-11r, %J"},
   { FLAGS_NONE, 0xf2a00000, 0xfbf08000, "subw%c\t%8-11r, %16-19r, %I"},
   { FLAGS_NONE, 0xf2c00000, 0xfbf08000, "movt%c\t%8-11r, %J"},
   { FLAGS_NONE, 0xea000000, 0xffe08000, "and%20's%c.w\t%8-11r, %16-19r, %S"},
   { FLAGS_NONE, 0xea200000, 0xffe08000, "bic%20's%c.w\t%8-11r, %16-19r, %S"},
   { FLAGS_NONE, 0xea400000, 0xffe08000, "orr%20's%c.w\t%8-11r, %16-19r, %S"},
   { FLAGS_NONE, 0xea600000, 0xffe08000, "orn%20's%c\t%8-11r, %16-19r, %S"},
   { FLAGS_NONE, 0xea800000, 0xffe08000, "eor%20's%c.w\t%8-11r, %16-19r, %S"},
   { FLAGS_NONE, 0xeb000000, 0xffe08000, "add%20's%c.w\t%8-11r, %16-19r, %S"},
   { FLAGS_NONE, 0xeb400000, 0xffe08000, "adc%20's%c.w\t%8-11r, %16-19r, %S"},
   { FLAGS_NONE, 0xeb600000, 0xffe08000, "sbc%20's%c.w\t%8-11r, %16-19r, %S"},
   { FLAGS_NONE, 0xeba00000, 0xffe08000, "sub%20's%c.w\t%8-11r, %16-19r, %S"},
   { FLAGS_NONE, 0xebc00000, 0xffe08000, "rsb%20's%c\t%8-11r, %16-19r, %S"},
   { FLAGS_MEMORY_ACCESS, 0xe8400000, 0xfff00000, "strex%c\t%8-11r, %12-15r, [%16-19r, #%0-7W]"},
   { FLAGS_NONE, 0xf0000000, 0xfbe08000, "and%20's%c.w\t%8-11r, %16-19r, %M"},
   { FLAGS_NONE, 0xf0200000, 0xfbe08000, "bic%20's%c.w\t%8-11r, %16-19r, %M"},
   { FLAGS_NONE, 0xf0400000, 0xfbe08000, "orr%20's%c.w\t%8-11r, %16-19r, %M"},
   { FLAGS_NONE, 0xf0600000, 0xfbe08000, "orn%20's%c\t%8-11r, %16-19r, %M"},
   { FLAGS_NONE, 0xf0800000, 0xfbe08000, "eor%20's%c.w\t%8-11r, %16-19r, %M"},
   { FLAGS_NONE, 0xf1000000, 0xfbe08000, "add%20's%c.w\t%8-11r, %16-19r, %M"},
   { FLAGS_NONE, 0xf1400000, 0xfbe08000, "adc%20's%c.w\t%8-11r, %16-19r, %M"},
   { FLAGS_NONE, 0xf1600000, 0xfbe08000, "sbc%20's%c.w\t%8-11r, %16-19r, %M"},
   { FLAGS_NONE, 0xf1a00000, 0xfbe08000, "sub%20's%c.w\t%8-11r, %16-19r, %M"},
   { FLAGS_NONE, 0xf1c00000, 0xfbe08000, "rsb%20's%c\t%8-11r, %16-19r, %M"},
   { FLAGS_MEMORY_ACCESS, 0xe8800000, 0xffd00000, "stmia%c.w\t%16-19r%21'!, %m"},
   { FLAGS_MEMORY_ACCESS, 0xe8900000, 0xffd00000, "ldmia%c.w\t%16-19r%21'!, %m"},
   { FLAGS_MEMORY_ACCESS, 0xe9000000, 0xffd00000, "stmdb%c\t%16-19r%21'!, %m"},
   { FLAGS_MEMORY_ACCESS, 0xe9100000, 0xffd00000, "ldmdb%c\t%16-19r%21'!, %m"},
   { FLAGS_MEMORY_ACCESS, 0xe9c00000, 0xffd000ff, "strd%c\t%12-15r, %8-11r, [%16-19r]"},
   { FLAGS_MEMORY_ACCESS, 0xe9d00000, 0xffd000ff, "ldrd%c\t%12-15r, %8-11r, [%16-19r]"},
   { FLAGS_MEMORY_ACCESS, 0xe9400000, 0xff500000, "strd%c\t%12-15r, %8-11r, [%16-19r, #%23`-%0-7W]%21'!%L"},
   { FLAGS_MEMORY_ACCESS, 0xe9500000, 0xff500000, "ldrd%c\t%12-15r, %8-11r, [%16-19r, #%23`-%0-7W]%21'!%L"},
   { FLAGS_MEMORY_ACCESS, 0xe8600000, 0xff700000, "strd%c\t%12-15r, %8-11r, [%16-19r], #%23`-%0-7W%L"},
   { FLAGS_MEMORY_ACCESS, 0xe8700000, 0xff700000, "ldrd%c\t%12-15r, %8-11r, [%16-19r], #%23`-%0-7W%L"},
   { FLAGS_MEMORY_ACCESS, 0xf8000000, 0xff100000, "str%w%c.w\t%12-15r, %a"},
   { FLAGS_MEMORY_ACCESS, 0xf8100000, 0xfe100000, "ldr%w%c.w\t%12-15r, %a"},

   /* Filter out Bcc with cond=E or F, which are used for other instructions.  */
   { FLAGS_NONE, 0xf3c08000, 0xfbc0d000, "undefined (bcc, cond=0xF)"},
   { FLAGS_NONE, 0xf3808000, 0xfbc0d000, "undefined (bcc, cond=0xE)"},
   { FLAGS_NONE, 0xf0008000, 0xf800d000, "b%22-25c.w\t%b%X"},
   { FLAGS_NONE, 0xf0009000, 0xf800d000, "b%c.w\t%B%x"},

   /* These have been 32-bit since the invention of Thumb.  */
   { FLAGS_NONE, 0xf000c000, 0xf800d001, "blx%c\t%B%x"},
   { FLAGS_NONE, 0xf000d000, 0xf800d000, "bl%c\t%B%x"},

   /* Fallback.  */
   { FLAGS_NONE, 0x00000000, 0x00000000, UNDEFINED_INSTRUCTION},
   { FLAGS_NONE, 0, 0, 0}
};

static const char *const arm_conditional[] = {
   "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc",
   "hi", "ls", "ge", "lt", "gt", "le", "al", "<und>", ""
};

static const char *const arm_fp_const[] =
{"0.0", "1.0", "2.0", "3.0", "4.0", "5.0", "0.5", "10.0"};

static const char *const arm_shift[] =
{"lsl", "lsr", "asr", "ror"};

typedef struct {
   const char *name;
   const char *description;
   const char *reg_names[16];
}
arm_regname;

static const arm_regname regnames[] = {
   {
      "raw" , "Select raw register names",
      { "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"}
   },
   {
      "gcc",  "Select register names used by GCC",
      { "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "sl",  "fp",  "ip",  "sp",  "lr",  "pc" }
   },
   {
      "std",  "Select register names used in ARM's ISA documentation",
      { "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11", "r12", "sp",  "lr",  "pc" }
   },
   {
      "apcs", "Select register names used in the APCS",
      { "a1", "a2", "a3", "a4", "v1", "v2", "v3", "v4", "v5", "v6", "sl",  "fp",  "ip",  "sp",  "lr",  "pc" }
   },
   {
      "atpcs", "Select register names used in the ATPCS",
      { "a1", "a2", "a3", "a4", "v1", "v2", "v3", "v4", "v5", "v6", "v7",  "v8",  "IP",  "SP",  "LR",  "PC" }
   },
   {
      "special-atpcs", "Select special register names used in the ATPCS",
      { "a1", "a2", "a3", "a4", "v1", "v2", "v3", "WR", "v5", "SB", "SL",  "FP",  "IP",  "SP",  "LR",  "PC" }
   },
};

static const char *const iwmmxt_wwnames[] =
{"b", "h", "w", "d"};

static const char *const iwmmxt_wwssnames[] = {
   "b", "bus", "bc", "bss",
   "h", "hus", "hc", "hss",
   "w", "wus", "wc", "wss",
   "d", "dus", "dc", "dss"
};

static const char *const iwmmxt_regnames[] = {
   "wr0", "wr1", "wr2", "wr3", "wr4", "wr5", "wr6", "wr7",
   "wr8", "wr9", "wr10", "wr11", "wr12", "wr13", "wr14", "wr15"
};

static const char *const iwmmxt_cregnames[] = {
   "wcid", "wcon", "wcssf", "wcasf", "reserved", "reserved", "reserved", "reserved",
   "wcgr0", "wcgr1", "wcgr2", "wcgr3", "reserved", "reserved", "reserved", "reserved"
};

/* Default to GCC register name set.  */
static unsigned int regname_selected = 1;

#define NUM_ARM_REGNAMES  NUM_ELEM (regnames)
#define arm_regnames      regnames[regname_selected].reg_names

static bool force_thumb = false;

/* Current IT instruction state.  This contains the same state as the IT
   bits in the CPSR.  */
static unsigned int ifthen_state;
/* IT state for the next instruction.  */
static unsigned int ifthen_next_state;
/* The address of the insn for which the IT state is valid.  */
static bfd_vma ifthen_address;
#define IFTHEN_COND ((ifthen_state >> 4) & 0xf)
/* Indicates that the current Conditional state is unconditional or outside
   an IT block.  */
#define COND_UNCOND 16

static void
print_insn_thumb16 (uint32_t pc, struct disassemble_info *info, uint32_t given)
{
   const struct opcode16 *insn;
   void *stream = info->stream;
   fprintf_ftype func = info->fprintf_func;
   for (insn = thumb_opcodes; insn->assembler; insn++) {
      info->flags = insn->flags;
      if ((given & insn->mask) == insn->value) {
         signed long value_in_comment = 0;
         const char *c = insn->assembler;

         for (; *c; c++) {
            int domaskpc = 0;
            int domasklr = 0;

            if (*c != '%') {
               func (stream, "%c", *c);
               continue;
            }

            switch (*++c) {
            case '%':
               func (stream, "%%");
               break;

            case 'c':
               if (ifthen_state)
                  func (stream, "%s", arm_conditional[IFTHEN_COND]);
               break;

            case 'C':
               if (ifthen_state)
                  func (stream, "%s", arm_conditional[IFTHEN_COND]);
               else
                  func (stream, "s");
               break;

            case 'I': {
               unsigned int tmp;

               ifthen_next_state = given & 0xff;
               for (tmp = given << 1; tmp & 0xf; tmp <<= 1)
                  func (stream, ((given ^ tmp) & 0x10) ? "e" : "t");
               func (stream, "\t%s", arm_conditional[(given >> 4) & 0xf]);
            }
            break;

            case 'x':
               if (ifthen_next_state)
                  func (stream, "\t; unpredictable branch in IT block\n");
               break;

            case 'X':
               if (ifthen_state)
                  func (stream, "\t; unpredictable <IT:%s>",
                        arm_conditional[IFTHEN_COND]);
               break;

            case 'S': {
               long reg;

               reg = (given >> 3) & 0x7;
               if (given & (1 << 6))
                  reg += 8;

               func (stream, "%s", arm_regnames[reg]);
            }
            break;

            case 'D': {
               long reg;

               reg = given & 0x7;
               if (given & (1 << 7))
                  reg += 8;

               func (stream, "%s", arm_regnames[reg]);
            }
            break;

            case 'N':
               if (given & (1 << 8))
                  domasklr = 1;
            /* Fall through.  */
            case 'O':
               if (*c == 'O' && (given & (1 << 8)))
                  domaskpc = 1;
            /* Fall through.  */
            case 'M': {
               int started = 0;
               int reg;

               func (stream, "{");

               /* It would be nice if we could spot
                  ranges, and generate the rS-rE format: */
               for (reg = 0; (reg < 8); reg++)
                  if ((given & (1 << reg)) != 0) {
                     if (started)
                        func (stream, ", ");
                     started = 1;
                     func (stream, "%s", arm_regnames[reg]);
                  }

               if (domasklr) {
                  if (started)
                     func (stream, ", ");
                  started = 1;
                  func (stream, "%s", arm_regnames[14] /* "lr" */);
               }

               if (domaskpc) {
                  if (started)
                     func (stream, ", ");
                  func (stream, "%s", arm_regnames[15] /* "pc" */);
               }

               func (stream, "}");
            }
            break;

            case 'W':
               /* Print writeback indicator for a LDMIA.  We are doing a
                  writeback if the base register is not in the register
                  mask.  */
               if ((given & (1 << ((given & 0x0700) >> 8))) == 0)
                  func (stream, "!");
               break;

            case 'b':
               /* Print ARM V6T2 CZB address: pc+4+6 bits.  */
            {
               bfd_vma address = (pc + 4
                                  + ((given & 0x00f8) >> 2)
                                  + ((given & 0x0200) >> 3));
               info->print_address_func (address, info);
            }
            break;

            case 's':
               /* Right shift immediate -- bits 6..10; 1-31 print
                  as themselves, 0 prints as 32.  */
            {
               long imm = (given & 0x07c0) >> 6;
               if (imm == 0)
                  imm = 32;
               func (stream, "#%ld", imm);
            }
            break;

            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9': {
               int bitstart = *c++ - '0';
               int bitend = 0;

               while (*c >= '0' && *c <= '9')
                  bitstart = (bitstart * 10) + *c++ - '0';

               switch (*c) {
               case '-': {
                  bfd_vma reg;

                  c++;
                  while (*c >= '0' && *c <= '9')
                     bitend = (bitend * 10) + *c++ - '0';
                  if (!bitend)
                     abort ();
                  reg = given >> bitstart;
                  reg &= (2 << (bitend - bitstart)) - 1;

                  switch (*c) {
                  case 'r':
                     func (stream, "%s", arm_regnames[reg]);
                     break;

                  case 'd':
                     func (stream, "%ld", (long) reg);
                     value_in_comment = reg;
                     break;

                  case 'H':
                     func (stream, "%ld", (long) (reg << 1));
                     value_in_comment = reg << 1;
                     break;

                  case 'W':
                     func (stream, "%ld", (long) (reg << 2));
                     value_in_comment = reg << 2;
                     break;

                  case 'a':
                     /* PC-relative address -- the bottom two
                        bits of the address are dropped
                        before the calculation.  */
                     info->print_address_func
                     (((pc + 4) & ~3) + (reg << 2), info);
                     value_in_comment = 0;
                     break;

                  case 'x':
                     func (stream, "0x%04lx", (long) reg);
                     break;

                  case 'B':
                     reg = ((reg ^ (1 << bitend)) - (1 << bitend));
                     info->print_address_func (reg * 2 + pc + 4, info);
                     value_in_comment = 0;
                     break;

                  case 'c':
                     func (stream, "%s", arm_conditional [reg]);
                     break;

                  default:
                     abort ();
                  }
               }
               break;

               case '\'':
                  c++;
                  if ((given & (1 << bitstart)) != 0)
                     func (stream, "%c", *c);
                  break;

               case '?':
                  ++c;
                  if ((given & (1 << bitstart)) != 0)
                     func (stream, "%c", *c++);
                  else
                     func (stream, "%c", *++c);
                  break;

               default:
                  abort ();
               }
            }
            break;

            default:
               abort ();
            }
         }

         if (value_in_comment > 32 || value_in_comment < -16)
            func (stream, "\t; 0x%lx", value_in_comment);
         return;
      }
   }
   /* No match.  */
   abort ();
}

/* Return the name of a v7A special register.  */

static const char *
banked_regname (unsigned reg)
{
   switch (reg) {
   case 15:
      return "CPSR";
   case 32:
      return "R8_usr";
   case 33:
      return "R9_usr";
   case 34:
      return "R10_usr";
   case 35:
      return "R11_usr";
   case 36:
      return "R12_usr";
   case 37:
      return "SP_usr";
   case 38:
      return "LR_usr";
   case 40:
      return "R8_fiq";
   case 41:
      return "R9_fiq";
   case 42:
      return "R10_fiq";
   case 43:
      return "R11_fiq";
   case 44:
      return "R12_fiq";
   case 45:
      return "SP_fiq";
   case 46:
      return "LR_fiq";
   case 48:
      return "LR_irq";
   case 49:
      return "SP_irq";
   case 50:
      return "LR_svc";
   case 51:
      return "SP_svc";
   case 52:
      return "LR_abt";
   case 53:
      return "SP_abt";
   case 54:
      return "LR_und";
   case 55:
      return "SP_und";
   case 60:
      return "LR_mon";
   case 61:
      return "SP_mon";
   case 62:
      return "ELR_hyp";
   case 63:
      return "SP_hyp";
   case 79:
      return "SPSR";
   case 110:
      return "SPSR_fiq";
   case 112:
      return "SPSR_irq";
   case 114:
      return "SPSR_svc";
   case 116:
      return "SPSR_abt";
   case 118:
      return "SPSR_und";
   case 124:
      return "SPSR_mon";
   case 126:
      return "SPSR_hyp";
   default:
      return NULL;
   }
}

/* Return the name of the DMB/DSB option.  */
static const char *
data_barrier_option (unsigned option)
{
   switch (option & 0xf) {
   case 0xf:
      return "sy";
   case 0xe:
      return "st";
   case 0xd:
      return "ld";
   case 0xb:
      return "ish";
   case 0xa:
      return "ishst";
   case 0x9:
      return "ishld";
   case 0x7:
      return "un";
   case 0x6:
      return "unst";
   case 0x5:
      return "nshld";
   case 0x3:
      return "osh";
   case 0x2:
      return "oshst";
   case 0x1:
      return "oshld";
   default:
      return NULL;
   }
}


/* Return the name of an V7M special register.  */

static const char *
psr_name (int regno)
{
   switch (regno) {
   case 0:
      return "APSR";
   case 1:
      return "IAPSR";
   case 2:
      return "EAPSR";
   case 3:
      return "PSR";
   case 5:
      return "IPSR";
   case 6:
      return "EPSR";
   case 7:
      return "IEPSR";
   case 8:
      return "MSP";
   case 9:
      return "PSP";
   case 16:
      return "PRIMASK";
   case 17:
      return "BASEPRI";
   case 18:
      return "BASEPRI_MAX";
   case 19:
      return "FAULTMASK";
   case 20:
      return "CONTROL";
   default:
      return "<unknown>";
   }
}

/* Decode a bitfield of the form matching regexp (N(-N)?,)*N(-N)?.
   Returns pointer to following character of the format string and
   fills in *VALUEP and *WIDTHP with the extracted value and number of
   bits extracted.  WIDTHP can be NULL.  */

static const char *
arm_decode_bitfield (const char *ptr,
                     unsigned long insn,
                     unsigned long *valuep,
                     int *widthp)
{
   unsigned long value = 0;
   int width = 0;

   do {
      int start, end;
      int bits;

      for (start = 0; *ptr >= '0' && *ptr <= '9'; ptr++)
         start = start * 10 + *ptr - '0';
      if (*ptr == '-')
         for (end = 0, ptr++; *ptr >= '0' && *ptr <= '9'; ptr++)
            end = end * 10 + *ptr - '0';
      else
         end = start;
      bits = end - start;
      if (bits < 0)
         abort ();
      value |= ((insn >> start) & ((2ul << bits) - 1)) << width;
      width += bits + 1;
   } while (*ptr++ == ',');
   *valuep = value;
   if (widthp)
      *widthp = width;
   return ptr - 1;
}

/* Print one 32-bit Thumb instruction from PC on INFO->STREAM.  */

static void
print_insn_thumb32 (bfd_vma pc, struct disassemble_info *info, long given)
{
   const struct opcode32 *insn;
   void *stream = info->stream;
   fprintf_ftype func = info->fprintf_func;

   //if (print_insn_coprocessor (pc, info, given, TRUE))
   //  return;

   for (insn = thumb32_opcodes; insn->assembler; insn++) {
      info->flags = insn->flags;
      if ((given & insn->mask) == insn->value) {
         bfd_boolean is_unpredictable = FALSE;
         signed long value_in_comment = 0;
         const char *c = insn->assembler;

         for (; *c; c++) {
            if (*c != '%') {
               func (stream, "%c", *c);
               continue;
            }

            switch (*++c) {
            case '%':
               func (stream, "%%");
               break;

            case 'c':
               if (ifthen_state)
                  func (stream, "%s", arm_conditional[IFTHEN_COND]);
               break;

            case 'x':
               if (ifthen_next_state)
                  func (stream, "\t; unpredictable branch in IT block\n");
               break;

            case 'X':
               if (ifthen_state)
                  func (stream, "\t; unpredictable <IT:%s>",
                        arm_conditional[IFTHEN_COND]);
               break;

            case 'I': {
               unsigned int imm12 = 0;

               imm12 |= (given & 0x000000ffu);
               imm12 |= (given & 0x00007000u) >> 4;
               imm12 |= (given & 0x04000000u) >> 15;
               func (stream, "#%u", imm12);
               value_in_comment = imm12;
            }
            break;

            case 'M': {
               unsigned int bits = 0, imm, imm8, mod;

               bits |= (given & 0x000000ffu);
               bits |= (given & 0x00007000u) >> 4;
               bits |= (given & 0x04000000u) >> 15;
               imm8 = (bits & 0x0ff);
               mod = (bits & 0xf00) >> 8;
               switch (mod) {
               case 0:
                  imm = imm8;
                  break;
               case 1:
                  imm = ((imm8 << 16) | imm8);
                  break;
               case 2:
                  imm = ((imm8 << 24) | (imm8 << 8));
                  break;
               case 3:
                  imm = ((imm8 << 24) | (imm8 << 16) | (imm8 << 8) | imm8);
                  break;
               default:
                  mod  = (bits & 0xf80) >> 7;
                  imm8 = (bits & 0x07f) | 0x80;
                  imm  = (((imm8 << (32 - mod)) | (imm8 >> mod)) & 0xffffffff);
               }
               func (stream, "#%u", imm);
               value_in_comment = imm;
            }
            break;

            case 'J': {
               unsigned int imm = 0;

               imm |= (given & 0x000000ffu);
               imm |= (given & 0x00007000u) >> 4;
               imm |= (given & 0x04000000u) >> 15;
               imm |= (given & 0x000f0000u) >> 4;
               func (stream, "#%u", imm);
               value_in_comment = imm;
            }
            break;

            case 'K': {
               unsigned int imm = 0;

               imm |= (given & 0x000f0000u) >> 16;
               imm |= (given & 0x00000ff0u) >> 0;
               imm |= (given & 0x0000000fu) << 12;
               func (stream, "#%u", imm);
               value_in_comment = imm;
            }
            break;

            case 'H': {
               unsigned int imm = 0;

               imm |= (given & 0x000f0000u) >> 4;
               imm |= (given & 0x00000fffu) >> 0;
               func (stream, "#%u", imm);
               value_in_comment = imm;
            }
            break;

            case 'V': {
               unsigned int imm = 0;

               imm |= (given & 0x00000fffu);
               imm |= (given & 0x000f0000u) >> 4;
               func (stream, "#%u", imm);
               value_in_comment = imm;
            }
            break;

            case 'S': {
               unsigned int reg = (given & 0x0000000fu);
               unsigned int stp = (given & 0x00000030u) >> 4;
               unsigned int imm = 0;
               imm |= (given & 0x000000c0u) >> 6;
               imm |= (given & 0x00007000u) >> 10;

               func (stream, "%s", arm_regnames[reg]);
               switch (stp) {
               case 0:
                  if (imm > 0)
                     func (stream, ", lsl #%u", imm);
                  break;

               case 1:
                  if (imm == 0)
                     imm = 32;
                  func (stream, ", lsr #%u", imm);
                  break;

               case 2:
                  if (imm == 0)
                     imm = 32;
                  func (stream, ", asr #%u", imm);
                  break;

               case 3:
                  if (imm == 0)
                     func (stream, ", rrx");
                  else
                     func (stream, ", ror #%u", imm);
               }
            }
            break;

            case 'a': {
               unsigned int Rn  = (given & 0x000f0000) >> 16;
               unsigned int U   = ! NEGATIVE_BIT_SET;
               unsigned int op  = (given & 0x00000f00) >> 8;
               unsigned int i12 = (given & 0x00000fff);
               unsigned int i8  = (given & 0x000000ff);
               bfd_boolean writeback = FALSE, postind = FALSE;
               bfd_vma offset = 0;

               func (stream, "[%s", arm_regnames[Rn]);
               if (U) { /* 12-bit positive immediate offset.  */
                  offset = i12;
                  if (Rn != 15)
                     value_in_comment = offset;
               } else if (Rn == 15) /* 12-bit negative immediate offset.  */
                  offset = - (int) i12;
               else if (op == 0x0) { /* Shifted register offset.  */
                  unsigned int Rm = (i8 & 0x0f);
                  unsigned int sh = (i8 & 0x30) >> 4;

                  func (stream, ", %s", arm_regnames[Rm]);
                  if (sh)
                     func (stream, ", lsl #%u", sh);
                  func (stream, "]");
                  break;
               } else switch (op) {
                  case 0xE:  /* 8-bit positive immediate offset.  */
                     offset = i8;
                     break;

                  case 0xC:  /* 8-bit negative immediate offset.  */
                     offset = -i8;
                     break;

                  case 0xF:  /* 8-bit + preindex with wb.  */
                     offset = i8;
                     writeback = TRUE;
                     break;

                  case 0xD:  /* 8-bit - preindex with wb.  */
                     offset = -i8;
                     writeback = TRUE;
                     break;

                  case 0xB:  /* 8-bit + postindex.  */
                     offset = i8;
                     postind = TRUE;
                     break;

                  case 0x9:  /* 8-bit - postindex.  */
                     offset = -i8;
                     postind = TRUE;
                     break;

                  default:
                     func (stream, ", <undefined>]");
                     goto skip;
                  }

               if (postind)
                  func (stream, "], #%d", (int) offset);
               else {
                  if (offset)
                     func (stream, ", #%d", (int) offset);
                  func (stream, writeback ? "]!" : "]");
               }

               if (Rn == 15) {
                  func (stream, "\t; ");
                  info->print_address_func (((pc + 4) & ~3) + offset, info);
               }
            }
skip:
            break;

            case 'A': {
               unsigned int U   = ! NEGATIVE_BIT_SET;
               unsigned int W   = WRITEBACK_BIT_SET;
               unsigned int Rn  = (given & 0x000f0000) >> 16;
               unsigned int off = (given & 0x000000ff);

               func (stream, "[%s", arm_regnames[Rn]);

               if (PRE_BIT_SET) {
                  if (off || !U) {
                     func (stream, ", #%c%u", U ? '+' : '-', off * 4);
                     value_in_comment = off * 4 * U ? 1 : -1;
                  }
                  func (stream, "]");
                  if (W)
                     func (stream, "!");
               } else {
                  func (stream, "], ");
                  if (W) {
                     func (stream, "#%c%u", U ? '+' : '-', off * 4);
                     value_in_comment = off * 4 * U ? 1 : -1;
                  } else {
                     func (stream, "{%u}", off);
                     value_in_comment = off;
                  }
               }
            }
            break;

            case 'w': {
               unsigned int Sbit = (given & 0x01000000) >> 24;
               unsigned int type = (given & 0x00600000) >> 21;

               switch (type) {
               case 0:
                  func (stream, Sbit ? "sb" : "b");
                  break;
               case 1:
                  func (stream, Sbit ? "sh" : "h");
                  break;
               case 2:
                  if (Sbit)
                     func (stream, "??");
                  break;
               case 3:
                  func (stream, "??");
                  break;
               }
            }
            break;

            case 'm': {
               int started = 0;
               int reg;

               func (stream, "{");
               for (reg = 0; reg < 16; reg++)
                  if ((given & (1 << reg)) != 0) {
                     if (started)
                        func (stream, ", ");
                     started = 1;
                     func (stream, "%s", arm_regnames[reg]);
                  }
               func (stream, "}");
            }
            break;

            case 'E': {
               unsigned int msb = (given & 0x0000001f);
               unsigned int lsb = 0;

               lsb |= (given & 0x000000c0u) >> 6;
               lsb |= (given & 0x00007000u) >> 10;
               func (stream, "#%u, #%u", lsb, msb - lsb + 1);
            }
            break;

            case 'F': {
               unsigned int width = (given & 0x0000001f) + 1;
               unsigned int lsb = 0;

               lsb |= (given & 0x000000c0u) >> 6;
               lsb |= (given & 0x00007000u) >> 10;
               func (stream, "#%u, #%u", lsb, width);
            }
            break;

            case 'b': {
               unsigned int S = (given & 0x04000000u) >> 26;
               unsigned int J1 = (given & 0x00002000u) >> 13;
               unsigned int J2 = (given & 0x00000800u) >> 11;
               bfd_vma offset = 0;

               offset |= !S << 20;
               offset |= J2 << 19;
               offset |= J1 << 18;
               offset |= (given & 0x003f0000) >> 4;
               offset |= (given & 0x000007ff) << 1;
               offset -= (1 << 20);

               info->print_address_func (pc + 4 + offset, info);
            }
            break;

            case 'B': {
               unsigned int S = (given & 0x04000000u) >> 26;
               unsigned int I1 = (given & 0x00002000u) >> 13;
               unsigned int I2 = (given & 0x00000800u) >> 11;
               bfd_vma offset = 0;

               offset |= !S << 24;
               offset |= !(I1 ^ S) << 23;
               offset |= !(I2 ^ S) << 22;
               offset |= (given & 0x03ff0000u) >> 4;
               offset |= (given & 0x000007ffu) << 1;
               offset -= (1 << 24);
               offset += pc + 4;

               /* BLX target addresses are always word aligned.  */
               if ((given & 0x00001000u) == 0)
                  offset &= ~2u;

               info->print_address_func (offset, info);
            }
            break;

            case 's': {
               unsigned int shift = 0;

               shift |= (given & 0x000000c0u) >> 6;
               shift |= (given & 0x00007000u) >> 10;
               if (WRITEBACK_BIT_SET)
                  func (stream, ", asr #%u", shift);
               else if (shift)
                  func (stream, ", lsl #%u", shift);
               /* else print nothing - lsl #0 */
            }
            break;

            case 'R': {
               unsigned int rot = (given & 0x00000030) >> 4;

               if (rot)
                  func (stream, ", ror #%u", rot * 8);
            }
            break;

            case 'U':
               if ((given & 0xf0) == 0x60) {
                  switch (given & 0xf) {
                  case 0xf:
                     func (stream, "sy");
                     break;
                  default:
                     func (stream, "#%d", (int) given & 0xf);
                     break;
                  }
               } else {
                  const char * opt = data_barrier_option (given & 0xf);
                  if (opt != NULL)
                     func (stream, "%s", opt);
                  else
                     func (stream, "#%d", (int) given & 0xf);
               }
               break;

            case 'C':
               if ((given & 0xff) == 0) {
                  func (stream, "%cPSR_", (given & 0x100000) ? 'S' : 'C');
                  if (given & 0x800)
                     func (stream, "f");
                  if (given & 0x400)
                     func (stream, "s");
                  if (given & 0x200)
                     func (stream, "x");
                  if (given & 0x100)
                     func (stream, "c");
               } else if ((given & 0x20) == 0x20) {
                  char const* name;
                  unsigned sysm = (given & 0xf00) >> 8;

                  sysm |= (given & 0x30);
                  sysm |= (given & 0x00100000) >> 14;
                  name = banked_regname (sysm);

                  if (name != NULL)
                     func (stream, "%s", name);
                  else
                     func (stream, "(UNDEF: %lu)", (unsigned long) sysm);
               } else {
                  func (stream, "%s", psr_name (given & 0xff));
               }
               break;

            case 'D':
               if (((given & 0xff) == 0)
                   || ((given & 0x20) == 0x20)) {
                  char const* name;
                  unsigned sm = (given & 0xf0000) >> 16;

                  sm |= (given & 0x30);
                  sm |= (given & 0x00100000) >> 14;
                  name = banked_regname (sm);

                  if (name != NULL)
                     func (stream, "%s", name);
                  else
                     func (stream, "(UNDEF: %lu)", (unsigned long) sm);
               } else
                  func (stream, "%s", psr_name (given & 0xff));
               break;

            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9': {
               int width;
               unsigned long val;

               c = arm_decode_bitfield (c, given, &val, &width);

               switch (*c) {
               case 'd':
                  func (stream, "%lu", val);
                  value_in_comment = val;
                  break;

               case 'W':
                  func (stream, "%lu", val * 4);
                  value_in_comment = val * 4;
                  break;

               case 'S':
                  if (val == 13)
                     is_unpredictable = TRUE;
               /* Fall through.  */
               case 'R':
                  if (val == 15)
                     is_unpredictable = TRUE;
               /* Fall through.  */
               case 'r':
                  func (stream, "%s", arm_regnames[val]);
                  break;

               case 'c':
                  func (stream, "%s", arm_conditional[val]);
                  break;

               case '\'':
                  c++;
                  if (val == ((1ul << width) - 1))
                     func (stream, "%c", *c);
                  break;

               case '`':
                  c++;
                  if (val == 0)
                     func (stream, "%c", *c);
                  break;

               case '?':
                  func (stream, "%c", c[(1 << width) - (int) val]);
                  c += 1 << width;
                  break;

               case 'x':
                  func (stream, "0x%lx", val & 0xffffffffUL);
                  break;

               default:
                  abort ();
               }
            }
            break;

            case 'L':
               /* PR binutils/12534
                  If we have a PC relative offset in an LDRD or STRD
                  instructions then display the decoded address.  */
               if (((given >> 16) & 0xf) == 0xf) {
                  bfd_vma offset = (given & 0xff) * 4;

                  if ((given & (1 << 23)) == 0)
                     offset = - offset;
                  func (stream, "\t; ");
                  info->print_address_func ((pc & ~3) + 4 + offset, info);
               }
               break;

            default:
               abort ();
            }
         }

         if (value_in_comment > 32 || value_in_comment < -16)
            func (stream, "\t; 0x%lx", value_in_comment);

         if (is_unpredictable)
            func (stream, UNPREDICTABLE_INSTRUCTION);

         return;
      }
   }
   /* No match.  */
   abort ();
}

static uint32_t address_reference[1024];
static uint32_t address_reference_count = 0;

static void print_addr(uint32_t address, const struct disassemble_info *self)
{
   if (self->flags & FLAGS_MEMORY_ACCESS) {
      address_reference[address_reference_count] = address;
      address_reference_count = (address_reference_count + 1) % sizeof(address_reference);
   }
   self->fprintf_func(self->stream, "0x%08X", address);
}

static bool is_address_reference(uint32_t addr)
{
   for (size_t i=0; i<address_reference_count; i++) {
      if (address_reference[i] == addr) {
         return true;
      }
   }
   return false;
}

ptrdiff_t do_disassemble(uintptr_t addr)
{
   struct disassemble_info dinfo = {
      .stream = stdout,
      .fprintf_func = (fprintf_ftype) fprintf,
      .print_address_func = print_addr,
   };
   addr &= ~1UL;
   uint16_t instr = *((uint16_t*)addr);

   uint8_t b15_11 = instr >> 11;
   uint8_t b15_13 = b15_11 >> 2;
   bool is_t32 = (b15_11 != 0x1c) && (b15_13 == 0x7);
   printf("0x%08X ", addr);
   if (is_address_reference(addr)) {
      uint32_t val = *((uint32_t*)addr);
      printf("%08X ; constant\n", val);
      return sizeof(uint32_t);
   } else if (is_t32) {
      uint16_t instr2 = *((uint16_t*)(addr + 2));
      uint32_t instr32 = (instr << 16) | instr2;
      printf("%08X ", instr32);
      print_insn_thumb32(addr, &dinfo, instr32);
   } else {
      printf("%04X     ", instr);
      print_insn_thumb16(addr, &dinfo, instr);
   }
   printf("\n");
   return is_t32? sizeof(uint32_t) : sizeof(uint16_t);
}
