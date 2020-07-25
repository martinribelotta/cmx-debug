/* Copyright (C) 2020 Martin Ribelotta.

   This file is part of tdebug.

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

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <unctrl.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include "assembler.h"
#include "disassembler.h"
#include "fault.h"
#include "ucmsis.h"

#define ARRAYCOUNT(arr)  (sizeof(arr) / sizeof(*(arr)))
#define FOREACH(e, arr) for (typeof(*arr) *e = arr; e < arr + ARRAYCOUNT(arr); e++)

static uint32_t toInt(const char *str, uint32_t defVal);

typedef struct {
   const char *symbol;
   uint32_t value;
} const_symbol_t;

static const const_symbol_t symbol_table[] = {
   { "SCB.ACTLR", 0xE000E008 },
   { "SCB.CPUID", 0xE000ED00 },
   { "SCB.ICSR",  0xE000ED04 },
   { "SCB.VTOR",  0xE000ED08 },
   { "SCB.AIRCR", 0xE000ED0C },
   { "SCB.SCR",   0xE000ED10 },
   { "SCB.CCR",   0xE000ED14 },
   { "SCB.SHPR1", 0xE000ED18 },
   { "SCB.SHPR2", 0xE000ED1C },
   { "SCB.SHPR3", 0xE000ED20 },
   { "SCB.SHCRS", 0xE000ED24 },
   { "SCB.CFSR",  0xE000ED28 },
   { "SCB.MMSR",  0xE000ED28 },
   { "SCB.BFSR",  0xE000ED29 },
   { "SCB.UFSR",  0xE000ED2A },
   { "SCB.HFSR",  0xE000ED2C },
   { "SCB.MMAR",  0xE000ED34 },
   { "SCB.BFAR",  0xE000ED38 },
   { "SCB.AFSR",  0xE000ED3C },
};

static uintptr_t as_ptr = 0;

static void promt(void)
{
   printf("DEBUG.\n> ");
   fflush(stdout);
}

static const char *skipspaces(const char *s)
{
   while (*s && isspace(*s))
      s++;
   return s;
}

static const char *skipword(const char *s)
{
   while (*s && !isspace(*s))
      s++;
   return s;
}

static const char *nextword(const char *s)
{
   return skipspaces(skipword(s));
}

static const int cmpword(const char *s, const char *w)
{
   while (*s && *w && !isspace(*s)) {
      if (*s != *w)
         return *s - *w;
      s++;
      w++;
   }
   return 0;
}

static bool const2addr(const char *s, uint32_t *addr)
{
   FOREACH(e, symbol_table) {
      if (cmpword(s, e->symbol) == 0) {
         *addr = e->value;
         return true;
      }
   }
   return false;
}

static bool str2addr(const char *s, uint32_t *addr)
{
   if (!*s)
      return false;
#define FALLTHROUGH __attribute__((fallthrough))
   int radix = 10;
   if (*s == '0') {
      switch (*++s) {
      case 'X': FALLTHROUGH;
      case 'x': radix = 16; s++; break;
      case 'B': FALLTHROUGH;
      case 'b': radix = 2; s++; break;
      case 'o': FALLTHROUGH;
      case 'O': s++; FALLTHROUGH;
      default: radix = 8; break;
      }
   } else if (*s == '$') {
      return const2addr(++s, addr);
   }
#undef FALLTHROUGH
   uint32_t acc = 0;
   while (*s && !isspace(*s)) {
      int c = *s++;
      if (isdigit(c)) {
         c -= '0';
      } else if (isalpha(c)) {
         c -= isupper(c)? 'A' - 10 : 'a' - 10;
      } else
         return false;
      if (c >= radix)
         return false;
      acc *= radix;
      acc += c;
   }
   *addr = acc;
   return true;
}

static void setgetptr(const char *params)
{
   if (*params) {
      uint32_t addr;
      if (!str2addr(params, &addr)) {
         printf("Error reading ip: %s\n", params);
         return;
      }
      as_ptr = addr;
   }
   printf("(dis)assembler addres: 0x%08X\n", as_ptr);
}

static void assemble(const char *params)
{
   static char line[80];
   uint32_t addr = toInt(params, as_ptr);
   while (1) {
      printf("0x%08X> ", addr);
      fflush(stdout);
      fgets(line, sizeof(line) - 1, stdin);
      if (!line[0] || line[0] == '\n')
         break;
      if (assemble_line(line, addr)) {
         addr += do_disassemble(addr);
      } else {
         printf("ASM ERROR: %s\n", assembler_error());
      }
   }
}

static void disassemble(const char *params)
{
   uint32_t count = 32;
   if (*params) {
      setgetptr(params);
      params = nextword(params);
      if (*params) {
         if (str2addr(params, &count) != 1) {
            printf("Cannot decode count %s\n", params);
            return;
         }
      }
   }
   while (count--)
      as_ptr += do_disassemble(as_ptr);
}

static void hexdump(const char *params)
{
   uint32_t count = 1;
   uint32_t addr = as_ptr;
   char type = 'b';
   if (*params) {
      type = *params++;
      params = nextword(params);
      if (*params) {
         if (str2addr(params, &addr) != 1) {
            printf("error decoding addr %s\n", params);
            return;
         }
         params = nextword(params);
         if (*params) {
            if (str2addr(params, &count) != 1) {
               printf("error decoding count %s\n", params);
               return;
            }
         }
      }
   }
   printf("\n%08X: ", addr);
   while (count) {
      switch(type) {
      case 'c':
         if (isprint(*((char*)addr))) {
            printf("%c", *((char*)addr));
         } else {
            printf("\u2592");
         }
         addr += sizeof(char);
         break;
      case 'w':
         printf("%08X ", *((uint32_t*)addr));
         addr += sizeof(uint32_t);
         break;
      case 'h':
         printf("%04X ", *((uint16_t*)addr));
         addr += sizeof(uint16_t);
         break;
      default:
      case 'b':
         printf("%02X ", *((uint8_t*)addr));
         addr += sizeof(uint8_t);
         break;
      }
      count--;
      if (count && ((addr % 16) == 0))
         printf("\n%08X: ", addr);
   }
   printf("\n");
}

static void load_file(const char *params)
{
   static char filename[64] = "dump.bin";
   uint32_t addr = as_ptr;
   uint32_t count = 32;
   if (*params) {
      char *ptr = filename;
      const char *src = params;
      params = skipword(params);
      while (src < params)
         *ptr++ = *src++;
      *ptr = 0;
      params = skipspaces(params);
      if (*params) {
         if (str2addr(params, &addr) != 1) {
            printf("Cannot decode %s as addr\n", params);
            return;
         }
         params = nextword(params);
         if (*params) {
            if (str2addr(params, &count) != 1) {
               printf("Cannot decode %s as count\n", params);
               return;
            }
         }
      }
   }
   printf("loading %s\n", filename);
   int fd = open(filename, O_RDONLY);
   if (fd == -1) {
      printf("error open: %s\n", strerror(errno));
      return;
   }
   count = read(fd, (void*) addr, count);
   close(fd);
   if (count == -1) {
      printf("error read: %s\n", strerror(errno));
      return;
   }
   printf("Readed %d bytes into 0x%08X\n", count, addr);
}

static void store_file(const char *params)
{
   static char filename[64] = "dump.bin";
   uint32_t addr = as_ptr;
   uint32_t count = 32;
   if (*params) {
      char *ptr = filename;
      const char *src = params;
      params = skipword(params);
      while (src < params)
         *ptr++ = *src++;
      *ptr = 0;
      params = skipspaces(params);
      if (*params) {
         if (str2addr(params, &addr) != 1) {
            printf("Cannot decode %s as addr\n", params);
            return;
         }
         params = nextword(params);
         if (*params) {
            if (str2addr(params, &count) != 1) {
               printf("Cannot decode %s as count\n", params);
               return;
            }
         }
      }
   }
   printf("dumping %s\n", filename);
   int fd = open(filename, O_WRONLY);
   if (fd == -1) {
      printf("error open: %s\n", strerror(errno));
      return;
   }
   count = write(fd, (void*) addr, count);
   close(fd);
   if (count == -1) {
      printf("error write: %s\n", strerror(errno));
      return;
   }
   printf("Writed %d bytes from 0x%08X to file %s\n", count, addr, filename);
}

static size_t typeSize(char c)
{
   switch(c) {
   default:
   case 'b': return sizeof(uint8_t);
   case 'h': return sizeof(uint16_t);
   case 'w': return sizeof(uint32_t);
   }
}

static uint32_t toInt(const char *str, uint32_t defVal)
{
   uint32_t val;
   return (str2addr(str, &val) != 1)? defVal : val;
}

static void writeMemory(size_t typeSize, uint32_t addr, uint32_t data)
{
   memcpy((void*) addr, &data, typeSize);
}

static void memoryCompare(uint32_t a_addr, uint32_t b_addr, uint32_t count)
{
   uint8_t *a = (uint8_t*) a_addr;
   uint8_t *b = (uint8_t*) b_addr;
   for (uint32_t i = 0; i < count; i++) {
      if (a[i] != b[i]) {
         printf("DIFF a[0x%08X]:0x%02X != b[0x%08X]:0x%02X\n", &a[i], a[i], &b[i], b[i]);
         return;
      }
   }
   printf("EQUAL\n");
}

static void executeTo(uint32_t codePtr, uint32_t spAddr)
{
   codePtr |= 1; // Ensure thumb instruction
   if (spAddr != -1) {
      __asm__ volatile("mov sp, %0": : "r" (spAddr));
   }
   void (*func)(void) = (void(*)(void)) codePtr;
   func();
}

__attribute__((optimize(3), noreturn))
void executeBoot(uint32_t vtoraddr)
{
   __asm__ volatile ("cpsid i" : : : "memory");
   // Disable all interrupts manually
   for (int i = 0; i < 8; i++)
      NVIC->ICER[i] = 0xFFFFFFFF;
   // Clear pending IRQs
   for (int i = 0; i < 8; i++)
      NVIC->ICPR[i] = 0xFFFFFFFF;
   SCB->VTOR = vtoraddr;
   uint32_t sp = ((volatile uint32_t*) vtoraddr)[0];
   uint32_t pc = ((volatile uint32_t*) vtoraddr)[1];
   __set_CONTROL(__get_CONTROL() & ~1UL);
   __set_MSP(sp);
   __asm__ volatile("bx\t%0"::"r" (pc));
   __builtin_unreachable();
}

static void repl(void)
{
   static char line[128];
   promt();
   do {
      line[0] = 0;
      fgets(line, sizeof(line) - 1, stdin);
   } while (line[0] == 0);
   const char *ptr = skipspaces(line);
   switch (*ptr) {
   case 'h':
   case 'H':
      puts(
         "HELP:\n"
         "  h[h]:                    This help. One h is more vervose\n"
         "  x [c|w|h|b] [addr] [n]:  Hex dump at addr, <n> bytes\n"
         "  d [addr] [n]:            Disassemble from addr <n> bytes\n"
         "  a [addr]:                Assemble into addr\n"
         "  l [file] [addr] [n]:     Load <n> bytes from file to addr\n"
         "  s [file] [addr] [n]:     Store <n> bytes in at addr file\n"
         "  w [w|h|b] [addr] [data]: Write memory type <data> at <addr>\n"
         "  c <a addr> <b addr> <n>: Compare <n> bytes from A to B <addr>\n"
         "  g <addr> [sp]:           Execute <addr>. if specify set stack to [sp]\n"
         "  gb <vector>:             Execute app from VTOR addr in <vector>\n\n"
      );
      printf("The param <addr> may be specify a named constant using $<name>\n");
      if (ptr[1] == 'h' || ptr[1] == 'H') {
         printf("known constants for <addr>:\n");
         FOREACH(e, symbol_table)
            printf("%-12s 0x%08X\n", e->symbol, e->value);
      }
      break;
   case 'x':
   case 'X':
      // hex dump
      hexdump(nextword(ptr));
      break;
   case 'a':
   case 'A':
      // Assemble
      assemble(nextword(ptr));
      break;
   case 'd':
   case 'D':
      // Dissassemble
      disassemble(nextword(ptr));
      break;
   case 'p':
   case 'P':
      // Set/get pointer
      setgetptr(nextword(ptr));
      break;
   case 'l':
   case 'L':
      load_file(nextword(ptr));
      break;
   case 's':
   case 'S':
      store_file(nextword(ptr));
      break;
   case 'w':
      writeMemory(typeSize(*(ptr=nextword(ptr))), toInt(ptr=nextword(ptr), as_ptr), toInt(ptr=nextword(ptr), 0));
      break;
   case 'c':
      memoryCompare(toInt(ptr=nextword(ptr), as_ptr), toInt(ptr=nextword(ptr), as_ptr), toInt(ptr=nextword(ptr), 1));
      break;
   case 'g':
      if (ptr[1] == 'b') {
         executeBoot(toInt(ptr=nextword(ptr), as_ptr));
      } else {
         executeTo(toInt(ptr=nextword(ptr), as_ptr), toInt(ptr=nextword(ptr), -1));
      }
      break;
   case '\0':
      // EOL
      break;
   default:
      printf("Unrecognized command: %s\n", ptr);
      break;
   }
}

extern void initialise_monitor_handles(void);

int main()
{
   initialise_monitor_handles();
   fault_init();
   printf("Debugger console\n");
   while (1)
      repl();
   return 0;
}
