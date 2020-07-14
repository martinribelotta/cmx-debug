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
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#include "disassembler.h"

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

static void setgetptr(const char *params)
{
   if (*params) {
      uintptr_t addr;
      if (sscanf(params, "%i", &addr) != 1) {
         printf("Error reading ip: %s\n", params);
         return;
      }
      as_ptr = addr;
   }
   printf("(dis)assembler addres: 0x%08X\n", as_ptr);
}

static void assemble(const char *params)
{
   // TODO
   puts("TODO");
}

static void disassemble(const char *params)
{
   ptrdiff_t count = 32;
   if (*params) {
      setgetptr(params);
      params = nextword(params);
      if (*params) {
         if (sscanf(params, "%i", &count) != 1) {
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
   ptrdiff_t count = 32;
   uintptr_t addr = as_ptr;
   char type = 'b';
   if (*params) {
      type = *params;
      params = nextword(params);
      if (*params) {
         if (sscanf(params, "%i", &addr) != 1) {
            printf("error decoding addr %s\n", params);
            return;
         }
         params = nextword(params);
         if (*params) {
            if (sscanf(params, "%i", &count) != 1) {
               printf("error decoding count %s\n", params);
               return;
            }
         }
      }
   }
   while (count) {
      if ((addr % 16) == 0) {
         printf("\n%08X: ", addr);
      }
      switch(type) {
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
   }
   printf("\n");
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
         "  h:                        this help\n"
         "  x [w|h|b] [addr] [count]: hex dump\n"
         "  d [addr] [count]:         disassemble\n"
         "  a [addr]:                 assemble\n"
      );
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
   case '\0':
      // EOL
      break;
   default:
      printf("Unrecognized command: %s\n", ptr);
      break;
   }
}

int main()
{
   printf("Debugger console\n");
   while (1)
      repl();
   return 0;
}
