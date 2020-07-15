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
      type = *params++;
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
         if (sscanf(params, "%i", &addr) != 1) {
            printf("Cannot decode %s as addr\n", params);
            return;
         }
         params = nextword(params);
         if (*params) {
            if (sscanf(params, "%i", &count) != 1) {
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
         if (sscanf(params, "%i", &addr) != 1) {
            printf("Cannot decode %s as addr\n", params);
            return;
         }
         params = nextword(params);
         if (*params) {
            if (sscanf(params, "%i", &count) != 1) {
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
   return (sscanf(str, "%i", &val) != 1)? defVal : val;
}

static void writeMemory(size_t typeSize, uint32_t addr, uint32_t data)
{
   memcpy((void*) addr, &data, typeSize);
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
         "  h:                       This help\n"
         "  x [c|w|h|b] [addr] [n]:  Hex dump at addr, <n> bytes\n"
         "  d [addr] [n]:            Disassemble from addr <n> bytes\n"
         "  a [addr]:                Assemble into addr\n"
         "  l [file] [addr] [n]:     Load <n> bytes from file to addr\n"
         "  s [file] [addr] [n]:     Store <n> bytes in at addr file\n"
         "  w [w|h|b] [addr] [data]: Write memory type <data> at <addr>\n"
         "  c <a addr> <b addr> <n>: Compare <n> bytes from A to B <addr>\n"
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
      printf("TODO\n");
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
   printf("Debugger console\n");
   while (1)
      repl();
   return 0;
}
