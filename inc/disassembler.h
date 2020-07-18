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

#ifndef __DISASSEMBLER_H__
#define __DISASSEMBLER_H__

#include <stdint.h>
#include <stddef.h>

typedef int (*fprintf_ftype)(void *fp, const char *const fmt, ...);

struct disassemble_info {
   void *stream;
   fprintf_ftype fprintf_func;
   void (*print_address_func)(uint32_t address, const struct disassemble_info *self);
   uint8_t flags;
};

ptrdiff_t do_disassemble(uintptr_t addr);

#endif /* __DISASSEMBLER_H__ */
