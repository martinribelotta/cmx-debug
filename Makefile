# Copyright (C) 2020 Martin Ribelotta.
#
# This file is part of tdebug.
#
# This library is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# It is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
# License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
# MA 02110-1301, USA.
#

TARGET:=tdebug

OUT:=build

INCLUDES:=inc
LIB_PATH:=lib

SPECS:=nano rdimon
LDSCRIPTS?=lib/cmx-generic.ld

ARCH_FLAGS:=-mcpu=cortex-m3 -mthumb

PIC:=-fpie -mno-pic-data-is-text-relative -mno-single-pic-base

CFLAGS:=$(ARCH_FLAGS)
CFLAGS+=-Og -g3
CFLAGS+=$(foreach i, $(INCLUDES), -I$(i))
CFLAGS+=$(foreach d, $(DEFINES), -D$(d))
CFLAGS+=-fdata-sections -ffunction-sections

LDFLAGS:=$(ARCH_FLAGS)
LDFLAGS+=$(foreach L, $(LIB_PATH), -L$(L))
LDFLAGS+=$(foreach l, $(LIBRARIES), -l$(l))
LDFLAGS+=$(foreach s, $(SPECS), --specs=$(s).specs)
LDFLAGS+=$(foreach t, $(LDSCRIPTS), -T$(t))
LDFLAGS+=-Wl,--gc-sections
LDFLAGS+=-Wl,--print-memory-usage

CROSS:=arm-none-eabi-
CC:=$(CROSS)gcc
LD:=$(CROSS)gcc
OD:=$(CROSS)objdump -dsxS
OCP:=$(CROSS)objcopy -O binary -S
MD:=mkdir -p
RM:=rm -fr

ifeq ($(VERBOSE),y)
Q:=
else
Q:=@
endif

SOURCES:=$(wildcard src/*.c)
OBJECTS:=$(addprefix $(OUT)/, $(patsubst src/%.c, %.o, $(SOURCES)))

ELF:=$(addprefix $(OUT)/, $(addsuffix .elf, $(TARGET)))
LST:=$(patsubst %.elf, %.lst, $(ELF))
BIN:=$(patsubst %.elf, %.bin, $(ELF))

all: $(ELF) $(LST) $(BIN)

$(OUT)/:
	@echo mkdir $@
	$(Q)$(MD) $@

$(OUT)/%.o: src/%.c | $(OUT)/
	@echo CC $(notdir $<)
	$(Q)$(CC) -c $(CFLAGS) -o $@ $<

$(ELF): $(OBJECTS) $(LDSCRIPTS) | $(OUT)/
	@echo LD $(notdir $@)
	$(Q)$(LD) -o $@ $(LDFLAGS) $(OBJECTS)

$(LST): $(ELF) | $(OUT)/
	@echo GEN $(notdir $@)
	$(Q)$(OD) $< > $@

$(BIN): $(ELF) | $(OUT)/
	@echo GEN $(notdir $@)
	$(Q)$(OCP) $< $@

clean:
	@echo CLEAN
	$(Q)$(RM) $(OUT)/

run: $(BIN)
	openocd -d0 -c "set __ELF_FILE__ $<" -f scripts/run-bluepill2.cfg

debug: $(ELF)
	openocd -d0 -f scripts/run-bluepill2-dbg.cfg
