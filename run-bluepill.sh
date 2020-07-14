#!/bin/sh

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

set -x
B=$(dirname $(realpath $0))
F=${B}/build/tdebug.bin
S=${B}/run-bluepill.cfg
CMD="openocd -d0 -c \"set __ELF_FILE__ ${F}\" -f ${S}"
#x-terminal-emulator -e "make -C ${B}/../../../ ; ${CMD}; echo 'Press enter to close'; read"
x-terminal-emulator -e "make ; cd ${B}; ${CMD}; echo 'Press enter to close'; read"
