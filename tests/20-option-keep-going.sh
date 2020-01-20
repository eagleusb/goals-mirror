#!/usr/bin/env bash
# Goals test.
# Copyright (C) 2020 Richard W.M. Jones
# Copyright (C) 2020 Red Hat Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

set -e

if goals -k -f 20-option-keep-going.gl > 20-option-keep-going.out 2>&1; then
    echo "$0: expected goals to exit with an error"
    exit 1
fi
fgrep FAIL2 20-option-keep-going.out
fgrep FAIL3 20-option-keep-going.out
fgrep GOOD3 20-option-keep-going.out
fgrep GOOD4 20-option-keep-going.out
fgrep GOOD5 20-option-keep-going.out
fgrep GOOD6 20-option-keep-going.out
fgrep GOOD7 20-option-keep-going.out
fgrep GOOD8 20-option-keep-going.out
fgrep GOOD9 20-option-keep-going.out
fgrep GOOD10 20-option-keep-going.out
fgrep GOOD11 20-option-keep-going.out
fgrep GOOD12 20-option-keep-going.out
fgrep GOOD13 20-option-keep-going.out
fgrep GOOD14 20-option-keep-going.out
fgrep GOOD15 20-option-keep-going.out
fgrep GOOD16 20-option-keep-going.out
fgrep GOOD17 20-option-keep-going.out
fgrep GOOD18 20-option-keep-going.out
fgrep GOOD19 20-option-keep-going.out
fgrep GOOD20 20-option-keep-going.out
! fgrep ALL 20-option-keep-going.out
! fgrep FAIL1 20-option-keep-going.out
! fgrep GOOD1 20-option-keep-going.out
! fgrep GOOD2 20-option-keep-going.out
rm 20-option-keep-going.out
