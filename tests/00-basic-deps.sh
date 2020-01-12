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

goals -f 00-basic-deps.gl > 00-basic-deps.out

mv 00-basic-deps.out 00-basic-deps.out.old
grep ^g < 00-basic-deps.out.old > 00-basic-deps.out

lines=`wc -l 00-basic-deps.out | awk '{print $1}'`
for (( i=1; $i <= $lines; i++ )); do
    # Read the i'th line.
    prefix=$( sed "${i}q;d" 00-basic-deps.out )
    # Read the i+1'th to end lines, and check each one does NOT
    # have $prefix as a prefix.
    sed -n "$((i+1))~1p" 00-basic-deps.out | grep $prefix && exit 1 ||:
done

rm 00-basic-deps.out 00-basic-deps.out.old
