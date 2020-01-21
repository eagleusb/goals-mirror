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

goals -s -f 00-basic-predicate.gl > 00-basic-predicate.out

for animal in daisy dylan florence; do
    if test "$(grep "$animal" 00-basic-predicate.out)" != "$animal is a sheep
$animal needs transformation
transform sheep $animal to cow $animal
$animal is a cow"; then
        echo "$0: output failed for $animal:"
        cat 00-basic-predicate.out
        exit 1
    fi
done

rm 00-basic-predicate.out
