# Goals stdlib shell prelude.
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

# This file is included in all shell scripts, and is used to define
# various common functions.

# This should be used to print the single parameter as a quoted string
# suitable for consuming in a goal expression.
function quoted_string ()
{
    # XXX This doesn't actually do quoting XXX
    echo -n "\"$1\""
}

# For printing strings in different colours.
function print_red ()
{
    echo -e "\x1b[1;31m$@\x1b[0m"
}

function print_green ()
{
    echo -e "\x1b[0;32m$@\x1b[0m"
}
