# Goals
# Copyright (C) 2019 Richard W.M. Jones
# Copyright (C) 2019 Red Hat Inc.
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

AC_INIT([goals], [0.1])
AC_CONFIG_MACRO_DIR([m4])

AC_PROG_INSTALL

dnl Check for basic C environment.
AC_PROG_CC_STDC
AC_PROG_INSTALL
AC_PROG_CPP

AC_C_PROTOTYPES
test "x$U" != "x" && AC_MSG_ERROR([Compiler not ANSI compliant])

dnl C headers and functions.
AC_CHECK_HEADERS([sys/sysinfo.h])
AC_CHECK_FUNCS([get_nprocs])

dnl Check for basic OCaml environment and findlib.
AC_PROG_OCAML
if test "x$OCAMLC" = "xno"; then
    AC_MSG_ERROR([OCaml compiler is required])
fi

dnl Check for required programs.
AC_PROG_FINDLIB
if test "x$OCAMLFIND" = "xno"; then
    AC_MSG_ERROR([OCaml findlib is required])
fi
AC_PROG_OCAMLLEX
if test "x$OCAMLLEX" = "xno"; then
    AC_MSG_ERROR([OCaml ocamllex lexical generator tool is required])
fi

AC_CHECK_PROG(MENHIR, [menhir], [menhir], [
    AC_MSG_ERROR([OCaml menhir parser generator is required])
])

AC_CHECK_PROG([POD2MAN], [pod2man], [pod2man], [
    AC_MSG_ERROR([pod2man tool from Perl is required])
])

AC_CHECK_PROG([POD2TEXT], [pod2text], [pod2text], [
    AC_MSG_ERROR([pod2text tool from Perl is required])
])

dnl Substitute OCaml flags and packages.
AC_SUBST([OCAMLFLAGS], ["-g -safe-string -warn-error CDEFLMPSUVYZX+52-3"])
AC_SUBST([OCAMLPACKAGES], ["-package str,unix,threads -thread"])

dnl Produce output files.
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([stamp-h], [echo timestamp > stamp-h])
AC_CONFIG_FILES([run], [chmod +x,-w run])
AC_CONFIG_FILES([Goalfile Makefile
                 goals.spec
                 src/config.ml])

AC_OUTPUT
