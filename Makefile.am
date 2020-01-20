# Goals
# Copyright (C) 2019-2020 Richard W.M. Jones
# Copyright (C) 2019-2020 Red Hat Inc.
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

# Pass through normal targets to Goalfile.in
#
# These will override the automake targets printing warnings which
# can be ignored.

all clean depend install check \
dist distcheck \
maintainer-clean \
maintainer-commit maintainer-tag \
maintainer-check-extra-dist maintainer-release \
maintainer-srpm maintainer-fedora-copr: src/goals
	@./run src/goals $@ 'DESTDIR="$(DESTDIR)"'

# If src/goals doesn't exist then brute-force build it.  Once we have
# src/goals we can rebuild it and other parts of the project using
# goals itself (see Goalfile.in).

src/goals:
	$(MENHIR) --explain src/parser.mly
# Hack required to break circular dependencies.
	echo 'val lexer_read : (Lexing.lexbuf -> token) option ref' >> src/parser.mli
	echo 'val eval_substitute : (Ast.env -> Ast.loc -> Ast.substs -> string) option ref' >> src/parser.mli
	$(OCAMLLEX) src/lexer.mll
	$(OCAMLFIND) opt $(OCAMLFLAGS) $(OCAMLPACKAGES) -I . -I src \
	    src/utils-c.c \
	    $$($(OCAMLDEP) -sort src/*.mli src/*.ml) \
	    -linkpkg -o $@
