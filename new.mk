install_prefix = $(CURDIR)/_install
dune = $(CURDIR)/../dune/dune.exe
export ARCH = amd64

ocaml/Makefile.config:
	cd ocaml && ./configure \
	   --prefix='$(install_prefix)' \
           --disable-flambda \
           --disable-ocamldoc \
           --enable-ocamltest

ocamlopt_flags.sexp: ocaml/Makefile.config
# natdynlinkops2
	cat ocaml/Makefile.config \
	  | sed 's/^NATDYNLINKOPTS=$$/NATDYNLINKOPTS=-g/' \
	  | grep '^NATDYNLINKOPTS=' \
	  | sed 's/^[^=]*=\(.*\)/-ccopt\n"\1"/' \
	  > ocaml/otherlibs/dynlink/natdynlinkops
	/bin/echo -n $$(cat ocaml/Makefile.config \
	  | sed 's/^NATDYNLINKOPTS=$$/NATDYNLINKOPTS=-bin-annot/' \
	  | grep '^NATDYNLINKOPTS=' \
	  | sed 's/^[^=]*=\(.*\)/\1/') \
	  > ocaml/otherlibs/dynlink/natdynlinkops2
	if [ "$$(cat ocaml/otherlibs/dynlink/natdynlinkops2)" \
	       != "-bin-annot" ]; \
	then \
	  /bin/echo -n "-ccopt" > ocaml/otherlibs/dynlink/natdynlinkops1; \
	else \
	  /bin/echo -n "-bin-annot" > ocaml/otherlibs/dynlink/natdynlinkops1; \
	fi
# flags.sexp
	grep -q '^FUNCTION_SECTIONS=true' ocaml/Makefile.config; \
	if [ $$? -eq 0 ] ; then \
	  /bin/echo -n "(:standard -function-sections)" > ocamlopt_flags.sexp; \
	else \
	  /bin/echo -n "(:standard)" > ocamlopt_flags.sexp; \
	fi
	# note: it looks like the use of "$(...)" with a command spanning over
	# two lines triggers a bug in GNU make 3.81, that will as a consequence
	# change the file name. It also looks like the bug is not triggered by
	# "`...`".
	/bin/echo -n "( `grep \"^OC_CFLAGS=\" ocaml/Makefile.config \
		  	| sed 's/^OC_CFLAGS=//'` )" > oc_cflags.sexp
	/bin/echo -n "( `grep \"^OC_CPPFLAGS=\" ocaml/Makefile.config \
		| sed 's/^OC_CPPFLAGS=//'` )" > oc_cppflags.sexp
	/bin/echo -n "( `grep \"^SHAREDLIB_CFLAGS=\" ocaml/Makefile.config \
		| sed 's/^SHAREDLIB_CFLAGS=//'` )" > sharedlib_cflags.sexp

_boot/_bootinstall: ocaml/Makefile.config
	mkdir -p _boot/_bootinstall/{bin,lib/ocaml}
	cp $^ _boot/_bootinstall/lib/ocaml
	ln -sf ../../default/boot_ocamlopt.exe _boot/_bootinstall/bin/ocamlopt.opt
	ln -sf ../../default/ocaml/main_native.exe _boot/_bootinstall/bin/ocamlc.opt
	ln -sf ../../default/ocaml/tools/ocamlmklib_native.exe _boot/_bootinstall/bin/ocamlmklib.opt
	ln -sf "`which ocamllex`" _boot/_bootinstall/bin/ocamllex.opt
	ln -sf "`which ocamldep`" _boot/_bootinstall/bin/ocamldep.opt
	for prog in ocamlopt ocamlc ocamllex ocamldep ocamlmklib; do \
	  ln -sf "$$prog.opt" "_boot/_bootinstall/bin/$$prog"; \
	done

.PHONY: boot-compiler runtime-stdlib
boot-compiler: ocamlopt_flags.sexp _boot/_bootinstall
	$(dune) build --profile=release --root=. --build-dir=_boot \
	  --trace-file trace.json \
	  boot_ocamlopt.exe \
	  ocaml/main_native.exe \
	  ocaml/tools/ocamlmklib_native.exe

runtime-stdlib: boot-compiler
	eval $$(opam env --revert); \
        OCAMLLIB="$(CURDIR)/_boot/_bootinstall/lib/ocaml" \
	PATH="$(CURDIR)/_boot/_bootinstall/bin:$$PATH" \
	$(dune) build --profile=release --root=. --build-dir=_buildNew --only-package=ocaml_runtime_stdlib @install

compiler: runtime-stdlib
	touch "$(CURDIR)/_buildNew/install/default/lib/ocaml_runtime_stdlib/dynlink.cmxa"
	eval $$(opam env --revert); \
	OCAMLLIB="$(CURDIR)/_buildNew/install/default/lib/ocaml_runtime_stdlib/" \
	PATH="$(CURDIR)/_boot/_bootinstall/bin:$$PATH" \
	$(dune) build --profile=release --root=. --build-dir=_build3 \
	  --only-package=ocaml @install ocaml/ocamltest/ocamltest.byte

exec: compiler
		eval $$(opam env --revert); \
	OCAMLLIB="$(CURDIR)/_buildNew/install/default/lib/ocaml_runtime_stdlib/" \
	PATH="$(CURDIR)/_boot/_bootinstall/bin:$$PATH" \
	bash

# FIXME:
#  ocamlprof, ocamlcp
#  delete all compiler-libs cmos except topstart.cmo
#  link topdirs/opttopdirs
.PHONY: _build_install
_build_install:
	rm -rf _build_install
	mkdir -p _build_install/{bin,lib/ocaml}
	cp -l {_build3,_buildNew}/install/default/bin/* _build_install/bin/
	( cd _build_install/bin; \
	  for i in *.opt; do ln -s $$i $${i%.opt}; done )
	cp -lR _buildNew/install/default/lib/ocaml_runtime_stdlib/* \
	   _build_install/lib/ocaml/
	rm -f _build_install/lib/ocaml/{META,Makefile.config,dune-package,dynlink.cmxa}
	cp -lR _build3/install/default/lib/ocaml/* \
	   _build_install/lib/ocaml/
	rm -f _build_install/lib/ocaml/{META,dune-package}

stage2_prefix = _build3/install/default
stage2_build = _build3/default
middle_end = closure

stdlib_prefix = _buildNew/install/default

# The following horror will be removed when work to allow the testsuite to
# run on an installed tree (led by David Allsopp) is completed.
# stage2 needs to have been built first.
.PHONY: runtest-upstream
runtest-upstream:
	rm -rf _runtest
	mkdir _runtest
	ln -s ../_build_install _runtest/_install
	cp -a ocaml/testsuite _runtest/testsuite
	 # replace backend-specific testsuite/tools with their new versions
	rm _runtest/testsuite/tools/*
	cp -a testsuite/tools/* _runtest/testsuite/tools/
	 # replace backend-specific testsuite/tests/asmgen with their new versions
	rm _runtest/testsuite/tests/asmgen/*
	cp -a testsuite/tests/asmgen/* _runtest/testsuite/tests/asmgen/

	(cd _runtest && ln -s ../ocaml/Makefile.tools Makefile.tools)
	(cd _runtest && ln -s ../ocaml/Makefile.build_config Makefile.build_config)
	(cd _runtest && ln -s ../ocaml/Makefile.config_if_required Makefile.config_if_required)
	(cd _runtest && ln -s ../ocaml/Makefile.config Makefile.config)

	(cd _runtest; \
	 for exe in _install/bin/*; do ln -s $$exe; done; \
	 for exe in ocamlc ocamlopt ocamllex; do \
	   rm -f $$exe; ln -s $$exe.byte $$exe; \
	 done; \
	 ln -s _install/lib/ocaml stdlib; \
	 mkdir runtime; \
	 for f in ocamlrun* stdlib/caml stdlib/stublibs/*; do \
	   ln -s ../$$f runtime/`basename $$f`; \
	 done; \
	 ln -s . lex; ln -s . yacc; \
	 ln -s _install/lib/ocaml/compiler-libs compilerlibs; \
	 mkdir otherlibs; \
	 ln -s ../stdlib/threads otherlibs/systhreads; \
	 mkdir otherlibs/unix; \
	 cp -l stdlib/{lib,}unix* otherlibs/unix; \
	 mkdir otherlibs/dynlink; \
	 cp -l stdlib/dynlink* otherlibs/dynlink; \
	 mkdir otherlibs/str; \
	 cp -l stdlib/{lib,}str* otherlibs/str; \
	)

	mkdir _runtest/toplevel
	cp $(stage2_build)/ocaml/toplevel/.ocamltoplevel.objs/byte/*.cm* \
	  _runtest/toplevel/

#	for lib in \
#	  ui \
#	  algorithms \
#	  lattices \
#	  numbers \
#	  identifiers \
#	  kinds \
#	  nominal \
#	  bound_identifiers \
#	  term_basics \
#	  terms \
#	  from_lambda \
#	  cmx \
#	  types \
#	  simplify \
#	  to_cmm \
#	  parser; \
#	do \
#	  cp $(stage2_build)/middle_end/flambda2/$$lib/flambda2_$${lib}.cma \
#	    _runtest/compilerlibs; \
#	done
#	cp $(stage2_build)/middle_end/flambda2/flambda2.cma \
#	  _runtest/compilerlibs
	# Various directories are put on the -I paths by tools/Makefile;
	# utils/ is one such, so we just dump the .cm* files in there for
	# various things.
	mkdir _runtest/utils
	cp $(stage2_prefix)/lib/ocaml/compiler-libs/*.cmi _runtest/utils
	cp $(stage2_prefix)/lib/ocaml/compiler-libs/*.cmo _runtest/utils
	cp $(stage2_prefix)/lib/ocaml/compiler-libs/*.cmx _runtest/utils
	cp $(stage2_prefix)/lib/ocaml/*.cmi _runtest/utils
	cp $(stage2_prefix)/lib/ocaml/*.cma _runtest/utils
	cp $(stage2_prefix)/lib/ocaml/*.a _runtest/utils
	cp $(stage2_prefix)/lib/ocaml/*.cmxa _runtest/utils
	cp $(stage2_build)/ocaml/.ocamlcommon.objs/native/config.o _runtest/utils
	# Needed for tests/warnings
	cp ocaml/utils/warnings.ml _runtest/utils
	# Suppress linker errors about -I directories not existing.
	for dir in asmcomp bytecomp driver file_formats lambda middle_end \
	  parsing typing; do ln -s utils _runtest/$$dir; done
	# stublibs
	mkdir -p _runtest/lib/ocaml/stublibs/
	cp $(stage2_prefix)/lib/ocaml/stublibs/*.so _runtest/lib/ocaml/stublibs
	# ocamldebug
	mkdir _runtest/debugger
	ln -s ../ocamldebug _runtest/debugger
	cp $(stage2_build)/ocaml/debugger/.main.eobjs/byte/*.cm* \
	  _runtest/debugger
	# The ast_invariants test needs VERSION to be present.  In fact ideally
	# we should have all the source files in _runtest too for this test,
	# but for the moment we accept it being a weaker check.  We're not
	# working on parts of the compiler that deal with the AST anyway in
	# this repo.
	touch _runtest/VERSION
	# tools
	mkdir _runtest/tools
	cp $(stage2_build)/ocaml/tools/ocamlmklib_byte.bc _runtest/tools/ocamlmklib
	cp $(stage2_build)/tools/ocamlobjinfo_byte.bc _runtest/tools/ocamlobjinfo
	# ocamltest itself
	mkdir _runtest/ocamltest
	cp $(stage2_build)/ocaml/ocamltest/ocamltest.byte _runtest/ocamltest/ocamltest
# 	PATH=$(stage0_prefix)/bin:$$PATH \
# 	  ARCH=$(arch) \
# 	  $(dune) build --root=. --profile=release --build-dir=_build1 \
# 	  ocaml/tools/cmpbyt.bc
# 	cp $(stage1_build)/ocaml/tools/cmpbyt.bc _runtest/tools/cmpbyt
	for dir in `cd ocaml/testsuite; ls -1 -d tests/*`; do \
	  if ! grep -q "^  $$dir " testsuite/flambda2-test-list; then \
	    echo "  $$dir"; \
	  fi; \
	done > _runtest/flambda2-test-list
	(export OCAMLSRCDIR=$$(pwd)/_runtest; \
	 export CAML_LD_LIBRARY_PATH=$$(pwd)/_runtest/lib/ocaml/stublibs; \
	 cd _runtest/testsuite \
	  && if $$(which parallel > /dev/null 2>&1); \
             then \
               if [ "$(middle_end)" = "flambda2" ]; then \
                 make list-parallel FILE=$$(pwd)/../flambda2-test-list; \
	       else \
                 make parallel; \
               fi \
             else \
               if [ "$(middle_end)" = "flambda2" ]; then \
                 make list FILE=$$(pwd)/../flambda2-test-list; \
	       else \
                 make all; \
               fi \
             fi)
