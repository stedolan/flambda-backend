#**************************************************************************
#*                                                                        *
#*                 The Flambda backend project for OCaml                  *
#*                                                                        *
#*   Copyright 2020--2021 Jane Street Group LLC                           *
#*                                                                        *
#**************************************************************************

.NOTPARALLEL:

# CR mshinwell: Find out how to get something like "set -eu" in effect.
prefix=@prefix@
stage0_prefix=@stage0_prefix@
stage1_prefix=@stage1_prefix@
stage2_prefix=@stage2_prefix@
middle_end=@middle_end@
dune=@dune@

stage1_build=_build1/default
stage2_build=_build2/default

arch := $(shell grep '^ARCH=' ocaml/Makefile.config | cut -d'=' -f2)

# The Flambda backend compiler build proceeds, from cold, in three stages.
# We call these (in order) stage0, stage1 and stage2.  They are documented
# below.

.DEFAULT_GOAL := stage2

# Building stage0 is the same as an upstream compiler build with make.  This is
# done in an rsynced tree to avoid polluting the ocaml/ subdirectory (see
# below for more details).  We need to make sure the separate tree is in
# sync with the ocaml/ directory first, in case files were added or removed.
#
# We use the -i option to rsync in order to short-circuit a null build with
# make, since it seems faster.
#
# CR-someday mshinwell: We should replace this with a coldstart using dune.
.PHONY: stage0
stage0: _build0/config.status
	rm -f .rsync-output
	rsync -i -a --filter=':- $$(pwd)/ocaml/.gitignore' \
	  $$(pwd)/ocaml/ $$(pwd)/_build0 \
	  | grep -v '/$$' \
	  | tee .rsync-output
	if [ -s .rsync-output ] || ! [ -d $(stage0_prefix) ]; then \
	  (cd _build0 && \
	    $(MAKE) world.opt && \
	    $(MAKE) ocamlnat && \
	    $(MAKE) install); \
	fi

# stage1 builds the Flambda backend compiler using the stage0 compiler.
#
# At the end, we have a working Flambda backend compiler, equivalent to having
# done "make ocamlopt" upstream.  For testing with a broken middle or backend,
# e.g. if the stdlib doesn't compile, you can stop here and run
# _build1/default/flambda_backend_main.exe.
#
# At this point we don't yet have a standard library and a set of otherlibs
# whose .cmx files are compatible with this new compiler.  Neither are the
# Flambda backend compiler, or the compilerlibs that form it, built with the
# Flambda backend compiler itself.  These steps comprise stage2.
#
# This code should use build contexts instead of --build-dir.
.PHONY: stage1
stage1: ocaml-stage1-config.status stage0 \
	  config_files
	cp ocaml-stage1-config.status ocaml/config.status
	(cd ocaml && ./config.status)
	PATH=$(stage0_prefix)/bin:$$PATH \
	  ARCH=$(arch) \
	  $(dune) build --root=. --profile=release --build-dir=_build1 @install
	(cd $(stage1_prefix)/bin && \
	  rm -f ocamllex && \
	  ln -s ocamllex.opt ocamllex)

# CR mshinwell: We should add targets that don't use --profile=release, for
# speed, and also ensuring that warnings are errors.  We should also consider
# adding a new Dune profile that is like "release" but has warnings as errors.

# stage2 rebuilds the whole compiler system with the Flambda backend compiler
# itself, including the stdlib, otherlibs, compilerlibs, etc.  The result is
# equivalent to having done "make world.opt && make ocamlnat" upstream.
.PHONY: stage2
stage2: ocaml-stage2-config.status stage1
	cp ocaml-stage2-config.status ocaml/config.status
	(cd ocaml && ./config.status)
	PATH=$(stage1_prefix)/bin:$$PATH \
	  ARCH=$(arch) \
	  $(dune) build --root=. --profile=release --build-dir=_build2 @install

# This target is like a polling version of upstream "make ocamlopt" (based
# on the stage1 target, above).
# It is likely to be what's most often wanted for day-to-day development of
# features, especially large ones that take a long time to get to compile,
# in the middle end and backend.
.PHONY: hacking
hacking: ocaml-stage1-config.status stage0 \
	  config_files
	cp ocaml-stage1-config.status ocaml/config.status
	(cd ocaml && ./config.status)
	PATH=$(stage0_prefix)/bin:$$PATH \
	  ARCH=$(arch) \
	  $(dune) build --root=. -w --profile=release --build-dir=_build1 @install


## Test compilation of backend-specific parts
ARCHES=amd64 arm64
ARCH_SPECIFIC =\
  backend/arch.ml backend/proc.ml backend/CSE.ml backend/selection.ml \
  backend/scheduling.ml backend/reload.ml backend/emit.ml

# This rule provides a quick way to check that machine-dependent
# files compiles fine for a foreign architecture (passed as ARCH=xxx).
# Based on stage1 target above.
.PHONY: check_arch
check_arch: ocaml-stage1-config.status stage0 \
	config_files
	cp ocaml-stage1-config.status ocaml/config.status
	(cd ocaml && ./config.status)
	@echo "========= CHECKING backend/$(ARCH) =============="
	rm -f $(ARCH_SPECIFIC)
	PATH=$(stage0_prefix)/bin:$$PATH \
	  $(dune) build --root=. --profile=release --build-dir=_build1 ocamloptcomp.cma
	rm -f $(ARCH_SPECIFIC)

.PHONY: check_all_arches
check_all_arches: ocaml-stage1-config.status stage0 \
	config_files
	cp ocaml-stage1-config.status ocaml/config.status
	(cd ocaml && ./config.status)
	if $$(grep '^ARCH64=true' ocaml/Makefile.config > /dev/null) ; \
	then \
		for i in $(ARCHES); do \
		   $(MAKE) --no-print-directory check_arch ARCH=$$i || exit 1; \
		done; \
	else \
		echo "Architecture tests are disabled on 32-bit platforms." ;\
	fi

# The stage0 configure step configures the tree to build pretty much the
# bare minimum that we need for building stage1.
# Currently the middle end for stage0 will match the selected middle end
# for the Flambda backend compiler, except when using Flambda 2, when it
# will be Closure (see configure.ac).
_build0/config.status: ocaml/configure.ac
	rm -rf _build0
	mkdir _build0
	rsync -a $$(pwd)/ocaml/ $$(pwd)/_build0
	(cd _build0 && \
	  cat ../configure_opts_stage0 | tr -d '\n' | xargs -0 ./configure -C \
	    --prefix=$(stage0_prefix) \
	    --disable-ocamldoc \
	    --disable-ocamltest \
	    --disable-debug-runtime \
	    --disable-instrumented-runtime \
	    --disable-debugger)

# stage1 has already been configured by running the configure script.
# It is configured according to any options requested by the user, including
# selection of the middle end, but the prefix is set to a temporary build
# directory.

# This configures stage2 to have the same configure options as stage1
# except that the prefix is set to the ultimate installation directory.
# We save the config.status file (which is executable) for fast reconfiguration
# of the ocaml/ subdirectory during the dune builds for stage1 and stage2.
# The stage2 configure can be run by make in parallel with that for stage0.
# We add --enable-ocamltest so that the config.status generated here can be
# immediately reused for the "compare" target (see below).
ocaml-stage2-config.status: ocaml/configure.ac
	rm -rf _stage2_configure
	mkdir _stage2_configure
	rsync -a --filter=':- $$(pwd)/ocaml/.gitignore' \
	  $$(pwd)/ocaml/ $$(pwd)/_stage2_configure
	(cd _stage2_configure && \
	  cat ../configure_opts | xargs -0 ./configure -C \
	    --prefix=$(prefix) \
	    --enable-ocamltest \
	    --disable-ocamldoc && \
	  cp config.status ../ocaml-stage2-config.status)

# natdynlinkops2:
# We need to augment dune's substitutions so this part isn't so
# difficult.  We use /bin/echo to avoid builtin variants of "echo"
# which don't accept "-n".  Unfortunately if there are no
# NATDYNLINKOPS, we need to provide a harmless option, otherwise dune
# will provide '' on the command line to ocamlopt which causes an
# error.
# CR mshinwell: This should be moved into the upstream dune build system.
#
# flags.sexp:
# Extract compilation flags from Makefile.config of stage1
# and write them to a file that dune can use in stage1 and stage2.
.PHONY: config_files
config_files: ocaml-stage1-config.status
	cp ocaml-stage1-config.status ocaml/config.status
	(cd ocaml && ./config.status)
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

## Build upstream compiler.
## Similar to stage0 except the installation directory
_build_upstream/config.status: ocaml/configure.ac
	rm -rf _build_upstream
	mkdir _build_upstream
	rsync -a $$(pwd)/ocaml/ $$(pwd)/_build_upstream
	(cd _build_upstream && \
	  cat ../configure_opts_stage0 | tr -d '\n' | xargs -0 ./configure -C \
	    --prefix=@prefix@ \
	    --disable-ocamldoc \
	    --enable-ocamltest)

.PHONY: build_upstream
build_upstream: _build_upstream/config.status
	rsync -a \
	  --filter=':- $$(pwd)/ocaml/.gitignore' \
		$$(pwd)/ocaml/ $$(pwd)/_build_upstream
	(cd _build_upstream && \
	    $(MAKE) world.opt && \
	    $(MAKE) ocamlnat)

.PHONY: install_upstream
install_upstream: build_upstream
	(cd _build_upstream && $(MAKE) install)
	cp ocaml/VERSION $(prefix)/lib/ocaml/

# Most of the installation tree is correctly set up by dune, but we need to
# copy it to the final destination, and rearrange a few things to match
# upstream.
.PHONY: install
install: stage2
	mkdir -p $(prefix)
	rsync --copy-links --chmod=u+rw,go+r -r $(stage2_prefix)/bin $(prefix)
	cp scripts/dummy-ocamlprof.sh $(prefix)/bin/ocamlprof
	cp scripts/dummy-ocamlcp.sh $(prefix)/bin/ocamlcp
	rsync --copy-links --chmod=u+rw,go+r -r $(stage2_prefix)/lib $(prefix)
	rm -f $(prefix)/bin/ocamllex
	ln -s $(prefix)/bin/ocamllex.opt $(prefix)/bin/ocamllex
	rm -f $(prefix)/bin/flambda_backend.main*
	rm -rf $(prefix)/lib/ocaml-variants
	rm -rf $(prefix)/lib/stublibs
	rm -f $(prefix)/lib/ocaml/META
	rm -f $(prefix)/lib/ocaml/dune-package
	rm -f $(prefix)/lib/ocaml/compiler-libs/*.cmo
	cp $(stage2_prefix)/lib/ocaml/compiler-libs/topstart.cmo \
	  $(prefix)/lib/ocaml/compiler-libs/
	rm -rf $(prefix)/lib/flambda_backend
	for file in topdirs opttopdirs; do \
	  for ext in cmi mli cmt cmti; do \
	    cp -f $(prefix)/lib/ocaml/compiler-libs/$${file}.$$ext \
	      $(prefix)/lib/ocaml; \
	  done; \
	done

# This target only runs the dune-based tests, not the upstream testsuite.
# stage2 needs to have been built first.
.PHONY: runtest
runtest:
	# It's a shame that dune needs the stage1 compiler on the path here.
	# Ideally that would be inaccessible within tests (to prevent mistakes
	# such as running "ocamlopt" rather than one of the stage2 binaries).
	PATH=$(stage1_prefix)/bin:$$PATH \
	  ARCH=$(arch) \
	  $(dune) runtest --root=. --profile=release --build-dir=_build2

# Only needed for running the test tools by hand; runtest will take care of
# building them using Dune
.PHONY: test-tools
test-tools: stage1
	PATH=$(stage1_prefix)/bin:$$PATH \
	  ARCH=$(arch) \
	  $(dune) build @middle_end/flambda2/tests/tools/all \
	    --root=. --profile=release --build-dir=_build2

.PHONY: promote
promote:
	PATH=$(stage1_prefix)/bin:$$PATH \
	  ARCH=$(arch) \
	  $(dune) promote --root=. --build-dir=_build2

# The following horror will be removed when work to allow the testsuite to
# run on an installed tree (led by David Allsopp) is completed.
# stage2 needs to have been built first.
.PHONY: runtest-upstream
runtest-upstream:
	rm -rf _runtest
	mkdir _runtest
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
	cp $(stage2_prefix)/bin/* _runtest/
	# There seems to be an assumption that ocamlc/ocamlopt/ocamllex are
	# bytecode...
	cp -f $(stage2_prefix)/bin/ocamlc.byte _runtest/ocamlc
	cp -f $(stage2_prefix)/bin/ocamlopt.byte _runtest/ocamlopt
	mkdir _runtest/lex
	mv _runtest/ocamllex.byte _runtest/lex/ocamllex
	mkdir _runtest/yacc
	mv _runtest/ocamlyacc _runtest/yacc/
	(cd _runtest && ln -s ../_build2/default/ocaml/runtime runtime)
	(cd _runtest && ln -s ../_build2/install/default/lib/ocaml stdlib)
	# compilerlibs
	mkdir _runtest/compilerlibs
	cp $(stage2_prefix)/lib/ocaml/compiler-libs/*.cma _runtest/compilerlibs
	cp $(stage2_prefix)/lib/ocaml/compiler-libs/*.a _runtest/compilerlibs
	cp $(stage2_prefix)/lib/ocaml/compiler-libs/*.cmxa _runtest/compilerlibs
	mkdir _runtest/toplevel
	cp $(stage2_build)/ocaml/toplevel/.ocamltoplevel.objs/byte/*.cm* \
	  _runtest/toplevel/
	for lib in \
	  ui \
	  algorithms \
	  lattices \
	  numbers \
	  identifiers \
	  kinds \
	  nominal \
	  bound_identifiers \
	  term_basics \
	  terms \
	  from_lambda \
	  cmx \
	  types \
	  simplify \
	  to_cmm \
	  parser; \
	do \
	  cp $(stage2_build)/middle_end/flambda2/$$lib/flambda2_$${lib}.cma \
	    _runtest/compilerlibs; \
	done
	cp $(stage2_build)/middle_end/flambda2/flambda2.cma \
	  _runtest/compilerlibs
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
	# dynlink
	mkdir -p _runtest/otherlibs/dynlink
	cp $(stage2_prefix)/lib/ocaml/dynlink* _runtest/otherlibs/dynlink
	# stublibs
	mkdir -p _runtest/lib/ocaml/stublibs/
	cp $(stage2_prefix)/lib/ocaml/stublibs/*.so _runtest/lib/ocaml/stublibs
	# str
	mkdir -p _runtest/otherlibs/str
	cp $(stage2_prefix)/lib/ocaml/str*.cmi _runtest/otherlibs/str/
	cp $(stage2_prefix)/lib/ocaml/libstr*.a _runtest/otherlibs/str
	cp $(stage2_prefix)/lib/ocaml/str*.cma _runtest/otherlibs/str
	cp $(stage2_prefix)/lib/ocaml/str*.cmxa _runtest/otherlibs/str
	cp $(stage2_prefix)/lib/ocaml/str*.a _runtest/otherlibs/str
	cp $(stage2_prefix)/lib/ocaml/str*.cmx _runtest/otherlibs/str
	# unix
	mkdir -p _runtest/otherlibs/unix
	cp $(stage2_prefix)/lib/ocaml/unix*.cmi _runtest/otherlibs/unix
	cp $(stage2_prefix)/lib/ocaml/libunix*.a _runtest/otherlibs/unix
	cp $(stage2_prefix)/lib/ocaml/unix*.cma _runtest/otherlibs/unix
	cp $(stage2_prefix)/lib/ocaml/unix*.cmxa _runtest/otherlibs/unix
	cp $(stage2_prefix)/lib/ocaml/unix*.a _runtest/otherlibs/unix
	cp $(stage2_prefix)/lib/ocaml/unix*.cmx _runtest/otherlibs/unix
	# systhreads
	mkdir -p _runtest/otherlibs/systhreads
	cp $(stage2_prefix)/lib/ocaml/threads/*.cmi _runtest/otherlibs/systhreads
	cp $(stage2_prefix)/lib/ocaml/threads/*.cma _runtest/otherlibs/systhreads
	cp $(stage2_prefix)/lib/ocaml/threads/*.a _runtest/otherlibs/systhreads
	cp $(stage2_prefix)/lib/ocaml/threads/*.cmxa _runtest/otherlibs/systhreads
	cp $(stage2_prefix)/lib/ocaml/threads/*.cmx _runtest/otherlibs/systhreads
	# ocamldebug
	mkdir _runtest/debugger
	mv _runtest/ocamldebug _runtest/debugger
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
	# This is deliberately run with the stage0 compiler in case the new
	# one is broken.  As such, we use the stage1 build dir, not that from
	# stage2.
	# This might be causing a spurious rebuild of the runtime
	PATH=$(stage0_prefix)/bin:$$PATH \
	  ARCH=$(arch) \
	  $(dune) build --root=. --profile=release --build-dir=_build1 \
	  ocaml/tools/cmpbyt.bc
	PATH=$(stage0_prefix)/bin:$$PATH \
	  ARCH=$(arch) \
	  $(dune) build --root=. --profile=release --build-dir=_build1 \
	  ocaml/ocamltest/ocamltest.byte
	cp $(stage1_build)/ocaml/tools/cmpbyt.bc _runtest/tools/cmpbyt
	# We should build the native ocamltest too.
	cp $(stage1_build)/ocaml/ocamltest/ocamltest.byte _runtest/ocamltest/ocamltest
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

# Compare the Flambda backend installation tree against the upstream one.
# "make install" needs to have been done first.
#
# We can't use the stage0 installation directory to compare against because
# it won't have been configured with the correct options.  Instead we build
# a fresh upstream compiler using the same configure options as the user
# originally specified when they configured the Flambda backend tree.
.PHONY: compare
compare: _compare/config.status
	rm -f .rsync-output-compare
	rsync -i -a --filter=':- $$(pwd)/ocaml/.gitignore' \
	  $$(pwd)/ocaml/ $$(pwd)/_compare \
	  | grep -v '/$$' \
	  | tee .rsync-output-compare
	if [ -s .rsync-output-compare ] || ! [ -d _compare/_install ]; then \
	  (cd _compare && \
	    $(MAKE) world.opt && \
	    $(MAKE) ocamlnat && \
	    $(MAKE) install); \
	fi
	./scripts/compare.sh $$(pwd)/_compare/_install $(prefix) \
	  $(stage0_prefix)/bin/ocamlobjinfo.opt

# CR mshinwell: Why does the ocamltest build complain about
# Ocaml_directories being missing?
_compare/config.status: ocaml/configure.ac
	rm -rf _compare
	mkdir _compare
	rsync -a --filter=':- $$(pwd)/ocaml/.gitignore' \
	  $$(pwd)/ocaml/ $$(pwd)/_compare
	(cd _compare && \
	  cat ../configure_opts | xargs -0 ./configure -C \
	    --prefix=$$(pwd)/_install \
	    --disable-stdlib-manpages \
	    --disable-ocamltest \
	    --disable-ocamldoc)

# For Github Actions checks
.PHONY: ci
ci:
	$(MAKE) stage2
	$(MAKE) install
	$(MAKE) runtest
	$(MAKE) runtest-upstream

.PHONY: fmt
fmt:
	ocamlformat -i \
	  $$(find middle_end/flambda2 \
	    \( -name "*.ml" -or -name "*.mli" \) \
	    -and \! \( -name "flambda_parser.*" -or -name "flambda_lex.*" \))
	ocamlformat -i \
	  $$(find backend/cfg \
	    \( -name "*.ml" -or -name "*.mli" \))
	ocamlformat -i middle_end/mangling.ml
	ocamlformat -i middle_end/mangling.mli
	ocamlformat -i tools/merge_archives.ml

.PHONY: check-fmt
check-fmt:
	if [ "$$(git status --porcelain middle_end/flambda2)" != "" ] || \
           [ "$$(git status --porcelain backend/cfg)" != "" ] || \
           [ "$$(git status --porcelain middle_end/mangling.ml)" != "" ] || \
           [ "$$(git status --porcelain middle_end/mangling.mli)" != "" ] || \
           [ "$$(git status --porcelain tools/merge_archives.ml)" != "" ]; then \
	  echo; \
	  echo "Tree must be clean before running 'make check-fmt'"; \
	  exit 1; \
	fi
	$(MAKE) fmt
	if [ "$$(git diff middle_end/flambda2)" != "" ] || \
           [ "$$(git diff backend/cfg)" != "" ] || \
           [ "$$(git diff middle_end/mangling.ml)" != "" ] || \
           [ "$$(git diff middle_end/mangling.mli)" != "" ] || \
           [ "$$(git diff tools/merge_archives.ml)" != "" ]; then \
	  echo; \
	  echo "The following code was not formatted correctly:"; \
	  echo "(the + side of the diff is how it should be formatted)"; \
	  echo "(working copy now contains correctly-formatted code)"; \
	  echo; \
	  git diff --no-ext-diff; \
	  exit 1; \
	fi

.PHONY: regen-flambda2-parser
regen-flambda2-parser:
	PATH=$(stage0_prefix)/bin:$$PATH \
	  ARCH=$(arch) \
	  $(dune) build @middle_end/flambda2/parser/regen --auto-promote --root=. \
	    --build-dir=_build2 \
	  || true
# Make sure regeneration is idempotent, and also check that the previous step
# worked (can't tell the difference between failure and successful
# auto-promotion)
	PATH=$(stage0_prefix)/bin:$$PATH \
	  ARCH=$(arch) \
	  $(dune) build @middle_end/flambda2/parser/regen --root=. --build-dir=_build2
