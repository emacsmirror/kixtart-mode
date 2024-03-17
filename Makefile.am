# Prevent byte-compilation warning:
#   Warning (bytecomp): Changing ‘byte-compile-dest-file’ is obsolete (as of 23.2);
#   set ‘byte-compile-dest-file-function’ instead.
AM_ELCFLAGS = --eval '(require (quote bytecomp))'

dist_lisp_LISP = kixtart-mode.el kixtart-docstrings.el
dist_noinst_LISP = kixtart-mode-tests.el

info_TEXINFOS = doc/kixtart-mode.texi
doc_kixtart_mode_TEXINFOS = doc/doclicense.texi doc/example-configuration.texi doc/features.texi doc/installation.texi

EXTRA_DIST = doc/README.texi

README: doc/README.texi doc/example-configuration.texi doc/features.texi doc/installation.texi doc/version.texi
	$(MAKEINFO) --plaintext --output=$@ $(srcdir)/doc/README.texi

check: kixtart-mode-tests.elc
	$(EMACS) --batch --quick \
	    --directory . \
	    --load kixtart-mode-tests.elc \
	    --funcall ert-run-tests-batch-and-exit

dist-hook: README
