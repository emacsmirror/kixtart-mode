# Prevent byte-compilation warning:
#   Warning (bytecomp): Changing 'byte-compile-dest-file' is obsolete (as of 23.2);
#   set 'byte-compile-dest-file-function' instead.
AM_ELCFLAGS = --eval '(require (quote bytecomp))'

dist_lisp_LISP = kixtart-mode.el kixtart-docstrings.el
dist_noinst_LISP = kixtart-mode-tests.el

info_TEXINFOS = doc/kixtart-mode.texi
doc_kixtart_mode_TEXINFOS = doc/doclicense.texi doc/example-configuration.texi doc/features.texi doc/installation.texi

AM_MAKEINFOHTMLFLAGS = --no-headers --no-split
EXTRA_DIST = doc/README.texi resources/font-lock-level-1.kix resources/font-lock-level-2.kix resources/font-lock-level-3.kix

README: doc/README.texi doc/example-configuration.texi doc/features.texi doc/installation.texi doc/version.texi
	$(AM_V_GEN)$(MAKEINFO) \
	    --set-customization-variable ASCII_PUNCTUATION=1 \
	    --plaintext \
	    --output=$@ \
	    $(srcdir)/doc/README.texi

check: kixtart-mode-tests.elc
	$(AM_V_at)$(EMACS) --batch --quick \
	    --directory . \
	    --load kixtart-mode-tests.elc \
	    --funcall ert-run-tests-batch-and-exit

dist-hook: README
