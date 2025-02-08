.POSIX:
.SUFFIXES: .el .elc

EMACS = emacs
RM = rm
RM_FLAGS = -rf
TEXI2ANY = texi2any
TEXI2ANY_HTMLFLAGS = --no-headers --no-split
TEXI2ANY_INFOFLAGS = --no-split
TEXI2ANY_PDFFLAGS = --Xopt=--quiet --Xopt=--tidy

readme_deps = \
	doc/README.texi \
	doc/example-configuration.texi \
	doc/features.texi \
	doc/installation.texi

manual_deps = \
	doc/doclicense.texi \
	doc/example-configuration.texi \
	doc/features.texi \
	doc/installation.texi \
	doc/kixtart-mode.texi \
	doc/version.texi

all: README kixtart-docstrings.elc kixtart-mode.elc kixtart-mode.info

.el.elc:
	$(EMACS) --batch --quick \
	    --directory . \
	    --funcall batch-byte-compile $<

README: $(readme_deps)
	$(TEXI2ANY) \
	    --set-customization-variable ASCII_PUNCTUATION=1 \
	    --plaintext \
	    --output=$@ \
	    doc/README.texi

check: kixtart-mode-tests.elc
	$(EMACS) --batch --quick \
	    --directory . \
	    --load kixtart-mode-tests.elc \
	    --funcall ert-run-tests-batch-and-exit

clean:
	$(RM) $(RM_FLAGS) \
	    kixtart-docstrings.elc \
	    kixtart-mode-tests.elc \
	    kixtart-mode.elc \
	    kixtart-mode.html \
	    kixtart-mode.info \
	    kixtart-mode.pdf \
	    kixtart-mode.t2d

doc/version.texi: kixtart-mode.el
	$(EMACS) --batch --quick \
	    --load lisp-mnt \
	    --eval "(with-temp-file \"$@\" \
	              (setq buffer-file-coding-system 'utf-8-unix) \
	              (insert (format \"@set VERSION %s\n\" \
	                              (lm-version \"kixtart-mode.el\"))))"

kixtart-docstrings.elc: kixtart-mode.elc

kixtart-mode-tests.elc: kixtart-mode.elc kixtart-mode-tests.el

kixtart-mode.html: $(manual_deps)
	$(TEXI2ANY) $(TEXI2ANY_HTMLFLAGS) \
	    --html \
	    --output=$@ \
	    doc/kixtart-mode.texi

kixtart-mode.info: $(manual_deps)
	$(TEXI2ANY) $(TEXI2ANY_INFOFLAGS) \
	    --output=$@ \
	    doc/kixtart-mode.texi

kixtart-mode.pdf: $(manual_deps)
	$(TEXI2ANY) $(TEXI2ANY_PDFFLAGS) \
	    --pdf \
	    --output=$@ \
	    doc/kixtart-mode.texi
