.POSIX:
.SUFFIXES: .el .elc

EMACS = emacs
RM = rm -f

compile: README kixtart-mode.elc

check: kixtart-mode-tests.elc
	$(EMACS) --batch --quick \
	    --directory . \
	    --load kixtart-mode-tests.elc \
	    --funcall ert-run-tests-batch-and-exit

kixtart-mode-tests.elc: kixtart-mode.elc kixtart-mode-tests.el

.el.elc:
	$(EMACS) --batch --quick \
	    --directory . \
	    --funcall batch-byte-compile $<

README: kixtart-mode.el
	$(EMACS) --batch --quick \
	    --load lisp-mnt \
	    --eval "(with-temp-file \"$@\" \
	              (setq buffer-file-coding-system 'utf-8-unix) \
	              (insert (lm-commentary \"kixtart-mode.el\")) \
	              (newline))"

clean:
	$(RM) README kixtart-mode-tests.elc kixtart-mode.elc
