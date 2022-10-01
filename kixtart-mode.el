;;; kixtart-mode.el --- Major mode for editing KiXtart files -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Morgan Willcock

;; Author: Morgan Willcock <morganwillcock@users.noreply.github.com>
;; Keywords: languages
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/morganwillcock/kixtart-mode
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; * Overview

;; KiXtart Mode is an Emacs major mode for creating and editing KiXtart script
;; files.  The following features are implemented:

;; - Syntax highlighting for commands, functions, labels, and macros
;; - Indentation based on the usage of commands and parentheses
;; - Motion around and selection of defined functions
;; - Imenu support for function and label names
;; - Outline Mode support
;; - Predefined Tempo templates with optional abbrev expansion

;; ** Installation

;; Clone this repository to a local sub-directory:

;;   git clone https://github.com/morganwillcock/kixtart-mode.git

;; Within the Emacs configuration, add the path of the cloned directory to the
;; value of `load-path'.  The following example assumes that the repository was
;; cloned within a directory named "manual-packages" within the user's
;; ".emacs.d" directory:

;;   (add-to-list 'load-path "~/.emacs.d/manual-packages/kixtart-mode")

;; ** Example configuration

;; Assuming that the KiXtart Mode package is currently not loaded, the following
;; example demonstrates how to load the package, configure some basic settings,
;; and enable some optional features:

;;   ;; Load the package by searching in paths listed in `load-path'.
;;   (require 'kixtart-mode)

;;   ;; Set indentation and fill preferences.
;;   (add-hook 'kixtart-mode-hook
;;             (lambda ()
;;               (indent-tabs-mode 1)
;;               (setq tab-width 4)
;;               (setq fill-column 80)))

;;   ;; Set template insertion to use interactive prompts.
;;   (setq tempo-interactive t)

;;   ;; Enable `abbrev-mode' by default.
;;   (add-hook 'kixtart-mode-hook #'abbrev-mode)
;;   ;; Enable automatic abbrev expansion with the KiXtart Mode abbrev
;;   ;; table when outside of comments and strings.
;;   (setq kixtart-abbrev-table-enabled t)

;; * Syntax highlighting

;; Syntax highlighting is implemented for all KiXtart language keywords using
;; the following font-lock faces:

;; Keyword type    Face
;; -------------------------------------------------
;; Command name    `font-lock-keyword-face'
;; Function name   `font-lock-function-name-face'
;; Label name      `font-lock-constant-face'
;; Macro           `font-lock-type-face'
;; Variable        `font-lock-variable-name-face'

;; Note: Syntax highlighting for function names requires the preceding
;; "FUNCTION" command to appear on the same line.

;; The additional face `font-lock-warning-face' is used to highlight portions of
;; text which are likely to be scripting errors.  Currently this covers two
;; potentially problematic cases related to KiXtart macros.  The first case is
;; where the macro name does not match any known macro: such macros always
;; evaluate to 0 and are likely the result of a spelling error.  The second case
;; covers a peculiarity of the original KiXtart parser, which seems to silently
;; discard a trailing portion of a macro name when the following criteria are
;; met:

;; 1. The beginning of the macro name matches a valid KiXtart macro name.

;; 2. Immediately following this match, there are additional characters present
;;    which are valid for use within a macro name.

;; 3. The additional characters do not extend the match to a different (longer)
;;    KiXtart macro name.

;; The trailing portion of the macro name will be highlighted as an error.

;; * Indentation

;; KiXtart Mode calculates the indentation level for a given line by determining
;; the number of script-blocks which are open around the current buffer
;; position, where parentheses or the following commands are deemed to open a
;; new or close an existing script-block:

;; Block-opening commands   Block-closing commands
;; --------------------------------------------------
;; DO                       UNTIL
;; CASE                     CASE ENDSELECT
;; ELSE                     ENDIF
;; FOR                      NEXT
;; FUNCTION                 ENDFUNCTION
;; IF                       ELSE ENDIF
;; WHILE                    LOOP

;; Note: SELECT commands are ignored for indentation purposes, to allow the
;; contained CASE commands to maintain the current indentation level.

;; The indentation offset applied to each subsequent script-block level is
;; determined by multiplying the value of customization variable
;; `kixtart-indent-offset' with the indentation level of the line which opened
;; the current script-block.  For example, the default value of 4 will increase
;; indentation in every script-block by 4 columns:

;;   IF $a
;;       *
;;   ENDIF

;;   1 + (
;;       *
;;   )

;; Using the indentation level of the line which opened the current script-block
;; allows nested script-blocks to share a line and only increase the indentation
;; by a single level.  For example, to create the equivalent of an AND
;; expression which will correctly short-circuit, it is likely preferable to use
;; two IF commands which are on the same line:

;;   IF $a IF $b
;;       *
;;   ENDIF ENDIF

;; * Motion commands

;; KiXtart Mode assigns custom functions to `beginning-of-defun-function' and
;; `end-of-defun-function' and so permits navigation around and selection of
;; function definitions with commands such as `beginning-of-defun',
;; `end-of-defun', and `mark-defun'.

;; These commands should work well for function definitions which appear at the
;; top-level of the script, where the FUNCTION and ENDFUNCTION commands which
;; define a KiXtart function have not been indented.  If FUNCTION commands have
;; been indented, operation may be improved by remapping the key-binding which
;; would normally call `beginning-of-defun' to call `beginning-of-defun-raw' in
;; its place:

;;   (add-hook 'kixtart-mode-hook
;;             (lambda ()
;;               (local-set-key [remap beginning-of-defun]
;;                              'beginning-of-defun-raw)))

;; This will leave point at the beginning of the relevant FUNCTION command
;; rather than at the beginning of the line which contained the relevant
;; FUNCTION command.

;; An additional navigation function is also provided in
;; `kixtart-up-script-block'.  This function uses the same rules of script-block
;; definition to move point to the buffer position which opened the current
;; script-block, based on its currently determined script-block level.  When at
;; the top-level of the script point is moved to the beginning of the buffer.

;; KiXtart Mode binds `kixtart-up-script-block' to 'C-c C-u' by default.

;; * Imenu support

;; The `imenu' command provides code indexing and navigation options which
;; allow point to jump to indexed buffer positions.  The `imenu' menu is
;; presented as a list of completions which contains the names of all
;; defined functions, as well as a single "/Labels" sub-menu entry which
;; lists all defined label names (the "/" character is chosen as a sub-menu
;; prefix because it is invalid as part of a function name).

;; Note: Indexing of function names requires the preceding FUNCTION command to
;; appear on the same line.

;; For very large scripts scanning for index entries can potentially be slow,
;; although automatic re-scanning may be preferable to manually invoking the
;; "*Rescan*" option from the menu.  Consider configuring `imenu-auto-rescan'
;; and `imenu-auto-rescan-maxout' to suitable values.

;; KiXtart Mode binds `imenu' to 'C-c C-j' by default.

;; * Outline Mode support

;; Outline Mode is major mode intended to edit outline-format documents with
;; selective display.  The same functionality is also available through
;; alternate key-bindings using the minor mode version, enabled by invoking
;; `outline-minor-mode'.

;; KiXtart Mode defines Outline Mode settings by borrowing some conventions for
;; headings and their associated depth from the built-in Lisp mode.  Two types
;; of headings are defined:

;; - Comments created by 3 or more ";" characters at the beginning of a line,
;;   followed by at least 1 white-space character

;; - FUNCTION commands at the beginning of a line

;; The depth of comment-based headings is determined by subtracting 2 from the
;; number of ";" characters which were used.  For example, the following
;; comments define Outline Mode headings for the first 3 levels:

;;   ;;; Level 1 heading
;;   ;;;; Level 2 heading
;;   ;;;;; Level 3 heading

;; The depth of FUNCTION command-based headings is not variable: all headings of
;; this type have the maximum possible value for their depth, meaning that
;; selective display of function definitions cannot affect selective display of
;; comment-based headings.

;; * Templates

;; Tempo templates for inserting command-defined script-blocks are
;; predefined and can be inserted by:

;; - Invoking Tempo template functions directly
;; - Invoking KiXtart Mode aliases for Tempo template functions
;; - Tempo functionality such as `tempo-complete-tag'
;; - Abbrev expansion

;; For direct use of Tempo functionality, the following key-bindings exist by
;; default:

;; 'C-c C-t C-b'
;;   `tempo-backward-mark'

;; 'C-c C-t C-f'
;;   `tempo-forward-mark'

;; 'C-c C-t C-t'
;;   `tempo-complete-tag'

;; Note: Tempo tag completion will match a template tag even when the match is a
;; prefix of a longer template tag.  For example, an input of "i" or "if" will
;; expand based on the template tag "if", even though it is a prefix of the
;; longer template tag "ifelse".  To expand based on the template tag "ifelse"
;; the input would have to be "ife", "ifel", "ifels", or "ifelse".

;; Note: For use of interactive prompts when inserting a template, customize the
;; value of 'tempo-interactive'.

;; Regardless of how a template has been inserted the customization variable
;; `kixtart-template-insert-newline' determines whether template insertion will
;; include a final newline character.  A non-nil value will cause a newline to
;; be inserted.  When set to a function (as determined by `functionp') the
;; result of the calling that function should determines the result by returning
;; a nil or non-nil value.  The default value is `eobp' which has the effect of
;; preserving the final newline when templates are inserted at the end of the
;; buffer.

;; ** Abbrev expansion

;; The KiXtart Mode abbrev table will be considered for use when the following
;; conditions are true:

;; 1. `kixtart-abbrev-table-enabled' has a non-nil value
;; 2. Point is currently not within a comment or a string

;; Expansion of the predefined template tags can be performed manually with
;; functions such as `expand-abbrev'.  For automatic expansions, this can be
;; achieved by enabling the abbrev minor-mode (`abbrev-mode') in the current
;; buffer.  It is likely convenient to automatically enable `abbrev-mode' in all
;; KiXtart Mode buffers:

;;   (add-hook 'kixtart-mode-hook #'abbrev-mode)

;; ** Template case control

;; Since all template insertion is done either directly or indirectly through
;; Tempo functions, customization of inserted text can be achieved by modifying
;; the Tempo configuration; if template content should be converted to
;; upper-case, this can be achieved by adding the `upcase' function to
;; `tempo-insert-string-functions':

;;   (add-hook 'kixtart-mode-hook
;;             (lambda ()
;;               (add-to-list (make-local-variable
;;                             'tempo-insert-string-functions)
;;                            #'upcase)))

;; ** Predefined templates

;; KiXtart Mode defines aliases to predefined Tempo template functions which
;; will insert commonly used command-defined script-blocks.

;; Note: Template contents are written using "PascalCase" for language keywords;
;; the first letter of all words within KiXtart commands are capitalized (for
;; example, "EndIf" rather than "ENDIF" or "endif").  This allows the case to
;; customised by optionally down-casing or up-casing the template as it is
;; inserted.

;; Function: `kixtart-template-ifelse'

;;     If *
;;
;;     Else
;;
;;     EndIf

;;   Expands from string "ifelse".
;;   Bound to 'C-c C-t I' by default.

;; Function: `kixtart-template-case'

;;     Case *

;;   Expands from string "case".
;;   Bound to 'C-c C-t c' by default.

;; Function: `kixtart-template-do'

;;     Do
;;         *
;;     Until

;;   Expands from string "do".
;;   Bound to 'C-c C-t d' by default.

;; Function: `kixtart-template-foreach'

;;     For Each * In
;;
;;     Next

;;   Expands from string "foreach".
;;   Bound to 'C-c C-t e' by default.

;; Function: `kixtart-template-for'

;;     For $* =  To  Step
;;
;;     Next

;;   Expands from string "for".
;;   Bound to 'C-c C-t f' by default.

;; Function: `kixtart-template-if'

;;     If *
;;
;;     EndIf

;;   Expands from string "if".
;;   Bound to 'C-c C-t i' by default.

;; Function: `kixtart-template-else'

;;     Else
;;         *

;;   Expands from string "else".
;;   Bound to 'C-c C-t l' by default.

;; Function: `kixtart-template-select'

;;     Select
;;     Case *
;;
;;     EndSelect

;;   Expands from string "select".
;;   Bound to 'C-c C-t s' by default.

;; Function: `kixtart-template-function'

;;     Function *()
;;
;;     EndFunction

;;   Expands from string "function".
;;   Bound to 'C-c C-t u' by default.

;; Function: `kixtart-template-while'

;;     While *
;;
;;     Loop

;;   Expands from string "while".
;;   Bound to 'C-c C-t w' by default.

;;; Code:

(require 'imenu)
(require 'tempo)
(eval-when-compile
  (require 'cl-lib)
  (require 'rx))

;;;; Customization

(defgroup kixtart nil
  "Major mode for editing KiXtart files."
  :tag "KiXtart"
  :link '(emacs-commentary-link "kixtart-mode")
  :group 'languages
  :prefix "kixtart-")

(defcustom kixtart-abbrev-table-enabled nil
  "Specifies whether KiXtart abbrev expansion is enabled.
A non-nil value indicates that `kixtart-mode-abbrev-table' should
be used as part of abbrev expansion."
  :type 'boolean)

(defcustom kixtart-indent-offset 4
  "Specifies the indentation offset applied by `kixtart-indent-line'.
Lines determined to be within script-blocks are indented by this
number of columns per script-block level."
  :type 'integer)

(defcustom kixtart-template-insert-newline #'eobp
  "Specifies whether a template includes a final newline."
  :type `(choice (const :tag "Never" nil)
                 (const :tag "Always" t)
                 (const :tag "At end of buffer" ,#'eobp)
                 (function :tag "Custom function")))

;;;; Search patterns

(defmacro kixtart-rx (&rest regexps)
  "Extended version of `rx' for translation of form REGEXPS."
  `(rx-let ((command
             (or ??
                 (seq symbol-start
                      (or "beep" "big" "break" "call" "case" "cd" "cls" "color"
                          "cookie1" "copy" "debug" "del" "dim" "display" "do"
                          "each" "else" "endfunction" "endif" "endselect" "exit"
                          "flushkb" "for" "function" "get" "gets" "global" "go"
                          "gosub" "goto" "if" "include" "loop" "md" "move"
                          "next" "password" "play" "quit" "rd" "redim" "return"
                          "run" "select" "set" "setl" "setm" "settime" "shell"
                          "sleep" "small" "until" "use" "while")
                      symbol-end)))
            (command-endfunction
             (seq symbol-start "endfunction" symbol-end))
            (command-function
             (seq symbol-start "function" symbol-end))
            (function
             (seq symbol-start
                  (or "abs" "addkey" "addprinterconnection" "addprogramgroup"
                      "addprogramitem" "asc" "ascan" "at" "backupeventlog" "box"
                      "cdbl" "chr" "cint" "cleareventlog" "close"
                      "comparefiletimes" "createobject" "cstr" "dectohex"
                      "delkey" "delprinterconnection" "delprogramgroup"
                      "delprogramitem" "deltree" "delvalue" "dir" "enumgroup"
                      "enumipinfo" "enumkey" "enumlocalgroup" "enumvalue"
                      "execute" "exist" "existkey" "expandenvironmentvars" "fix"
                      "formatnumber" "freefilehandle" "getcommandline"
                      "getdiskspace" "getfileattr" "getfilesize" "getfiletime"
                      "getfileversion" "getobject" "iif" "ingroup" "instr"
                      "instrrev" "int" "isdeclared" "join" "kbhit" "keyexist"
                      "lcase" "left" "len" "loadhive" "loadkey" "logevent"
                      "logoff" "ltrim" "memorysize" "messagebox" "open"
                      "readline" "readprofilestring" "readtype" "readvalue"
                      "redirectoutput" "replace" "right" "rnd" "round" "rtrim"
                      "savekey" "sendkeys" "sendmessage" "setascii" "setconsole"
                      "setdefaultprinter" "setfileattr" "setfocus" "setoption"
                      "setsystemstate" "settitle" "setwallpaper"
                      "showprogramgroup" "shutdown" "sidtoname" "split" "srnd"
                      "substr" "trim" "ubound" "ucase" "unloadhive" "val"
                      "vartype" "vartypename" "writeline" "writeprofilestring"
                      "writevalue")
                  symbol-end))
            (function-def
             ;; Function names cannot start with a character which wrongly
             ;; identifies the name as a label, macro, or variable.
             (seq (group
                   command-function)
                  (1+ whitespace)
                  (group
                   (seq (1+ (intersection user-chars (not (char ?$ ?: ?@))))
                        (0+ user-chars)))))
            (label
             (seq symbol-start ?: (1+ user-chars)))
            (macro
             ;; The real parser seems to silently discard the trailing part of a
             ;; macro name if the leading part matches an actual macro name.
             ;; Match groups are used so that the trailing part of the name can
             ;; be fontified as a warning.
             (seq (group
                   (seq ?@
                        (or "address" "build" "color" "comment" "cpu" "crlf"
                            "csd" "curdir" "date" "day" "domain" "dos" "error"
                            "fullname" "homedir" "homedrive" "homeshr"
                            "hostname" "inwin" "ipaddress0" "ipaddress1"
                            "ipaddress2" "ipaddress3" "kix" "lanroot" "ldomain"
                            "ldrive" "lm" "logonmode" "longhomedir" "lserver"
                            "maxpwage" "mdayno" "mhz" "month" "monthno" "msecs"
                            "onwow64" "pid" "primarygroup" "priv" "productsuite"
                            "producttype" "programfilesx86" "pwage" "ras"
                            "result" "rserver" "scriptdir" "scriptexe"
                            "scriptname" "serror" "sid" "site" "startdir"
                            "syslang" "ticks" "time" "tssession" "userid"
                            "userlang" "wdayno" "wksta" "wuserid" "ydayno"
                            "year")))
                  (group
                   (0+ user-chars))))
            (macro-format
             ;; Match anything which has the appearance of a macro.  An unknown
             ;; macro will evaluate to 0 so a late match against this pattern
             ;; allows the full macro syntax to be fontified as a warning.
             (seq ?@ (1+ user-chars)))
            (outline
             (or command-function
                 (seq (>= 3 (syntax \<)) (1+ whitespace))))
            (script-block-close
             (seq symbol-start
                  (or "case" "else" "endif" "endfunction" "endif" "endselect"
                      "loop" "next" "until")
                  symbol-end))
            (script-block-open
             (seq symbol-start
                  (or "do" "case" "else" "for" "function" "if" "select" "while")
                  symbol-end))
            (user-chars
             ;; Valid characters for user defined names (functions, labels,
             ;; variables) are effectively defined by which characters in the
             ;; printable ASCII range have not been used for other purposes.
             (or (char (?0 . ?9))
                 (char (?a . ?z))
                 (char ?! ?# ?% ?: ?\\ ?_ ?` ?{ ?})))
            (variable
             ;; "$" is a valid variable name and all but the last $ character in
             ;; a repeated sequence will be evaluated as values.

             ;;   "$ = 1" assigns 1 to the variable $
             ;;  "$$ = 2" evaluates $ and then assigns it a new value of 2
             ;; "$$$ = 3" evaluates $ twice and then assigns it a new value of 3
             (seq ?$ (0+ user-chars))))
     (rx ,@regexps)))

;;;; Utility

(defun kixtart--in-comment-or-string-p (&optional ppss)
  "Return a non-nil value when inside a comment or string.
Prefer existing parser state PPSS over calling `syntax-ppss'"
  (nth 8 (or ppss (syntax-ppss))))

(defun kixtart--paren-depth (&optional ppss)
  "Return the current parentheses depth.
Prefer existing parser state PPSS over calling `syntax-ppss'."
  (car (or ppss (syntax-ppss))))

(defun kixtart--match-string-as-token ()
  "Return the current `match-string' data as a syntax token."
  (intern-soft (concat "kixtart-"
                       (downcase (match-string-no-properties 0))
                       "-t")))

;;;; Block parser

(cl-defstruct (kixtart-block-state (:constructor kixtart-make-block-state)
                                   (:copier nil))
  string
  token
  position)

(defun kixtart--parse-block-state ()
  "Scan backwards and return the current block state."
  ;; Move out of strings and comments.
  (save-excursion
    (while (kixtart--in-comment-or-string-p)
      (backward-up-list nil t t))
    ;; Search backwards matching pairs of script-block defining keyword tokens.
    (let ((parse-sexp-ignore-comments t)
          block-end
          block-start)
      (condition-case nil
          (while (and (not (bobp))
                      (null block-start))
            (forward-sexp -1)
            (cond ((looking-at (kixtart-rx script-block-open))
                   (let ((open-token (kixtart--match-string-as-token)))
                         ;; Try to match this current script-block opening token
                         ;; with the most recently seen script-block closing
                         ;; token.
                         (unless (pcase `(,open-token ,(car block-end))
                                   ;; No script-block close.
                                   (`(,_ nil))
                                   ;; Ignore "CASE" and "ELSE" since they
                                   ;; effectively close and re-open a script-block.
                                   (`(,(or 'kixtart-case-t 'kixtart-else-t) ,_) t)
                                   ;; Matching token pairs.
                                   ((or '(kixtart-do-t       kixtart-until-t)
                                        '(kixtart-for-t      kixtart-next-t)
                                        '(kixtart-function-t kixtart-endfunction-t)
                                        `(kixtart-if-t       ,(or 'kixtart-else-t
                                                                  'kixtart-endif-t))
                                        '(kixtart-select-t   kixtart-endselect-t)
                                        '(kixtart-while-t    kixtart-loop-t))
                                    (pop block-end)))
                           (setq block-start open-token))))
                  ((looking-at (kixtart-rx script-block-close))
                   (push (kixtart--match-string-as-token) block-end))))
        (scan-error
         (backward-up-list nil t t)))
      (kixtart-make-block-state
       :position (point)
       :token block-start
       :string (and block-start
                    (buffer-substring-no-properties (point)
                                                    (progn
                                                      (forward-sexp)
                                                      (point))))))))

;;;; Motion

(defun kixtart-beginning-of-defun (&optional arg)
  "Move backwards to the beginning of a function definition.
With ARG, do it that many times.  Negative ARG means move
forwards to the ARGth following beginning of defun.

If search is successful, return t.  Success is defined to be any
successful match in ARG attempts to move.  Point ends up at the
beginning of the line where the search succeeded.  Otherwise,
return nil."
  (interactive "^p")
  (unless arg (setq arg 1))
  (let* ((forwards (< arg 0))
         (search-fn (if forwards #'re-search-forward #'re-search-backward))
         (inc-fn (if forwards #'1+ #'1-))
         (match-pos nil))
    (save-excursion
      ;; Ensure that searching forwards doesn't match the current position.
      (when (and forwards (looking-at-p (kixtart-rx command-function)))
        (forward-char 8))
      ;; Search for the arg-th FUNCTION command in the given direction.
      (while (and (not (zerop arg))
                  (funcall search-fn (kixtart-rx command-function) nil t)
                  (or (kixtart--in-comment-or-string-p)
                      (setq arg (funcall inc-fn arg) match-pos (point))))))
    ;; Ensure point is at the beginning of the match.
    (when match-pos
      (goto-char (if forwards (- match-pos 8) match-pos)))
    (not (null match-pos))))

(defun kixtart-end-of-defun ()
  "Move forwards to the end of a function definition."
  (let ((match-pos nil))
    (save-excursion
      (while (and (re-search-forward (kixtart-rx command-endfunction) nil t)
                  (or (kixtart--in-comment-or-string-p)
                      (not (setq match-pos (point)))))))
    (when match-pos
      (goto-char match-pos))
    (not (null match-pos))))

(defun kixtart-up-script-block ()
  "Move point to the opening of the current script-block."
  (interactive)
  (goto-char (kixtart-block-state-position (kixtart--parse-block-state))))

;;;; Indentation

(defun kixtart--new-indent ()
  "Return the calculated indentation level for the current line."
  (save-excursion
    (back-to-indentation)
    (let ((ppss (syntax-ppss)))
      (if (kixtart--in-comment-or-string-p ppss)
          (current-column)
        (let ((paren-depth (kixtart--paren-depth ppss))
              (paren-close (looking-at-p "\\s)"))
              (line-token (and (looking-at (kixtart-rx script-block-close))
                               (kixtart--match-string-as-token)))
              (block-state (kixtart--parse-block-state)))
          ;; Move to the position where the current script-block was opened
          (goto-char (kixtart-block-state-position block-state))
          (+ (current-indentation)
             (* kixtart-indent-offset
                (+
                 ;; Remove indentation which was already applied to the buffer
                 ;; position by opening parenthesis.
                 (- (kixtart--paren-depth))
                 ;; Add indentation based on parentheses.
                 (max 0 (if paren-close (1- paren-depth) paren-depth))
                 ;; Add indentation based on matching script-block tokens.
                 (pcase `(,(kixtart-block-state-token block-state)
                          ,line-token)
                   ;; Avoid further pattern matches where there is no
                   ;; script-block open.
                   (`(nil ,_) 0)
                   ;; Always match an opening "SELECT" to allow anything
                   ;; preceeding the first "CASE" block to align with the
                   ;; "SELECT" block.
                   (`(kixtart-select-t ,_) 0)
                   ;; Avoid further pattern matches for a script-block open
                   ;; without a script-block close.
                   (`(,_ nil) 1)
                   ;; Matching token pairs.
                   ((or '(kixtart-do-t       kixtart-until-t)
                        `(kixtart-case-t     ,(or 'kixtart-case-t
                                                  'kixtart-endselect-t))
                        '(kixtart-else-t     kixtart-endif-t)
                        '(kixtart-for-t      kixtart-next-t)
                        '(kixtart-function-t kixtart-endfunction-t)
                        `(kixtart-if-t       ,(or 'kixtart-else-t
                                                  'kixtart-endif-t))
                        '(kixtart-while-t    kixtart-loop-t))
                    0)
                   ;; Default to increasing the indentation.
                   (_ 1))))))))))

(defun kixtart-indent-line ()
  "Indent the current line to match the script-block level.
When point is within the current indentation it will move to the
new indentation column."
  (let ((new-indent (kixtart--new-indent))
        (cur-indent (current-indentation)))
    (if (= new-indent cur-indent)
        'noindent
      (let ((goto-indentation (<= (current-column) cur-indent)))
        (save-excursion
          (indent-line-to new-indent))
        (when goto-indentation
          (back-to-indentation))))))

;;;; Outline mode

(defun kixtart-outline-level ()
  "Return the depth for the current outline heading."
  (if (looking-at-p (kixtart-rx command-function))
      most-positive-fixnum
    (save-excursion
      (forward-same-syntax)
      (- (current-column) 2))))

;;;; Templates

(defun kixtart--tempo-newline-eob ()
  "Insert a newline if point is at end of the buffer."
  (when (pcase kixtart-template-insert-newline
          ((and (pred functionp) func)
           (funcall func))
          ((and (pred identity) do-insert)
           do-insert))
    (insert "\n")))

(defun kixtart--tempo-insert-lookup (name template)
  "Optionally insert prompt data for NAME using TEMPLATE.
The template is only returned if the data lookup for NAME does
not return the empty string.  TEMPLATE will have its `p' symbols
removed when template insertion is interactive."
  (unless (string= "" (tempo-lookup-named name))
    (cons 'l (if tempo-interactive
                 (remove 'p template)
               template))))

(defconst kixtart-tempo-tags nil
  "Tempo tags for KiXtart Mode.")

(define-abbrev-table 'kixtart-mode-abbrev-table nil
  "Abbrev table for KiXtart Mode."
  :enable-function (lambda ()
                     (and kixtart-abbrev-table-enabled
                          (not (kixtart--in-comment-or-string-p)))))

(defmacro kixtart--define-template (tag documentation &rest elements)
  "Define a tempo template and add its tag to the abbrev table.
TAG, DOCUMENTATION, and ELEMENTS are passed directly to
`tempo-define-template'.  TAG is also used as the abbrev string
which will be expanded to the template."
  (let ((template (gensym)))
    `(let ((,template (tempo-define-template (concat "kixtart-" ,tag)
                                             (quote ,@elements)
                                             ,tag
                                             ,documentation
                                             kixtart-tempo-tags)))
       (define-abbrev kixtart-mode-abbrev-table ,tag "" (identity ,template)
         :system t)
       (put (identity ,template) 'no-self-insert t)
       (defalias (intern (concat "kixtart-template-" ,tag))
         (identity ,template)
         ,documentation))))

(kixtart--define-template
 "while"
 "Insert a KiXtart While loop."
 ("While " (p "while-expression: " expr) > n
  > r> n o
  "Loop" > (kixtart--tempo-newline-eob)))

(kixtart--define-template
 "select"
 "Insert a KiXtart Select statement."
 ("Select" > n
  > "Case " (p "case-expression: " expr) > n
  > r> n o
  "EndSelect" > (kixtart--tempo-newline-eob)))

(kixtart--define-template
 "ifelse"
 "Insert a KiXtart If Else statement."
 ("If " (p "if-expression: " expr) > n
  > r> n o
  "Else" > n
  > p n
  "EndIf" > (kixtart--tempo-newline-eob)))

(kixtart--define-template
 "if"
 "Insert a KiXtart If statement."
 ("If " (p "if-expression: " expr) > n
  > r> n o
  "EndIf" > (kixtart--tempo-newline-eob)))

(kixtart--define-template
 "function"
 "Insert a KiXtart Function definition."
 ("Function " (p "name: " name) (p "arguments: " args 'noinsert)
  (kixtart--tempo-insert-lookup 'args '("(" (s args) p ")" )) > n
  > r> n o
  "EndFunction" > (kixtart--tempo-newline-eob)))

(kixtart--define-template
 "foreach"
 "Insert a KiXtart For Each loop."
 ("For Each " (p "element-variable: " element) " In "
  (p "group-expression: " group) > n
  > r> n o
  "Next" > (kixtart--tempo-newline-eob)))

(kixtart--define-template
 "for"
 "Insert a KiXtart For loop."
 ("For $" (p "counter-variable: " counter)
  " = "
  (p "start-expression: " start) " To " (p "end-expression: " end)
  (p "step-size: " step 'noinsert)
  (kixtart--tempo-insert-lookup 'step '(" Step " (s step) p)) > n
  > r> n o
  "Next" > (kixtart--tempo-newline-eob)))

(kixtart--define-template
 "else"
 "Insert a KiXtart Else statement."
 (& "Else" > n
    > r> (kixtart--tempo-newline-eob)))

(kixtart--define-template
 "do"
 "Insert a KiXtart Do loop."
 ("Do" > n
  > r> n o
  "Until " > (p "until-expression: " expr) > (kixtart--tempo-newline-eob)))

(kixtart--define-template
 "case"
 "Insert a KiXtart Case statement."
 (& "Case " (p "case-expression: " expr) > n
    > r (kixtart--tempo-newline-eob)))

;;;; Keymap

(defvar kixtart-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-j") #'imenu)
    (define-key map (kbd "C-c C-t C-b") #'tempo-backward-mark)
    (define-key map (kbd "C-c C-t C-f") #'tempo-forward-mark)
    (define-key map (kbd "C-c C-t C-t") #'tempo-complete-tag)
    (define-key map (kbd "C-c C-t I") 'kixtart-template-ifelse)
    (define-key map (kbd "C-c C-t c") 'kixtart-template-case)
    (define-key map (kbd "C-c C-t d") 'kixtart-template-do)
    (define-key map (kbd "C-c C-t e") 'kixtart-template-foreach)
    (define-key map (kbd "C-c C-t f") 'kixtart-template-for)
    (define-key map (kbd "C-c C-t i") 'kixtart-template-if)
    (define-key map (kbd "C-c C-t l") 'kixtart-template-else)
    (define-key map (kbd "C-c C-t s") 'kixtart-template-select)
    (define-key map (kbd "C-c C-t u") 'kixtart-template-function)
    (define-key map (kbd "C-c C-t w") 'kixtart-template-while)
    (define-key map (kbd "C-c C-u") #'kixtart-up-script-block)
    map))

;;;; Syntax table

;; Note that the ? character is left as punctuation even though it is
;; technically a command.  This seems to best represent how the original parser
;; works.  "?:mylabel", "?myfunction", "?command", "?$var=1" are all valid.

(defconst kixtart-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Add ' for string-quotes.
    (modify-syntax-entry ?' "\"" table)
    ;; Set line comment start and end.
    (modify-syntax-entry ?\; "\<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Set block comment open and close.  Nesting is not supported.
    (modify-syntax-entry ?\/ ". 14b" table)
    (modify-syntax-entry ?\* ". 23b" table)
    ;; Set punctuation.  The numeric operators * / ^ and ~ are already set.
    (modify-syntax-entry ?\+ "." table)
    (modify-syntax-entry ?\- "." table)
    (modify-syntax-entry ?\= "." table)
    (modify-syntax-entry ?\< "." table)
    (modify-syntax-entry ?\> "." table)
    (modify-syntax-entry ?\& "." table)
    (modify-syntax-entry ?\| "." table)
    ;; Set allowed symbol constituents.
    (modify-syntax-entry ?\! "_" table)
    (modify-syntax-entry ?\# "_" table)
    (modify-syntax-entry ?\$ "_" table)
    (modify-syntax-entry ?\% "_" table)
    (modify-syntax-entry ?\: "_" table)
    (modify-syntax-entry ?\@ "_" table)
    (modify-syntax-entry ?\\ "_" table)
    (modify-syntax-entry ?\_ "_" table)
    (modify-syntax-entry ?\` "_" table)
    (modify-syntax-entry ?\{ "_" table)
    (modify-syntax-entry ?\} "_" table)
    table))

;;;; Mode

;;;###autoload
(define-derived-mode kixtart-mode prog-mode "KiXtart Mode"
  "Major mode for editing KiXtart files."
  (setq mode-name "KiXtart")
  (setq-local comment-start ";")
  (setq-local font-lock-defaults
              `(((,(kixtart-rx macro)
                  (1 font-lock-type-face) (2 font-lock-warning-face))
                 (,(kixtart-rx macro-format) . font-lock-warning-face)
                 (,(kixtart-rx function)     . font-lock-builtin-face)
                 (,(kixtart-rx function-def)
                  (1 font-lock-keyword-face) (2 font-lock-function-name-face))
                 (,(kixtart-rx command)      . font-lock-keyword-face)
                 (,(kixtart-rx label)        . font-lock-constant-face)
                 (,(kixtart-rx variable)     . font-lock-variable-name-face))
                nil t))
  (setq-local beginning-of-defun-function #'kixtart-beginning-of-defun)
  (setq-local end-of-defun-function #'kixtart-end-of-defun)
  (setq-local indent-line-function #'kixtart-indent-line)
  (setq-local outline-level #'kixtart-outline-level)
  (setq-local outline-regexp (kixtart-rx outline))
  (setq imenu-create-index-function #'imenu-default-create-index-function)
  (setq imenu-generic-expression `((nil ,(kixtart-rx function-def) 2)
                                   ("/Labels" ,(kixtart-rx label) 0)))
  (tempo-use-tag-list 'kixtart-tempo-tags))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kix\\'" . kixtart-mode))

(provide 'kixtart-mode)
;;; kixtart-mode.el ends here
