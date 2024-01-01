;;; kixtart-mode.el --- Major mode for editing KiXtart files -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Morgan Willcock

;; Author: Morgan Willcock <morgan@ice9.digital>
;; Keywords: languages
;; Maintainer: Morgan Willcock <morgan@ice9.digital>
;; Package-Requires: ((emacs "27.1"))
;; URL: https://git.sr.ht/~mew/kixtart-mode
;; Version: 1.2.0

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

;;; News:

;; Version 1.2.0 (2024-01-01)
;; ==========================

;; Fixed indentation for hanging CASE and ELSE commands.

;; The function `kixtart-up-script-block' can now be repeated with the
;; repeat-map binding 'u'.

;; Unless the value of the new customization variable
;; `kixtart-block-motion-push-mark' has been customized to nil the function
;; `kixtart-up-script-block' now pushes the previous location to the mark ring
;; when the value of point is modified.

;; Font-locking now uses custom faces which inherit from the previously used
;; default font-lock faces.

;; The command `kixtart-close-command-block' now re-indents the current line
;; when KiXtart commands are inserted or removed from the buffer.

;; The command `kixtart-close-command-block' prints a message when point is not
;; within an open command block.

;; Fixed indentation not being applied to the first line of syntax in the
;; buffer.

;; Version 1.1.1 (2023-03-14)
;; ==========================

;; Fixed retrieving the current function name when the buffer is narrowed.

;; Fixed incorrect multi-line fontification of a symbol as a function name when
;; the word "function" appeared within a previous comment.

;; Version 1.1.0 (2023-02-17)
;; ==========================

;; Fontification for function names now works where the function name appears on
;; a later line than the FUNCTION command.

;; The Imenu index now includes functions where the function name appears on a
;; later line than the FUNCTION command.

;; The Imenu index entries for labels now have the ":" prefix removed.

;; Macros are now fontified with the `font-lock-preprocessor-face' face.

;; Font-lock rules now includes a match for object properties, fontified using
;; the `font-lock-type-face' face.  This addition helps to reduce the chance of
;; incorrect font-lock highlighting performed by later rules.

;; The function `kixtart-current-defun' is now available to return the name of
;; the user-defined KiXtart function which currently surrounds point.

;; Support was added for `add-log' functions, which allow the current function
;; name to be automatically added to change log entries.

;; Support was added for `which-function-mode', a minor-mode which displays the
;; current function name in the mode-line.

;; Fixed the use of multi-line comment indicators at the beginning of the
;; buffer and within strings.

;; Fixed the use of multi-line separators within strings.

;; Version 1.0.0 (2022-12-04)
;; ==========================

;; Initial release.

;;; Commentary:

;; Overview
;; ========

;; KiXtart Mode is an Emacs major mode for creating and editing KiXtart script
;; files.  The following features are implemented:

;; - Syntax highlighting for commands, functions, labels, and macros
;; - Indentation based on the usage of commands and parentheses
;; - Motion around and selection of defined functions
;; - Imenu support for function and label names
;; - Current function name for which-function-mode and add-log functions
;; - Outline Mode support
;; - Predefined Tempo templates with optional abbrev expansion

;; Installation
;; ------------

;; Clone this repository to a local sub-directory:

;;   git clone https://git.sr.ht/~mew/kixtart-mode

;; Within the Emacs configuration, add the path of the cloned directory to the
;; value of `load-path'.  The following example assumes that the repository was
;; cloned within a directory named "manual-packages" within the user's
;; ".emacs.d" directory:

;;   (add-to-list 'load-path "~/.emacs.d/manual-packages/kixtart-mode")

;; Example configuration
;; ---------------------

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

;;   ;; Default to using the dos coding system (ASCII character set and CRLF
;;   ;; line endings) for file names with a .kix extension.
;;   (add-to-list 'file-coding-system-alist (cons "\\.kix\\'" 'dos))

;; Syntax highlighting
;; ===================

;; Syntax highlighting is implemented for all KiXtart language keywords using
;; the following font-lock faces:

;; Keyword type            Face
;; -------------------------------------------------------
;; Command                 `kixtart-command-face'
;; Function (built-in)     `kixtart-function-face'
;; Function (definition)   `kixtart-function-name-face'
;; Label                   `kixtart-label-face'
;; Macro                   `kixtart-macro-face'
;; Property                `kixtart-property-face'
;; Variable                `kixtart-variable-face'

;; The additional face `kixtart-warning-face' is used to highlight portions of
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

;; Indentation
;; ===========

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

;; Multi-line expressions
;; ----------------------

;; Additional indentation is applied to a line where the previous script line
;; ends in a comma.  This provides automatic indentation for KiXtart commands
;; and expressions which expect comma-separated tokens:

;;   $x = 1, 2, 3,
;;       4, 5, 6,
;;       *

;; For tokens which are not comma-separated, it is currently not possible to
;; manage indentation across lines automatically.  As a workaround, additional
;; indentation is applied when a special comment indicates that the current line
;; is ending in the middle of an expression.  The presence of the special
;; comment ";\" appearing at the end of the previous script line will increase
;; the indentation of the current line.

;;   COPY $source ;\
;;       $destination ;\
;;       *

;; Motion commands
;; ===============

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
;; When the value of the customization variable `kixtart-block-motion-push-mark'
;; is non-nil the previous location will be pushed to the mark ring when the
;; value of point is modified.

;; KiXtart Mode binds `kixtart-up-script-block' to 'C-c C-u' by default.  If
;; `repeat-mode' is active the command may be repeated through the use of the
;; repeat-map binding 'u'.

;; Imenu support
;; =============

;; The `imenu' command provides code indexing and navigation options which
;; allow point to jump to indexed buffer positions.  The `imenu' menu is
;; presented as a list of completions which contains the names of all
;; defined functions, as well as a single "/Labels" sub-menu entry which
;; lists all defined label names (the "/" character is chosen as a sub-menu
;; prefix because it is invalid as part of a function name).

;; For very large scripts scanning for index entries can potentially be slow,
;; although automatic re-scanning may be preferable to manually invoking the
;; "*Rescan*" option from the menu.  Consider configuring `imenu-auto-rescan'
;; and `imenu-auto-rescan-maxout' to suitable values.

;; KiXtart Mode binds `imenu' to 'C-c C-j' by default.

;; Current function name
;; =====================

;; The function `kixtart-current-defun' will return the name of the user-defined
;; KiXtart function which currently surrounds point, or nil if point is
;; currently outside of a function.  This function is also used internally to
;; provide support for:

;; - `add-log' functions, which allow the current function name to be
;;   automatically added to change log entries

;; - `which-function-mode', a minor-mode which displays the current function
;;   name in the mode-line

;; Note: The customization variable `kixtart-which-function-default-name' is
;; available and can override the function name which is displayed when point is
;; currently outside of a function.  Setting this value to nil will revert to
;; the original `which-function-mode' behavior of displaying a name retrieved
;; from Imenu data.

;; Outline Mode support
;; ====================

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

;; Templates
;; =========

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

;; Abbrev expansion
;; ----------------

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

;; Template case control
;; ---------------------

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

;; Predefined templates
;; --------------------

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

;; Closing open blocks
;; ===================

;; As an alternative to inserting fully templated code into the buffer, the
;; command `kixtart-close-command-block' will insert a KiXtart command which can
;; close the currently open script block, where an open block is determined by
;; looking backwards from point.  Repeatedly invoking the command will cycle
;; through the possible options, with the previously inserted option being
;; removed.  When no further options are available the previously inserted
;; option will be removed and no new option will be inserted.

;; The case of the inserted KiXtart command will be determined by the case of
;; the block opening.  For example, "WHILE" will be closed by "LOOP", "while"
;; will be closed "loop", and any other case-variation will be closed by "Loop".

;; KiXtart Mode binds `kixtart-close-command-block' to 'C-c C-c' by default.  If
;; `repeat-mode' is active the command may be repeated through the use of the
;; repeat-map bindings 'C-c' and 'c'.

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

(defcustom kixtart-block-motion-push-mark t
  "Specifies whether block motion will push to the `mark-ring'.
A non-nil value indicates that block motion commands are
permitted to push the previous location to the `mark-ring' when
the value of point changes."
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

(defcustom kixtart-which-function-default-name "-"
  "Specifies the default function name provided to `which-function-mode'.
This name is provided as the function name when point is outside
of a function.  When the value is set to nil
`which-function-mode' falls back to matching based on the latest
Imenu index data."
  :type '(choice (string :tag "Default name")
                 (const :tag "Use Imenu index" nil)))

;;;; Faces

(defgroup kixtart-faces nil
  "Faces used by KiXtart Mode."
  :group 'kixtart
  :prefix "kixtart-")

(defface kixtart-command-face
  '((default :inherit font-lock-keyword-face))
  "Face to highlight a KiXtart command.")
(defvar kixtart-command-face
  'kixtart-command-face
  "Face specification to highlight a KiXtart command.")

(defface kixtart-function-face
  '((default :inherit font-lock-builtin-face))
  "Face to highlight a KiXtart function name.")
(defvar kixtart-function-face
  'kixtart-function-face
  "Face specification to highlight a KiXtart function name.")

(defface kixtart-function-name-face
  '((default :inherit font-lock-function-name-face))
  "Face to highlight a KiXtart function definition.")
(defvar kixtart-function-name-face
  'kixtart-function-name-face
  "Face specification to highlight a KiXtart function definition.")

(defface kixtart-label-face
  '((default :inherit font-lock-constant-face))
  "Face to highlight a KiXtart label name.")
(defvar kixtart-label-face
  'kixtart-label-face
  "Face specification to highlight a KiXtart label name.")

(defface kixtart-macro-face
  '((default :inherit font-lock-preprocessor-face))
  "Face to highlight a KiXtart macro.")
(defvar kixtart-macro-face
  'kixtart-macro-face
  "Face specification to highlight a KiXtart macro.")

(defface kixtart-property-face
  '((default :inherit font-lock-type-face))
  "Face to highlight a KiXtart property name.")
(defvar kixtart-property-face
  'kixtart-property-face
  "Face specification to highlight a KiXtart property name.")

(defface kixtart-variable-face
  '((default :inherit font-lock-variable-name-face))
  "Face to highlight a KiXtart variable.")
(defvar kixtart-variable-face
  'kixtart-variable-face
  "Face specification to highlight a KiXtart variable.")

(defface kixtart-warning-face
  '((default :inherit font-lock-warning-face))
  "Face to highlight a KiXtart syntax warning.")
(defvar kixtart-warning-face
  'kixtart-warning-face
  "Face specification to highlight a KiXtart syntax warning.")

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
            (dot-property
             ;; Assume that object properties cannot start with a number, which
             ;; is probably true and prevents matching floating point numbers.
             (seq ?. (1+ (intersection user-chars (not (char (?0 . ?9)))))
                  (0+ user-chars)))
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
            (function-name
             ;; Function names cannot start with a character which wrongly
             ;; identifies the name as a label, macro, or variable.
             (seq (1+ (intersection user-chars (not (char ?$ ?: ?@))))
                  (0+ user-chars)))
            (label
             (seq symbol-start ?: (1+ user-chars)))
            (macro
             (seq ?@
                  (or "address" "build" "color" "comment" "cpu" "crlf" "csd"
                      "curdir" "date" "day" "domain" "dos" "error" "fullname"
                      "homedir" "homedrive" "homeshr" "hostname" "inudf" "inwin"
                      "ipaddress0" "ipaddress1" "ipaddress2" "ipaddress3" "kix"
                      "lanroot" "ldomain" "ldrive" "lm" "logonmode"
                      "longhomedir" "lserver" "maxpwage" "mdayno" "mhz" "month"
                      "monthno" "msecs" "onwow64" "pid" "primarygroup" "priv"
                      "productsuite" "producttype" "programfilesx86" "pwage"
                      "ras" "releaseid" "releasename" "result" "rserver"
                      "scriptdir" "scriptexe" "scriptname" "serror" "sid" "site"
                      "startdir" "syslang" "ticks" "time" "tssession" "userid"
                      "userlang" "wdayno" "wksta" "wuserid" "ydayno" "year")))
            (macro-format
             ;; Match anything which has the appearance of a macro.
             (seq ?@ (1+ user-chars)))
            (multiline-indicator
             ;; Special comment to indicate that the current command or
             ;; expression is unterminated and will continue on a later line.
             (seq ?\; ?\\ line-end))
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

;;;; Font lock

;; Prevent the use of lexical binding for font-lock boundaries.
(defvar font-lock-beg)
(defvar font-lock-end)

(defun kixtart--font-lock-extend-region-function-def ()
  "Move fontification boundaries to include function keyword and name.
It is assumed that this function is added into the
`font-lock-extend-region-functions' list in a position where it
will be called after the `font-lock-extend-region-wholelines'
function."
  (cl-flet ((after-func (from)
              (goto-char from)
              (forward-comment (- (point)))
              (and (looking-back (kixtart-rx command-function) (- (point) 8))
                   (match-beginning 0)))
            (before-name (from)
              (goto-char from)
              (forward-comment (point-max))
              (and (looking-at (kixtart-rx function-name))
                   (match-end 0))))
    (let (changed)
      (when (before-name font-lock-beg)
        (when-let ((func-beg (after-func font-lock-beg)))
          (setq font-lock-beg func-beg)
          (setq changed t)))
      (when (after-func font-lock-end)
        (when-let ((name-end (before-name font-lock-end)))
          (setq font-lock-end name-end)
          (setq changed t)))
      changed)))

(defvar kixtart-font-lock-keywords
  `((,(kixtart-rx (group macro) (group (0+ user-chars)))
     ;; The real parser seems to silently discard the trailing part of a
     ;; macro name if the leading part matches an actual macro name.
     (1 kixtart-macro-face) (2 kixtart-warning-face))
    ;; Unknown macros will always evaluate to 0.
    (,(kixtart-rx macro-format) . kixtart-warning-face)
    (,(kixtart-rx dot-property) . kixtart-property-face)
    (,(kixtart-rx function)     . kixtart-function-face)
    (,(kixtart-rx label)        . kixtart-label-face)
    (,(kixtart-rx variable)     . kixtart-variable-face)
    (,(kixtart-rx command-function) (0 kixtart-command-face)
     ;; Anchored match for function name.
     (,(kixtart-rx function-name)
      (unless (kixtart--in-comment-or-string-p)
        (forward-comment (point-max))
        (if (looking-at (kixtart-rx function-name))
            (match-end 0)
          ;; Skip highlighting anything else on this line by leaving point at
          ;; the end of the line.
          (end-of-line)))
      nil
      (0 kixtart-function-name-face)))
    (,(kixtart-rx command)      . kixtart-command-face)))

;;;; Utility

(defun kixtart--follows-eol-multiline-separator-p ()
  "Return a non-nil value when the current line begins mid-list.
Being within a list is determined by the previous script line
ending in a \",\" character, ignoring any trailing whitespace or
comments."
  (save-excursion
    (beginning-of-line)
    (and (not (kixtart--in-comment-or-string-p))
         (progn
           (forward-comment (- (point)))
           (eq (char-before) ?,)))))

(defun kixtart--follows-eol-multiline-indicator-p ()
  "Return a non-nil value when following a mutliline indicator.
Being within a multiline expression is indicated by the previous
script line ending with the special comment \";\\\"."
  (save-excursion
    (beginning-of-line)
    (and (not (kixtart--in-comment-or-string-p))
         (progn
           (forward-comment (- (point)))
           (cl-plusp (current-column)))
         (re-search-forward (kixtart-rx multiline-indicator)
                            (line-end-position)
                            t))))

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

(cl-defstruct (kixtart-block (:constructor kixtart-make-block)
                             (:copier nil))
  in-parens
  token
  token-string
  position)

(defun kixtart--parse-block ()
  "Scan backwards and return the current block state."
  ;; Move out of strings and comments.
  (save-excursion
    (while (kixtart--in-comment-or-string-p)
      (backward-up-list nil t t))
    ;; Search backwards matching pairs of script-block defining keyword tokens.
    (let ((parse-sexp-ignore-comments t)
          block-end
          block-start
          token-string)
      (condition-case nil
          (while (and (not (bobp))
                      (null block-start))
            (forward-sexp -1)
            (cond ((looking-at (kixtart-rx script-block-open))
                   (pcase (cons (kixtart--match-string-as-token)
                                (car block-end))
                     (`(,open-token . nil)
                      (setq block-start open-token
                            token-string (match-string-no-properties 0)))
                     ;; Matching token pairs.  Ignore opening "CASE" and "ELSE"
                     ;; since they effectively close and re-open a script-block.
                     ((or '(kixtart-do-t       . kixtart-until-t)
                          '(kixtart-for-t      . kixtart-next-t)
                          '(kixtart-function-t . kixtart-endfunction-t)
                          `(kixtart-if-t       . ,(or 'kixtart-else-t
                                                      'kixtart-endif-t))
                          '(kixtart-select-t   . kixtart-endselect-t)
                          '(kixtart-while-t    . kixtart-loop-t))
                      (pop block-end))))
                  ((looking-at (kixtart-rx script-block-close))
                   (push (kixtart--match-string-as-token) block-end))))
        (scan-error
         (backward-up-list nil t t)))
      (kixtart-make-block
       :in-parens (not (or block-start (bobp)))
       :position (point)
       :token block-start
       :token-string token-string))))

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
  (let* ((forwards (cl-minusp arg))
         (search-fn (if forwards #'re-search-forward #'re-search-backward))
         (inc-fn (if forwards #'1+ #'1-))
         match-pos)
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
  (let (match-pos)
    (save-excursion
      (while (and (re-search-forward (kixtart-rx command-endfunction) nil t)
                  (or (kixtart--in-comment-or-string-p)
                      (not (setq match-pos (point)))))))
    (when match-pos
      (goto-char match-pos))
    (not (null match-pos))))

(defun kixtart-up-script-block ()
  "Move point to the opening of the current script-block.
Unless prevented by the value of `kixtart-block-motion-push-mark'
the previous location is pushed to the `mark-ring' when the value
of point is modified."
  (interactive)
  (let ((from (point)))
    (goto-char (kixtart-block-position (kixtart--parse-block)))
    (unless (or (null kixtart-block-motion-push-mark)
                (and transient-mark-mode mark-active)
                (eq from (point)))
      (push-mark from))))

(defvar kixtart-up-script-block-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") #'kixtart-up-script-block)
    map))

(put 'kixtart-up-script-block
     'repeat-map
     'kixtart-up-script-block-repeat-map)

;;;; Indentation

(defun kixtart--new-indent ()
  "Return the calculated indentation level for the current line."
  (save-excursion
    (back-to-indentation)
    (let ((ppss (syntax-ppss)))
      (if (kixtart--in-comment-or-string-p ppss)
          (current-column)
        (let ((multiline-indicator (kixtart--follows-eol-multiline-indicator-p))
              (multiline-separator (kixtart--follows-eol-multiline-separator-p))
              (line-token (and (looking-at (kixtart-rx script-block-close))
                               (kixtart--match-string-as-token)))
              (block (kixtart--parse-block))
              (new-level (kixtart--paren-depth ppss)))
          ;; Remove one level of indentation when the current line begins by
          ;; closing one parenthesis level.
          (when (and (cl-plusp new-level)
                     (looking-at-p "\\s)"))
            (cl-decf new-level))
          ;; Move to the position where the current block was opened.
          (goto-char (kixtart-block-position block))
          ;; Remove indentation which was applied to the block opening position
          ;; by parentheses.
          (cl-decf new-level (kixtart--paren-depth))
          ;; Add indentation where the current line continues the previous line,
          ;; avoiding a cumulative effect for comma separated values in
          ;; parentheses (e.g. function parameters).
          (when (or multiline-indicator
                    (and multiline-separator
                         (not (kixtart-block-in-parens block))))
            (cl-incf new-level))
          ;; Add indentation based on matching script-block tokens.
          (when (pcase (cons (kixtart-block-token block) line-token)
                  ;; Avoid further pattern matches where there is no
                  ;; script-block open.
                  (`(nil . ,_))
                  ;; Always match an opening "SELECT" to allow anything
                  ;; preceeding the first "CASE" block to align with the
                  ;; "SELECT" block.
                  (`(kixtart-select-t . ,_))
                  ;; Avoid further pattern matches for a script-block open
                  ;; without a script-block close.
                  (`(,_ . nil) t)
                  ;; Matching token pairs.
                  ((or '(kixtart-do-t       . kixtart-until-t)
                       `(kixtart-case-t     . ,(or 'kixtart-case-t
                                                   'kixtart-endselect-t))
                       '(kixtart-else-t     . kixtart-endif-t)
                       '(kixtart-for-t      . kixtart-next-t)
                       '(kixtart-function-t . kixtart-endfunction-t)
                       `(kixtart-if-t       . ,(or 'kixtart-else-t
                                                   'kixtart-endif-t))
                       '(kixtart-while-t    . kixtart-loop-t)))
                  ;; Default to increasing the indentation.
                  (_ t))
            (cl-incf new-level))
          ;; For "CASE" or "ELSE" move to the block-opening position of the
          ;; current position, to align with the opening "SELECT" or "IF".
          (while (memq (kixtart-block-token block)
                       '(kixtart-case-t kixtart-else-t))
            (setq block (kixtart--parse-block))
            (goto-char (kixtart-block-position block)))
          (+ (if (bobp) 0 (current-indentation))
             (* new-level kixtart-indent-offset)))))))

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

;;;; Block closing

(defvar-local kixtart--close-command-strings nil
  "The list of strings which can close the currently open block.")

(defun kixtart--syntax-case-function (token)
  "Return the case function which best matches the string TOKEN."
  (cond ((string= (upcase token) token) #'upcase)
        ((string= (downcase token) token) #'downcase)))

(defun kixtart-close-command-block ()
  "Insert the command to close the currently open block."
  (interactive)
  (let ((tick (buffer-chars-modified-tick)))
    (if (and kixtart--close-command-strings
             (eq last-command #'kixtart-close-command-block))
        (backward-delete-char (length (pop kixtart--close-command-strings)))
      (setq kixtart--close-command-strings
            (let* ((block (kixtart--parse-block))
                   (close-list (pcase (kixtart-block-token block)
                                 ('kixtart-case-t     (list "Case" "EndSelect"))
                                 ('kixtart-do-t       (list "Until"))
                                 ('kixtart-else-t     (list "EndIf"))
                                 ('kixtart-for-t      (list "Next"))
                                 ('kixtart-function-t (list "EndFunction"))
                                 ('kixtart-if-t       (list "Else" "EndIf"))
                                 ('kixtart-select-t   (list "Case"))
                                 ('kixtart-while-t    (list "Loop")))))
              (pcase (kixtart-block-token-string block)
                ((and (pred stringp)
                      (app kixtart--syntax-case-function func)
                      (guard (functionp func)))
                 (mapcar func close-list))
                (_ close-list))))
      (unless kixtart--close-command-strings
        (message "No open command block to close.")))
    (when-let ((close-command (car kixtart--close-command-strings)))
      (insert close-command))
    (unless (eq tick (buffer-chars-modified-tick))
      (funcall indent-line-function))))

(defvar kixtart-close-command-block-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c") #'kixtart-close-command-block)
    (define-key map (kbd "c") #'kixtart-close-command-block)
    map))

(put 'kixtart-close-command-block
     'repeat-map
     'kixtart-close-command-block-repeat-map)

;;;; Current function.

(defun kixtart--current-defun ()
  "Internal implementation of `kixtart-current-defun'.
Return the function name which surrounds point.  When point is
not within a function return nil.  It is assumed that this
function is called with buffer restrictions removed."
  (save-excursion
    (let ((from (point))
          (func-beg (progn
                      ;; Move point beyond the current word if it looks like the
                      ;; FUNCTION command.
                      (pcase (current-word t)
                        ((and (pred stringp)
                              (pred (string-match-p
                                     (kixtart-rx command-function))))
                         (skip-syntax-forward "w")))
                      (and (kixtart-beginning-of-defun) (point)))))
      (and func-beg
           (pcase (and (kixtart-end-of-defun) (point))
             ((or (pred null) (pred (< from)))
              (goto-char (+ func-beg 8))
              (forward-comment (point-max))
              (and (looking-at (kixtart-rx function-name))
                   (match-string-no-properties 0))))))))

(defun kixtart-current-defun ()
  "Return the function name which surrounds point.
When point is not within a function return nil."
  (save-restriction
    (widen)
    (kixtart--current-defun)))

(defun kixtart-which-function ()
  "Return the function name which surrounds point.
When point is not within a function return the value of
`kixtart-which-function-default-name'."
  (or (kixtart-current-defun) kixtart-which-function-default-name))

;;;; Imenu

(defun kixtart--create-imenu-index ()
  "Build and return an index alist suitable for Imenu.
Functions are added at the top level of the menu.  Labels are
added into a sub-menu."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (let (labels index)
        (while (re-search-backward
                (kixtart-rx (or command-function label)) nil t)
          (cond ((kixtart--in-comment-or-string-p))
                ((eq (char-after) ?:)
                 ;; Add label names.
                 (push (cons (substring (match-string-no-properties 0) 1)
                             (point))
                       labels))
                (t
                 ;; Add function names.
                 (when-let ((name (kixtart--current-defun)))
                   (push (cons name (point)) index)))))
        (when labels
          (push (cons "/Labels" labels) index))
        index))))


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
          ((and (pred functionp) func) (funcall func))
          (do-insert do-insert))
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

(defvar kixtart-tempo-tags nil
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
  (declare (indent 2))
  (let ((funname (intern (concat "kixtart-template-" tag))))
    `(progn
       (defalias ',funname
         (tempo-define-template (concat "kixtart-" ,tag)
                                (quote ,@elements)
                                ,tag
                                ,documentation
                                'kixtart-tempo-tags))
       (define-abbrev kixtart-mode-abbrev-table ,tag "" #',funname
         :system t)
       (put ',funname 'no-self-insert t))))

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
    (define-key map (kbd "C-c C-c") #'kixtart-close-command-block)
    (define-key map (kbd "C-c C-j") #'imenu)
    (define-key map (kbd "C-c C-t C-b") #'tempo-backward-mark)
    (define-key map (kbd "C-c C-t C-f") #'tempo-forward-mark)
    (define-key map (kbd "C-c C-t C-t") #'tempo-complete-tag)
    (define-key map (kbd "C-c C-t I") #'kixtart-template-ifelse)
    (define-key map (kbd "C-c C-t c") #'kixtart-template-case)
    (define-key map (kbd "C-c C-t d") #'kixtart-template-do)
    (define-key map (kbd "C-c C-t e") #'kixtart-template-foreach)
    (define-key map (kbd "C-c C-t f") #'kixtart-template-for)
    (define-key map (kbd "C-c C-t i") #'kixtart-template-if)
    (define-key map (kbd "C-c C-t l") #'kixtart-template-else)
    (define-key map (kbd "C-c C-t s") #'kixtart-template-select)
    (define-key map (kbd "C-c C-t u") #'kixtart-template-function)
    (define-key map (kbd "C-c C-t w") #'kixtart-template-while)
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
    (modify-syntax-entry ?* ". 23b" table)
    ;; Set punctuation.  The numeric operators * / ^ and ~ are already set.
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?| "." table)
    ;; Set allowed symbol constituents.
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?# "_" table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?% "_" table)
    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?\\ "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?` "_" table)
    (modify-syntax-entry ?{ "_" table)
    (modify-syntax-entry ?} "_" table)
    table))

;;;; Mode

;;;###autoload
(define-derived-mode kixtart-mode prog-mode "KiXtart Mode"
  "Major mode for editing KiXtart files."
  (setq mode-name "KiXtart")
  (setq-local comment-start ";")
  (setq-local font-lock-defaults '(kixtart-font-lock-keywords nil t))
  (setq-local beginning-of-defun-function #'kixtart-beginning-of-defun)
  (setq-local end-of-defun-function #'kixtart-end-of-defun)
  (setq-local indent-line-function #'kixtart-indent-line)
  (setq-local add-log-current-defun-function #'kixtart-current-defun)
  (setq-local outline-level #'kixtart-outline-level)
  (setq-local outline-regexp (kixtart-rx outline))
  (setq imenu-create-index-function #'kixtart--create-imenu-index)
  (tempo-use-tag-list 'kixtart-tempo-tags)
  (add-hook 'which-func-functions 'kixtart-which-function nil t)
  (add-to-list 'font-lock-extend-region-functions
               #'kixtart--font-lock-extend-region-function-def t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kix\\'" . kixtart-mode))

(provide 'kixtart-mode)
;;; kixtart-mode.el ends here
