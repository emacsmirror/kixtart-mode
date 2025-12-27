;;; kixtart-mode.el --- Major mode for editing KiXtart scripts -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Morgan Willcock

;; Author: Morgan Willcock <morgan@ice9.digital>
;; Keywords: languages
;; Maintainer: Morgan Willcock <morgan@ice9.digital>
;; Package-Requires: ((emacs "28.1") (eldoc "1.14.0"))
;; URL: https://git.sr.ht/~mew/kixtart-mode
;; Version: 1.4.3

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

;; Support for editing KiXtart scripts.

;; See the KiXtart Mode manual for configuration examples and descriptions of
;; available editing features.

;;; Code:

(require 'align)
(require 'compile)
(require 'etags)
(require 'imenu)
(require 'tempo)
(eval-when-compile
  (require 'cl-lib)
  ;; For `when-let*' and `and-let*' in Emacs 28.
  (when (< emacs-major-version 29)
    (require 'subr-x)))

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

;;;; Customization

(defgroup kixtart nil
  "Major mode for editing KiXtart scripts."
  :tag "KiXtart"
  :link '(custom-manual "(kixtart-mode)Top")
  :group 'languages
  :prefix "kixtart-")

;;;; Keywords

(eval-and-compile
  (defvar kixtart-keyword-commands
    '("?" "Beep" "Big" "Break" "Call" "Case" "CD" "CLS" "Color" "Cookie1" "Copy"
      "Debug" "Del" "Dim" "Display" "Do" "Each" "Else" "EndFunction" "EndIf"
      "EndSelect" "Exit" "FlushKB" "For" "Function" "Get" "GetS" "Global" "Go"
      "GoSub" "Goto" "If" "Include" "Loop" "MD" "Move" "Next" "Password" "Play"
      "Quit" "RD" "ReDim" "Return" "Run" "Select" "Set" "SetL" "SetM" "SetTime"
      "Shell" "Sleep" "Small" "Until" "Use" "While"))
  (defvar kixtart-keyword-functions
    '("Abs" "AddKey" "AddPrinterConnection" "AddProgramGroup" "AddProgramItem"
      "Asc" "AScan" "At" "BackupEventLog" "Box" "CDbl" "Chr" "CInt"
      "ClearEventLog" "Close" "CompareFileTimes" "CreateObject" "CStr"
      "DecToHex" "DelKey" "DelPrinterConnection" "DelProgramGroup"
      "DelProgramItem" "DelTree" "DelValue" "Dir" "EnumGroup" "EnumIPInfo"
      "EnumKey" "EnumLocalGroup" "EnumValue" "Execute" "Exist" "ExistKey"
      "ExpandEnvironmentVars" "Fix" "FormatNumber" "FreeFileHandle"
      "GetCommandLine" "GetDiskSpace" "GetFileAttr" "GetFileSize" "GetFileTime"
      "GetFileVersion" "GetObject" "IIf" "InGroup" "InStr" "InStrRev" "Int"
      "IsDeclared" "Join" "KBHit" "KeyExist" "LCase" "Left" "Len" "LoadHive"
      "LoadKey" "LogEvent" "LogOff" "LTrim" "MemorySize" "MessageBox" "Open"
      "ReadLine" "ReadProfileString" "ReadType" "ReadValue" "RedirectOutput"
      "Replace" "Right" "Rnd" "Round" "RTrim" "SaveKey" "SendKeys" "SendMessage"
      "SetASCII" "SetConsole" "SetDefaultPrinter" "SetFileAttr" "SetFocus"
      "SetOption" "SetSystemState" "SetTitle" "SetWallpaper" "ShowProgramGroup"
      "Shutdown" "SIDToName" "Split" "SRnd" "SubStr" "Trim" "UBound" "UCase"
      "UnloadHive" "Val" "VarType" "VarTypeName" "WriteLine"
      "WriteProfileString" "WriteValue"))
  (defvar kixtart-keyword-macros
    '("@Address" "@Build" "@Color" "@Comment" "@CPU" "@CRLF" "@CSD" "@CurDir"
      "@Date" "@Day" "@Domain" "@DOS" "@Error" "@FullName" "@HomeDir"
      "@HomeDrive" "@HomeShr" "@HostName" "@InUDF" "@InWin" "@IPAddress0"
      "@IPAddress1" "@IPAddress2" "@IPAddress3" "@Kix" "@LanRoot" "@LDomain"
      "@LDrive" "@LM" "@LogonMode" "@LongHomeDir" "@LServer" "@MaxPwAge"
      "@MDayNo" "@Mhz" "@Month" "@MonthNo" "@MSecs" "@OnWOW64" "@PID"
      "@PrimaryGroup" "@Priv" "@ProductSuite" "@ProductType" "@ProgramFilesx86"
      "@PwAge" "@RAS" "@ReleaseID" "@ReleaseName" "@Result" "@RServer"
      "@ScriptDir" "@ScriptExe" "@ScriptName" "@SError" "@SID" "@Site"
      "@StartDir" "@SysLang" "@Ticks" "@Time" "@TSSession" "@UserID" "@UserLang"
      "@WDayNo" "@WkSta" "@WUserID" "@YDayNo" "@Year")))

(defvar kixtart-keywords
  (eval-when-compile
    (sort (append kixtart-keyword-commands
                  kixtart-keyword-functions
                  (copy-sequence kixtart-keyword-macros))
          #'string-lessp)))

;;;; Search patterns

(defmacro kixtart-rx (&rest regexps)
  "Extended version of `rx' for translation of form REGEXPS."
  `(rx-let ((command
             (seq symbol-start
                  (or (1+ ??) ,@(cdr kixtart-keyword-commands))
                  symbol-end))
            (command-endfunction
             (seq symbol-start "endfunction" symbol-end))
            (command-function
             (seq symbol-start "function" symbol-end))
            (command-global
             (seq symbol-start "global" symbol-end))
            (dot-property
             ;; Assume that object properties cannot start with a number, which
             ;; is probably true and prevents matching floating point numbers.
             (seq ?. (1+ (intersection user-chars (not (char (?0 . ?9)))))
                  (0+ user-chars)))
            (function
             (seq symbol-start
                  (or ,@kixtart-keyword-functions)
                  symbol-end))
            (function-name
             ;; Function names cannot start with a character which wrongly
             ;; identifies the name as a label, macro, or variable.
             (seq (1+ (intersection user-chars (not (char ?$ ?: ?@))))
                  (0+ user-chars)))
            (label
             (seq symbol-start ?: (1+ user-chars)))
            (macro
             (seq (or ,@kixtart-keyword-macros)))
            (macro-format
             ;; Match anything which has the appearance of a macro.
             (seq ?@ (1+ user-chars)))
            (multiline-indicator
             ;; Special comment to indicate that the current command or
             ;; expression is unterminated and will continue on a later line.
             (seq ?\; ?\\ line-end))
            (outline
             (seq line-start (or (seq command-function
                                      (opt (1+ whitespace) function-name))
                                 (seq (>= 3 (syntax \<))
                                      (1+ whitespace)
                                      (0+ not-newline)))))
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
    (let* ((case-fold-search t)
           (new-beg (and (before-name font-lock-beg)
                         (after-func font-lock-beg)))
           (new-end (and (after-func font-lock-end)
                         (before-name font-lock-end))))
      (when new-beg
        (setq font-lock-beg new-beg))
      (when new-end
        (setq font-lock-end new-end))
      (or new-beg new-end))))

(defconst kixtart-font-lock-keywords-1
  `((,(rx symbol-start
          (or "call" "endfunction" "function" "include")
          symbol-end)
     . kixtart-command-face))
  "Font lock keywords for level 1 highlighting in KiXtart mode.

Highlights the KiXtart commands used for function declarations
and for calling or including other scripts.")

(defconst kixtart-font-lock-keywords-2
  `((,(kixtart-rx command)      . kixtart-command-face)
    (,(kixtart-rx function)     . kixtart-function-face)
    (,(kixtart-rx (group macro) (group (0+ user-chars)))
     ;; The real parser seems to silently discard the trailing part of a macro
     ;; name if the leading part matches an actual macro name.
     (1 kixtart-macro-face) (2 kixtart-warning-face))
    ;; Unknown macros will always evaluate to 0.
    (,(kixtart-rx macro-format) . kixtart-warning-face))
  "Font lock keywords for level 2 highlighting in KiXtart mode.

Highlights all internal KiXtart commands, functions, and macros.")

(defconst kixtart-font-lock-keywords-3
  (append
   `((,(kixtart-rx dot-property) . kixtart-property-face)
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
       (0 kixtart-function-name-face))))
   kixtart-font-lock-keywords-2)
  "Font lock keywords for level 3 highlighting in KiXtart mode.

Highlights all internal KiXtart commands, functions, and macros,
as well as user-defined or external names.")

(defvar kixtart-font-lock-keywords kixtart-font-lock-keywords-1
  "Default expressions to highlight in KiXtart mode.")

;;;; Utility

(defun kixtart--follows-eol-multiline-separator-p ()
  "Return a non-nil value when the current line begins mid-list.

Being within a list is determined by the previous script line
ending in a \",\" character, ignoring any trailing white-space or
comments."
  (save-excursion
    (beginning-of-line)
    (and (not (kixtart--in-comment-or-string-p))
         (progn
           (forward-comment (- (point)))
           (eq (char-before) ?,)))))

(defun kixtart--follows-eol-multiline-indicator-p ()
  "Return a non-nil value when following a multi-line indicator.

Being within a multi-line expression is indicated by the previous
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

(defun kixtart--in-comment-line-p (&optional ppss)
  "Return a non-nil value when inside a single comment line.

Prefer existing parser state PPSS over calling `syntax-ppss'."
  (null (nth 7 (or ppss (syntax-ppss)))))

(defun kixtart--start-of-comment-or-string (&optional ppss)
  "Return the starting position of the comment or string at point.

Return nil when point is outside of a comment or string.  Prefer
existing parser state PPSS over calling `syntax-ppss'."
  (nth 8 (or ppss (syntax-ppss))))

(defalias 'kixtart--in-comment-or-string-p #'kixtart--start-of-comment-or-string
  "Return a non-nil value when inside a comment or string.")

(defun kixtart--string-terminator (&optional ppss)
  "Return the termination character for the string at point.

Return nil when point is outside of a string.  Prefer existing
parser state PPSS over calling `syntax-ppss'."
  (nth 3 (or ppss (syntax-ppss))))

(defalias 'kixtart--in-string-p #'kixtart--string-terminator
  "Return a non-nil value when inside a string.")

(defun kixtart--thing-at-point (thing &optional no-properties)
  "Return the THING at point.

Consider the ? character to be self-delimiting.  When
NO-PROPERTIES is non-nil, strip text properties from the return
value."
  ;; Always try both branches because the syntax classification of ? is updated
  ;; dynamically based on symbol boundaries.
  (or (let ((start (or (and (eq (char-after) ??) (point))
                       (and (eq (char-before) ??) (1- (point))))))
        ;; The `with-restriction' macro isn't available until Emacs 29.
        (and start (save-restriction
                     (narrow-to-region start (1+ start))
                     (thing-at-point thing no-properties))))
      (thing-at-point thing no-properties)))

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
  (in-parens nil :type boolean)
  (token nil :type symbol)
  (token-string nil :type string)
  (position nil :type (natnum 0 *)))

(defun kixtart--parse-block ()
  "Scan backwards and return the current block state."
  (save-excursion
    ;; Move out of strings and comments.
    (when-let* ((start (kixtart--start-of-comment-or-string)))
      (goto-char start))
    ;; Search backwards matching pairs of script-block defining keyword tokens.
    (let ((case-fold-search t)
          (parse-sexp-ignore-comments t)
          block-end
          block-start
          token-string)
      (condition-case nil
          (while (not (or block-start (bobp)))
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

;;;; Expression parser

(defun kixtart--parse-declared-variables ()
  "Return an alist of variables names and their buffer positions.

Assume that point is on the first character of a command which
declares variables."
  (save-excursion
    (forward-sexp)
    (let ((case-fold-search t)
          vars)
      (condition-case nil
          (while (and (progn
                        (forward-comment (point-max))
                        (looking-at (kixtart-rx variable)))
                      (progn
                        ;; Move over this variable.
                        (goto-char (match-end 0))
                        ;; Move over an array specification.  This does not
                        ;; check that the specification is actually valid.
                        (when (eq (char-after) ?\[)
                          (forward-sexp))
                        ;; Store the variable if there was no was no scan error
                        ;; from trying to move across the [ ] pair of an array
                        ;; specification.
                        (push (cons (match-string-no-properties 0)
                                    (match-beginning 0))
                              vars)
                        (forward-comment (point-max))
                        ;; Match a comma, which indicates that there are
                        ;; additional variables being defined.
                        (and (eq (char-after) ?,)
                             (progn (forward-char) t)))))
        (scan-error))
      (nreverse vars))))

;;;; Motion

(defcustom kixtart-block-motion-push-mark t
  "Specifies whether block motion will push to the `mark-ring'.

A non-nil value indicates that block motion commands are
permitted to push the previous location to the `mark-ring' when
the value of point changes."
  :type 'boolean)

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
  (let* ((case-fold-search t)
         (forwards (cl-minusp arg))
         (search-fn (if forwards #'re-search-forward #'re-search-backward))
         (inc-fn (if forwards #'1+ #'1-))
         match-pos)
    (save-excursion
      ;; Ensure that searching forwards doesn't match the current position.
      (when (and forwards (looking-at-p (kixtart-rx command-function)))
        (forward-char 8))
      ;; Search for the argth FUNCTION command in the given direction.
      (while (and (not (zerop arg))
                  (funcall search-fn (kixtart-rx command-function) nil t)
                  (or (kixtart--in-comment-or-string-p)
                      (setq arg (funcall inc-fn arg) match-pos (point))))))
    (and match-pos (goto-char
                    ;; Ensure point is at the beginning of the match.
                    (if forwards (- match-pos 8) match-pos)))))

(defun kixtart-end-of-defun ()
  "Move forwards to the end of a function definition."
  (let ((case-fold-search t)
        match-pos)
    (save-excursion
      (while (and (re-search-forward (kixtart-rx command-endfunction) nil t)
                  (or (kixtart--in-comment-or-string-p)
                      (not (setq match-pos (point)))))))
    (and match-pos (goto-char match-pos))))

(defun kixtart-up-script-block ()
  "Move point to the opening of the current script-block.

Unless prevented by the value of `kixtart-block-motion-push-mark'
the previous location is pushed to the `mark-ring' when the value
of point is modified."
  (interactive)
  (let ((from (point)))
    (goto-char (kixtart-block-position (kixtart--parse-block)))
    (unless (or (null kixtart-block-motion-push-mark)
                (region-active-p)
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

(defcustom kixtart-indent-offset 4
  "Specifies the indentation offset applied by `kixtart-indent-line'.

Lines determined to be within script-blocks are indented by this
number of columns per script-block level."
  :type 'integer)

(defun kixtart--new-indent ()
  "Return the calculated indentation level for the current line."
  (save-excursion
    (back-to-indentation)
    (let ((ppss (syntax-ppss)))
      (if (kixtart--in-comment-or-string-p ppss)
          (current-column)
        (let* ((case-fold-search t)
               (multiline-indicator (kixtart--follows-eol-multiline-indicator-p))
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
                  ;; preceding the first "CASE" block to align with the
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

;;;; Align

(defcustom kixtart-align-rules-list
  `((kixtart-assignment
     (regexp     . ,(kixtart-rx (seq variable
                                     (group (0+ whitespace))
                                     "="
                                     (group (0+ whitespace)))))
     (group     . (1 2))
     (case-fold . t)
     (tab-stop  . nil)))
  "Specifies the list of available alignment rules.

See the variable `align-rules-list' for details on the list
and rule formats."
  :type align-rules-list-type
  :risky t)

(add-to-list 'align-dq-string-modes 'kixtart-mode)
(add-to-list 'align-open-comment-modes 'kixtart-mode)
(add-to-list 'align-sq-string-modes 'kixtart-mode)

;;;; Electric layout

(defun kixtart--electric-layout-eol ()
  "Return how to insert a newline using `electric-layout-mode'.

It is assumed that this function will be called immediately after
the \"\\\" character has been typed by the user, with the newline
being inserted if the end of the line now appears to be the
special comment which indicates a multiline expression."
  (and (looking-back (kixtart-rx multiline-indicator) (- (point) 2))
       (let ((ppss (syntax-ppss)))
         (and (not (kixtart--in-string-p ppss))
              (kixtart--in-comment-line-p ppss)
              'after))))

;;;; Code evaluation

(defcustom kixtart-eval-buffer-name "*KiXtart Output*"
  "Specifies the buffer name used for interpreter output."
  :type 'string)

(defcustom kixtart-eval-extra-args nil
  "Specifies additional interpreter arguments.

The value should be a list of strings which will be used as
additional arguments for the KiXtart interpreter."
  :type '(repeat string))

(defcustom kixtart-eval-header "$ = SetOption(\"ASCII\", \"ON\")\n"
  "Specifies a string which is included in evaluated code.

The string value is inserted at the beginning of any scripts
which are sent to the KiXtart interpreter.  Note that the script
which is being executed will need to set the \"ASCII\" option to
\"ON\" to allow the process output to be read."
  :type 'string)

(defcustom kixtart-eval-hook (list #'kixtart-scroll-buffer-windows)
  "A hook which is called during code evaluation.

The hook functions will be called with the buffer used for
displaying interpreter output as the current buffer, before the
interpreter process is started."
  :type 'hook)

(defcustom kixtart-program "kix32"
  "Specifies the name of the KiXtart executable."
  :type 'string)

(define-derived-mode kixtart-eval-mode special-mode "KiXtart-Eval"
  "Major mode used for KiXtart interpreter output."
  ;; Configure the mode line to show the process state.
  (setq mode-line-process '(":%s"))
  ;; Move `window-point' forwards when inserting at its position.
  (setq-local window-point-insertion-type t))

(defvar kixtart-eval-history nil
  "History of minibuffer input for `kixtart-eval'.")

(defun kixtart-eval (string)
  "Evaluate STRING using the KiXtart interpreter."
  (interactive
   (list (read-from-minibuffer "KiXtart: " nil nil nil 'kixtart-eval-history)))
  (let ((script-file (make-temp-file "kixtart-mode" nil ".kix"
                                     (concat kixtart-eval-header string)))
        (buffer (or (get-buffer kixtart-eval-buffer-name)
                    (with-current-buffer (generate-new-buffer
                                          kixtart-eval-buffer-name)
                      (kixtart-eval-mode)
                      (current-buffer)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (run-hooks 'kixtart-eval-hook)))
    (display-buffer
     (process-buffer
      (make-process :name (file-name-base kixtart-program)
                    :buffer buffer
                    :command `(,kixtart-program
                               ,@kixtart-eval-extra-args
                               ,script-file)
                    :sentinel #'ignore)))))

(defun kixtart-eval-buffer ()
  "Evaluate the buffer using the KiXtart interpreter."
  (interactive)
  (kixtart-eval (buffer-substring-no-properties (point-min) (point-max)))
  (message "Evaluated the %s buffer" (buffer-name)))

(defun kixtart-eval-region ()
  "Evaluate the active region using the KiXtart interpreter."
  (interactive)
  (cond ((use-region-p)
         (kixtart-eval
          (buffer-substring-no-properties (region-beginning) (region-end)))
         (message "Evaluated the region in the %s buffer" (buffer-name)))
        (t
         (user-error "No active region"))))

(defun kixtart-eval-region-or-buffer ()
  "Evaluate a portion of the buffer using the KiXtart interpreter.

When a region is active, evaluation that region, otherwise
evaluate the entire buffer."
  (interactive)
  (if (use-region-p)
      (kixtart-eval-region)
    (kixtart-eval-buffer)))

(defun kixtart-scroll-buffer-windows ()
  "Scroll all windows which are displaying the current buffer."
  (let ((buffer (current-buffer)))
    (walk-windows (lambda (window)
                    (when (eq (window-buffer window) buffer)
                      (select-window window)
                      (goto-char (point-max))))
                  nil t)))

;;;; Compilation

(defvar kixtart-complilation-error
  (rx bol
      "Script" (0+ ? ) ?: (0+ ? ) (group (1+ not-newline))
      "\n"
      "Line" (0+ ? ) ?: (0+ ? ) (group (char (?1 . ?9)) (0+ digit))
      eol)
  "The regexp pattern to match a KiXtart script error.")

(defvar kixtart-complilation-error-tokenized
  (rx bol
      "Script" (0+ ? ) ?: (0+ ? ) (group (+? not-newline))
      ?. (or ?K ?k) (or ?X ?x)
      "\n"
      "Line" (0+ ? ) ?: (0+ ? ) (group (char (?1 . ?9)) (0+ digit))
      eol)
  "The regexp pattern to match a tokenized KiXtart script error.")

;; Match the entire filename and use it directly.
(add-to-list 'compilation-error-regexp-alist-alist
             (list 'kixtart kixtart-complilation-error 1 2))
(add-to-list 'compilation-error-regexp-alist 'kixtart)
;; Match a .KX filename with the file extension removed and try to guess what
;; the original source file extension was.
(add-to-list 'compilation-error-regexp-alist-alist
             (list 'kixtart-tokenized kixtart-complilation-error-tokenized
                   '(1 "%s.kix" "%s.KIX" "%s.Kix") 2))
(add-to-list 'compilation-error-regexp-alist 'kixtart-tokenized)

;;;; Block closing

(defvar-local kixtart--close-command-strings nil
  "The list of strings which can close the currently open block.")

(defun kixtart--syntax-case-function (token)
  "Return the case function which best matches the string TOKEN."
  (cond ((string= (upcase token) token) #'upcase)
        ((string= (downcase token) token) #'downcase)
        (t #'identity)))

(defun kixtart-close-command-block ()
  "Insert the command to close the currently open block."
  (interactive)
  (let ((tick (buffer-chars-modified-tick)))
    (cond ((and kixtart--close-command-strings
                (eq last-command #'kixtart-close-command-block))
           (delete-char (- (length (pop kixtart--close-command-strings)))))
          ((setq kixtart--close-command-strings
                 (let ((block (kixtart--parse-block)))
                   (and-let* ((token-string (kixtart-block-token-string block)))
                     (mapcar (kixtart--syntax-case-function token-string)
                             (pcase (kixtart-block-token block)
                               ('kixtart-case-t     '("Case" "EndSelect"))
                               ('kixtart-do-t       '("Until"))
                               ('kixtart-else-t     '("EndIf"))
                               ('kixtart-for-t      '("Next"))
                               ('kixtart-function-t '("EndFunction"))
                               ('kixtart-if-t       '("Else" "EndIf"))
                               ('kixtart-select-t   '("Case"))
                               ('kixtart-while-t    '("Loop"))))))))
          (t (user-error "No open command block to close")))
    (when-let* ((close-command (car kixtart--close-command-strings)))
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

;;;; Current function

(defun kixtart--current-defun ()
  "Internal implementation of `kixtart-current-defun'.

Return the function name which surrounds point.  When point is
not within a function return nil.  It is assumed that this
function is called with buffer restrictions removed."
  (save-excursion
    (let ((case-fold-search t)
          (from (point))
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

;;;; Which function

(defcustom kixtart-which-function-default-name nil
  "Specifies the default function name for `which-function-mode'.

This value is provided to `which-function-mode' as the current
function name when point is outside of a function.  When the
value is set to nil, the name used when point is outside of a
function will only be determined by `which-function-mode'."
  :type '(choice (string :tag "Default function name")
                 (const :tag "Let `which-function-mode' decide" nil)))

(defun kixtart-which-function ()
  "Return the function name which surrounds point.

When point is not within a function return the value of
`kixtart-which-function-default-name'."
  (or (kixtart-current-defun) kixtart-which-function-default-name))

;;;; ElDoc

(defcustom kixtart-doc-search-functions
  (list #'kixtart-doc-search-at-point
        #'kixtart-doc-search-before-point
        #'kixtart-doc-search-in-function-args
        #'kixtart-doc-search-command-line)
  "Abnormal hook for functions which return documentation.

Each function is called in sequence until one returns a non-nil
value, which should be a cons cell containing the upper-case form
of the symbol which was matched and a list of documentation
structures compatible with the generic method
`kixtart-doc-string'."
  :type 'hook)

(defcustom kixtart-doc-separator "\n"
  "Specifies the separator for display of documentation matches.

Where a documentation search returns multiple matches, the
results will be displayed using this string as the separator."
  :type 'string)

(defcustom kixtart-eldoc-echo-truncate t
  "Specifies how `eldoc-mode' will use the echo area.

A postive integer value indicates that text should be truncated
after the given number of characters.  Any other non-nil value
indicates that text should be truncated at the first paragraph.

Note that ElDoc version 1.14.0 or newer is required for this
option to work correctly."
  :type '(choice (const :tag "Never" nil)
                 (natnum :tag "After number of characters")
                 (const :tag "At end of paragraph" t)))

(cl-defstruct (kixtart-doc-symbol
               (:constructor nil)
               (:copier nil))
  (symbols nil :type list)
  (final nil :type (or boolean list symbol)))

(cl-defstruct (kixtart-doc-syntax
               (:include kixtart-doc-symbol)
               (:constructor nil)
               (:copier nil))
  (syntax nil :type string))

(cl-defstruct (kixtart-doc-command
               (:include kixtart-doc-syntax)
               (:constructor kixtart-make-doc-command)
               (:copier nil)))

(cl-defstruct (kixtart-doc-function
               (:include kixtart-doc-syntax)
               (:constructor kixtart-make-doc-function)
               (:copier nil)))

(cl-defstruct (kixtart-doc-macro
               (:include kixtart-doc-symbol)
               (:constructor kixtart-make-doc-macro)
               (:copier nil))
  (description nil :type string)
  (type nil :type (natnum 0 *)))

(cl-defgeneric kixtart-doc-accepts-argument-p (doc symbol)
  "Return non-nil when DOC reference for SYMBOL expects arguments.")

(cl-defmethod kixtart-doc-accepts-argument-p ((doc kixtart-doc-symbol) symbol)
  "Return non-nil when DOC reference for SYMBOL expects arguments.

DOC is a `kixtart-doc-symbol' structure."
  (not (pcase (kixtart-doc-symbol-final doc)
         ('last (eq (car (last (kixtart-doc-symbol-symbols doc))) symbol))
         ('first (eq (car (kixtart-doc-symbol-symbols doc)) symbol))
         ((and (pred listp) final-list) (memq symbol final-list))
         ((and (pred booleanp) final) final)
         (_ (error "Unknown final")))))

(cl-defgeneric kixtart-doc-face (doc)
  "Return the face used to present DOC.")

(cl-defmethod kixtart-doc-face ((doc kixtart-doc-command))
  "Return the face used to present DOC.

DOC is a `kixtart-doc-command' structure."
  (ignore doc)
  'kixtart-command-face)

(cl-defmethod kixtart-doc-face ((doc kixtart-doc-function))
  "Return the face used to present DOC.

DOC is a `kixtart-doc-function' structure."
  (ignore doc)
  'kixtart-function-face)

(cl-defmethod kixtart-doc-face ((doc kixtart-doc-macro))
  "Return the face used to present DOC.

DOC is a `kixtart-doc-macro' structure."
  (ignore doc)
  'kixtart-macro-face)

(defun kixtart-doc-type-name (id)
  "Return the integer type ID as a string."
  (pcase id
    ((pred (<= 8192))
     (concat (kixtart-doc-type-name (- id 8192)) "[]"))
    (0  "Empty")
    (1  "Null")
    (2  "Integer")
    (3  "Long")
    (4  "Single")
    (5  "Double")
    (6  "Currency")
    (7  "Date")
    (8  "String")
    (9  "Object")
    (10 "Error")
    (11 "Boolean")
    (12 "Variant")
    (13 "Object")
    (17 "Byte")
    (_  (error "Unknown type"))))

(cl-defgeneric kixtart-doc-string (doc)
  "Return DOC as a string.")

(cl-defmethod kixtart-doc-string ((doc kixtart-doc-macro))
  "Return DOC as a string.

DOC is a `kixtart-doc-macro' structure."
  (let ((description (kixtart-doc-macro-description doc))
        (type (kixtart-doc-macro-type doc)))
    (if type
        (format "(%s) %s" (kixtart-doc-type-name type) description)
      description)))

(cl-defmethod kixtart-doc-string ((doc kixtart-doc-syntax))
  "Return DOC as a string.

DOC is a `kixtart-doc-syntax' structure."
  (kixtart-doc-syntax-syntax doc))

(defmacro kixtart-doc-register (constructor &rest specs)
  "Register documentation SPECS using CONSTRUCTOR.

CONSTRUCTOR should be a function which returns a documentation
structure.  Each form in SPECS should be a list, where the first
element is a symbol or list of symbols to be passed to
CONSTRUCTOR as keyword argument `:symbols'.  All other elements
in SPECS are passed as additional arguments to CONSTRUCTOR."
  (declare (indent 1))
  `(progn
     ,@(mapcar
        (pcase-lambda (`(,symbols . ,rest))
          `(cl-pushnew
            (funcall #',constructor :symbols ',(ensure-list symbols) ,@rest)
            kixtart-doc-list
            :test 'equal))
        specs)))

(defvar kixtart-doc-list nil
  "The list of documentation structures available to search.")

(defun kixtart-doc-search-at-point (&optional predicate)
  "Match documentation structures using PREDICATE.

If any matches are found, return a cons cell containing the
upper-case form of the symbol at point and a list of matching
documentation structures.  The structures are only matched if
they contain the symbol in their symbols slot and PREDICATE
returns a non-nil value when called with the structure and symbol
as its arguments."
  (unless predicate (setq predicate #'always))
  (and-let* ((thing (kixtart--thing-at-point 'symbol))
             (symbol (and (stringp thing) (intern (upcase thing))))
             (docs (cl-loop for doc in kixtart-doc-list
                            when (memq symbol (kixtart-doc-symbol-symbols doc))
                            when (funcall predicate doc symbol)
                            collect doc)))
    (cons symbol docs)))

(defun kixtart-doc-search-before-point ()
  "Match documentation structures in positions before point.

This only considers functions and commands which take arguments."
  (save-excursion
    ;; Move backwards through white-space.
    (forward-comment (- (point)))
    (kixtart-doc-search-at-point
     ;; Results must accept arguments.
     #'kixtart-doc-accepts-argument-p)))

(defun kixtart-doc-search-in-function-args ()
  "Match documentation structures for functions.

This only considers functions which take arguments, and only
when point appears to be within parentheses."
  (save-excursion
    ;; Move out of parens if this looks like function arguments.
    (and (cl-plusp (kixtart--paren-depth))
         (progn
           (backward-up-list nil t t)
           ;; Move backwards through white-space.
           (forward-comment (- (point)))
           (kixtart-doc-search-at-point
            ;; Results must be functions that accept arguments.
            (lambda (doc symbol)
              (and (kixtart-doc-function-p doc)
                   (kixtart-doc-accepts-argument-p doc symbol))))))))

(defun kixtart-doc-search-command-line ()
  "Match documentation structures at the beginning of the line.

This only consider commands which take arguments."
  (save-excursion
    ;; Move to indentation.
    (and (>= (point) (progn (back-to-indentation) (point)))
         (kixtart-doc-search-at-point
          ;; Results must be commands that accept arguments.  Ignore results for
          ;; FOR since the result will be ambiguous.
          (lambda (doc symbol)
            (and (not (eq symbol 'FOR))
                 (kixtart-doc-command-p doc)
                 (kixtart-doc-accepts-argument-p doc symbol)))))))

(defun kixtart-doc-search ()
  "Search for documentation structures.

Matches are returned as a cons cell, where the first element is
the upper-case form of the symbol which was matched, and the
second element is the first non-nil result of calling the
functions listed in the abnormal hook
`kixtart-doc-search-functions', which should be a list of
documentation structures."
  (save-excursion
    ;; Move out of comments and strings.
    (when-let* ((start (kixtart--start-of-comment-or-string)))
      (goto-char start))
    (run-hook-with-args-until-success 'kixtart-doc-search-functions)))

(defun kixtart-eldoc-function (callback)
  "Call CALLBACK with a docstring relevant for point."
  (pcase (kixtart-doc-search)
    (`(,thing . ,docs)
     (let ((docstring (mapconcat #'kixtart-doc-string
                                 docs
                                 kixtart-doc-separator)))
       (funcall callback docstring
                :thing (symbol-name thing)
                :face (kixtart-doc-face (car docs))
                :echo (or (and (natnump kixtart-eldoc-echo-truncate)
                               kixtart-eldoc-echo-truncate)
                          (and kixtart-eldoc-echo-truncate
                               (string-search "\n\n" docstring))))))))

;;;; Completion

(defcustom kixtart-completion-annotation-function
  #'kixtart-completion-annotate-macros
  "Specifies the function which creates completion annotations."
  :type 'function)

(defcustom kixtart-completion-case-fold nil
  "Specifies that KiXtart completion should be case-insensitive."
  :type 'boolean)

(defcustom kixtart-completion-list-hook
  (list #'kixtart-completion-upcase-macros
        #'kixtart-completion-add-crlf-commands)
  "A hook which is called during symbol completion.

At the time that the hook functions are called, the value of
`kixtart-completion-list' contains the current list of completion
keywords, and `kixtart-completion-input' contains a copy of the
input string which is being completed.  Hook functions are free
to modify both values."
  :type 'hook)

(defvar kixtart-completion-list nil
  "The list of completion keywords to offer.")

(defvar kixtart-completion-input nil
  "The input string which is being completed.")

(defun kixtart-completion-add-crlf-commands ()
  "Modify the completion list to match CRLF output commands.

When the completion input string is a sequence of 2 or more \"?\"
characters, add it to the front of the completion list to ensure
there is a single match."
  (when (string-match-p (rx (>= 2 ??)) kixtart-completion-input)
    (push kixtart-completion-input kixtart-completion-list)))

(defun kixtart-completion-include-tags ()
  "Append the current TAGS table to the completion list.

Note that this function is not responsible for visiting or
updating the TAGS file."
  (when (or tags-file-name tags-table-list)
    (setq kixtart-completion-list
          (nconc kixtart-completion-list (tags-completion-table)))))

(defun kixtart-completion-upcase-macros ()
  "Convert macro names in the completion list to uppercase."
  (setq kixtart-completion-list
        (cl-loop for k in kixtart-completion-list
                 collect (if (string-prefix-p "@" k) (upcase k) k))))

(defun kixtart-completion-annotate-macros (string)
  "Return the annotation for STRING when it is a macro name."
  (and (string-prefix-p "@" string)
       (let ((symbol (intern (upcase string))))
         (cl-loop for doc in kixtart-doc-list
                  when (kixtart-doc-macro-p doc)
                  when (memq symbol (kixtart-doc-macro-symbols doc))
                  return (concat " " (kixtart-doc-macro-description doc))))))

(defun kixtart-completion-at-point-function ()
  "Complete the symbol at point."
  (and (not (kixtart--in-comment-or-string-p))
       (pcase (bounds-of-thing-at-point 'symbol)
         (`(,beg . ,end)
          (list beg end
                (completion-table-case-fold
                 (completion-table-dynamic
                  (lambda (string)
                    (let ((kixtart-completion-list kixtart-keywords)
                          (kixtart-completion-input string))
                      (run-hooks 'kixtart-completion-list-hook)
                      kixtart-completion-list)))
                 (not kixtart-completion-case-fold))
                :annotation-function kixtart-completion-annotation-function)))))

;;;; Hideshow

(defun kixtart--hs-forward-sexp (&optional arg)
  "Move forwards across a balanced expression ARG times.

Command defined blocks are considered to be balanced expressions.
Using a negative argument to move backwards is not supported."
  (if arg
      (when (cl-minusp arg)
        (error "Negative argument is not supported"))
    (setq arg 1))
  (while (cl-plusp arg)
    (if (let ((case-fold-search t))
          (forward-comment (point-max))
          (looking-at (kixtart-rx script-block-open)))
        (let ((case-fold-search t)
              (parse-sexp-ignore-comments t)
              (block-start (list (kixtart--match-string-as-token)))
              close-pos)
          (save-excursion
            (while (not (or close-pos
                            (progn
                              ;; Check that there is something beyond the
                              ;; current symbol.
                              (forward-sexp 1)
                              (forward-comment (point-max))
                              (eobp))))
              (forward-sexp 1)   ; Move forwards beyond the next symbol.
              (forward-sexp -1)  ; Move backwards to the start of the symbol.
              (cond ((looking-at (kixtart-rx script-block-close))
                     (pcase (cons (car block-start)
                                  (kixtart--match-string-as-token))
                       ((or '(kixtart-do-t       . kixtart-until-t)
                            '(kixtart-for-t      . kixtart-next-t)
                            '(kixtart-function-t . kixtart-endfunction-t)
                            `(,(or 'kixtart-if-t 'kixtart-else-t)
                              . kixtart-endif-t)
                            '(kixtart-select-t   . kixtart-endselect-t)
                            `(kixtart-case-t
                              . ,(or 'kixtart-case-t 'kixtart-endselect-t))
                            '(kixtart-while-t    . kixtart-loop-t))
                        (pop block-start)
                        (unless block-start
                          (forward-sexp)  ; Move to the end of the symbol.
                          (setq close-pos (point))))))
                    ((looking-at (kixtart-rx script-block-open))
                     (push (kixtart--match-string-as-token) block-start)))))
          (if close-pos
              (goto-char close-pos)
            (user-error "No next sexp")))
      (forward-sexp 1))
    (cl-decf arg)))

(add-to-list 'hs-special-modes-alist
             `(kixtart-mode ,(kixtart-rx (or ?\( ?\[ script-block-open))
                            ,(kixtart-rx (or ?\) ?\] script-block-close))
                            ,(rx (or ?\; "/*"))
                            kixtart--hs-forward-sexp))

;;;; Imenu

(defcustom kixtart-imenu-submenu-prefix "/"
  "Specifies the string prefix used for Imenu submenu names."
  :type 'string)

(defun kixtart--convert-imenu-item-to-markers (item)
  "Modify ITEM to use markers instead of buffer positions."
  (let ((data (cdr item)))
    (setcdr item
            (if (listp data)
                (mapc #'kixtart--convert-imenu-item-to-markers data)
              (copy-marker data t)))))

(defun kixtart--create-imenu-index ()
  "Build and return an index alist suitable for Imenu.

Functions are added at the top level of the menu.  Labels are
added into a submenu."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (let ((case-fold-search t)
            globals index labels)
        (while (re-search-backward
                (kixtart-rx (or command-function command-global label)) nil t)
          (cond ((kixtart--in-comment-or-string-p))
                ((eq (char-after) ?:)
                 ;; Add label names.
                 (push (cons (substring (match-string-no-properties 0) 1)
                             (point))
                       labels))
                ((looking-at-p (kixtart-rx command-global))
                 ;; Add global variable declarations.
                 (when-let* ((vars (kixtart--parse-declared-variables)))
                   (mapc (lambda (item) (setcdr item (point))) vars)
                   (setq globals (nconc globals vars))))
                (t
                 ;; Add function names.
                 (when-let* ((name (kixtart--current-defun)))
                   (push (cons name (point)) index)))))
        (when globals
          (push (cons (concat kixtart-imenu-submenu-prefix "Globals") globals)
                index))
        (when labels
          (push (cons (concat kixtart-imenu-submenu-prefix "Labels") labels)
                index))
        (when imenu-use-markers
          (mapc #'kixtart--convert-imenu-item-to-markers index))
        index))))

;;;; Outline mode

(defun kixtart-outline-level ()
  "Return the depth for the current outline heading."
  (if (eq (char-after) ?\;)
      (save-excursion
        (forward-same-syntax)
        (- (current-column) 2))
    most-positive-fixnum))

(defun kixtart-outline-search (&optional bound move backward looking-at)
  "Search for the next outline heading.

For BOUND, MOVE, BACKWARD, and LOOKING-AT, see the descriptions
in `outline-search-function'."
  (let ((case-fold-search t))
    (if looking-at
        (looking-at (kixtart-rx outline))
      (let ((search-fn (if backward #'re-search-backward #'re-search-forward))
            found)
        (save-excursion
          (while (and
                  (funcall search-fn (kixtart-rx outline) bound t nil)
                  (not (setq
                        found
                        (and
                         (save-excursion
                           (goto-char (match-beginning 0))
                           (let ((ppss (syntax-ppss)))
                             (and
                              ;; Avoid matching anything inside a string.
                              (not (kixtart--in-string-p ppss))
                              (if (eq (char-after) ?\;)
                                  ;; Avoid matching inside block comments.
                                  (kixtart--in-comment-line-p ppss)
                                ;; Avoid matching FUNCTION within a comment.
                                (not (kixtart--in-comment-or-string-p ppss))))))
                         (point)))))))
        (when move
          (goto-char (or found bound (if backward (point-min) (point-max)))))
        found))))

;;;; Templates

(defcustom kixtart-abbrev-table-enabled nil
  "Specifies whether KiXtart abbrev expansion is enabled.

A non-nil value indicates that `kixtart-mode-abbrev-table' should
be used as part of abbrev expansion."
  :type 'boolean)

(defcustom kixtart-template-insert-newline #'eobp
  "Specifies whether a template includes a final newline."
  :type `(choice (const :tag "Never" nil)
                 (const :tag "Always" t)
                 (const :tag "At end of buffer" ,#'eobp)
                 (function :tag "Custom function")))

(defun kixtart--tempo-newline-eob ()
  "Insert a newline if point is at end of the buffer."
  (when (pcase kixtart-template-insert-newline
          ((and (pred functionp) func) (funcall func))
          (do-insert do-insert))
    (insert "\n")))

(defun kixtart--tempo-insert-lookup (name template)
  "Optionally insert prompt data for NAME using TEMPLATE.

The template is only returned if the data lookup for NAME does
not return the empty string."
  (and (not (string= "" (tempo-lookup-named name)))
       (cons 'l template)))

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
   (kixtart--tempo-insert-lookup 'args '("(" (s args) ")" )) > n
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
   (kixtart--tempo-insert-lookup 'step '(" Step " (s step))) > n
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

;;;; Keymaps

(defvar kixtart-mode-template-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-b") #'tempo-backward-mark)
    (define-key map (kbd "C-f") #'tempo-forward-mark)
    (define-key map (kbd "C-t") #'tempo-complete-tag)
    (define-key map (kbd "I") #'kixtart-template-ifelse)
    (define-key map (kbd "c") #'kixtart-template-case)
    (define-key map (kbd "d") #'kixtart-template-do)
    (define-key map (kbd "e") #'kixtart-template-foreach)
    (define-key map (kbd "f") #'kixtart-template-for)
    (define-key map (kbd "i") #'kixtart-template-if)
    (define-key map (kbd "l") #'kixtart-template-else)
    (define-key map (kbd "s") #'kixtart-template-select)
    (define-key map (kbd "u") #'kixtart-template-function)
    (define-key map (kbd "w") #'kixtart-template-while)
    map)
  "Keymap for `kixtart-mode' templates.")

(defvar kixtart-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'kixtart-close-command-block)
    (define-key map (kbd "C-c C-d") #'eldoc)
    (define-key map (kbd "C-c C-e") #'kixtart-eval-region-or-buffer)
    (define-key map (kbd "C-c C-j") #'imenu)
    (define-key map (kbd "C-c C-t") kixtart-mode-template-map)
    (define-key map (kbd "C-c C-u") #'kixtart-up-script-block)
    map))

;;;; Menu

(easy-menu-define kixtart-mode-menu kixtart-mode-map
  "Menu for KiXtart Mode."
  `("KiXtart"
    ["Start of function" beginning-of-defun
     :help "Go to the start of the function around point"]
    ["End of function" end-of-defun
     :help "Go to the end of the function around point"]
    ["Mark function" mark-defun
     :help "Mark the function definition around point"]
    "---"
    ["Close command block" kixtart-close-command-block
     :help "Close the command block open around point"]
    ["Jump to place" imenu
     :help "Jump to a place of significance in the buffer"]
    ["Jump to block opening" kixtart-up-script-block
     :help "Jump to the opening of the current script block"]
    "---"
    ,(cons
      "Templates"
      (cl-loop for (tempo-tag . tempo-function) in kixtart-tempo-tags
               collect (vector
                        (concat "Insert " tempo-tag)
                        tempo-function
                        :help (format "Insert the template for Tempo tag \"%s\""
                                      tempo-tag))))
    "---"
    ["Eval buffer" kixtart-eval-buffer
     :help "Evaluate the buffer contents using the KiXtart interpreter"]
    ["Eval region" kixtart-eval-region
     :active (use-region-p)
     :help "Evaluate the current region using the KiXtart interpreter"]
    ["Eval string" kixtart-eval
     :help "Evaluate a string using the KiXtart interpreter"]))

;;;; Syntax table

(defconst kixtart-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Add ' for string-quotes.
    (modify-syntax-entry ?' "\"" table)
    ;; Set line comment start and end.
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Set block comment open and close.  Nesting is not supported.
    (modify-syntax-entry ?/ ". 14b" table)
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
    (modify-syntax-entry ?? "_" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?\\ "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?` "_" table)
    (modify-syntax-entry ?{ "_" table)
    (modify-syntax-entry ?} "_" table)
    table))

(defconst kixtart-syntax-propertize-function
  (syntax-propertize-rules
   ;; Since the rules are applied in a single pass, begin by matching ? against
   ;; its own symbol boundary.  This effectively sets the logic of the next rule
   ;; to be a match where the symbol boundary is extended by adjacent symbol
   ;; characters.
   ((rx symbol-start (1+ ??) symbol-end))
   ;; Convert matches to punctuation so that sexp motion skips through ?
   ;; characters and finds usable symbol boundaries for KiXtart keywords.
   ((rx (1+ ??))
    (0 "."))))

;;;; Mode

;;;###autoload
(define-derived-mode kixtart-mode prog-mode "KiXtart"
  "Major mode for editing KiXtart scripts."
  (setq-local comment-start ";")
  (setq-local font-lock-defaults '((kixtart-font-lock-keywords
                                    kixtart-font-lock-keywords-1
                                    kixtart-font-lock-keywords-2
                                    kixtart-font-lock-keywords-3)
                                   nil t))
  (setq-local beginning-of-defun-function #'kixtart-beginning-of-defun)
  (setq-local end-of-defun-function #'kixtart-end-of-defun)
  (setq-local indent-line-function #'kixtart-indent-line)
  (setq-local add-log-current-defun-function #'kixtart-current-defun)
  (setq-local outline-level #'kixtart-outline-level)
  (setq-local outline-search-function #'kixtart-outline-search)
  (setq-local syntax-propertize-function kixtart-syntax-propertize-function)
  (setq align-mode-rules-list kixtart-align-rules-list)
  (setq imenu-create-index-function #'kixtart--create-imenu-index)
  (tempo-use-tag-list 'kixtart-tempo-tags)
  (add-hook 'completion-at-point-functions
            #'kixtart-completion-at-point-function nil t)
  (add-hook 'which-func-functions #'kixtart-which-function nil t)
  (add-to-list 'font-lock-extend-region-functions
               #'kixtart--font-lock-extend-region-function-def t)
  (when (boundp 'electric-layout-allow-in-comment-or-string)
    (setq-local electric-layout-allow-in-comment-or-string t)
    (setq-local electric-layout-rules `((?\\
                                         . ,#'kixtart--electric-layout-eol))))
  (when kixtart-doc-list
    (add-hook 'eldoc-documentation-functions #'kixtart-eldoc-function nil t)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kix\\'" . kixtart-mode))

(provide 'kixtart-mode)
;;; kixtart-mode.el ends here
