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

;;; Commentary:

;; Support for editing KiXtart script files.

;; See the manual for configuration examples and instructions.

;;; Code:

;; `cl-lib' is required at runtime to use `cl-search'.  Since `imenu' also
;; requires `cl-lib' there is actually no change in runtime requirements.
(require 'cl-lib)
(require 'imenu)
(require 'tempo)
(eval-when-compile
  (require 'rx))

;;;; Customization

(defgroup kixtart nil
  "Major mode for editing KiXtart files."
  :tag "KiXtart"
  :link '(custom-manual "(kixtart-mode)Top")
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

(defcustom kixtart-completion-annotation-function
  #'kixtart-completion-annotate-macros
  "Specifies the function which creates completion annotations."
  :type 'function)

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

(defcustom kixtart-eldoc-echo-truncate t
  "Specifies how `eldoc-mode' will use the echo area.
An integer value indicates that text should be truncated after
the given number of characters.  Any other non-nil value
indicates that text should be truncated at the first paragraph."
  :type '(choice (const :tag "Never" nil)
                 (integer :tag "After number of characters")
                 (const :tag "At end of paragraph" t)))

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
ending in a \",\" character, ignoring any trailing white-space or
comments."
  (save-excursion
    (beginning-of-line)
    (and (not (kixtart--in-comment-or-string-p))
         (progn
           (forward-comment (- (point)))
           (eq (char-before) ?,)))))

(defun kixtart--follows-eol-multiline-indicator-p ()
  "Return a non-nil value when following a mutli-line indicator.
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

(defun kixtart--start-of-comment-or-string (&optional ppss)
  "Return the starting position of the comment or string at point.
Return nil when point is outside of a comment or string.  Prefer
existing parser state PPSS over calling `syntax-ppss'."
  (nth 8 (or ppss (syntax-ppss))))

(defalias 'kixtart--in-comment-or-string-p #'kixtart--start-of-comment-or-string
  "Return a non-nil value when inside a comment or string.")

(defun kixtart--thing-at-point (thing &optional no-properties)
  "Return the THING at point.
Consider the ? character to be self-delimiting.  When
NO-PROPERTIES is non-nil, strip text properties from the return
value."
  ;; Always try both branches because the syntax classification of ? is updated
  ;; dynamically based on symbol boundaries.
  (or (let ((start (or (and (eq (char-after) ??) (point))
                       (and (eq (char-before) ??) (1- (point))))))
        (and start (with-restriction start (1+ start)
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
  (position nil :type (integer 0 *)))

(defun kixtart--parse-block ()
  "Scan backwards and return the current block state."
  (save-excursion
    ;; Move out of strings and comments.
    (when-let ((start (kixtart--start-of-comment-or-string)))
      (goto-char start))
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

;;;; ElDoc

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
  (type nil :type (integer 0 *)))

(cl-defgeneric kixtart-doc-accepts-argument-p (doc symbol)
  "Return non-nil when DOC reference for SYMBOL expects arguments.")

(cl-defmethod kixtart-doc-accepts-argument-p ((doc kixtart-doc-symbol) symbol)
  "Return non-nil when DOC reference for SYMBOL expects arguments.
DOC is a `kixtart-doc-symbol' structure."
  (not (pcase (kixtart-doc-symbol-final doc)
         ('last (eq (car (last (kixtart-doc-symbol-symbols doc))) symbol))
         ('first (eq (car (kixtart-doc-symbol-symbols doc)) symbol))
         ((and (pred listp) final) (memq symbol final))
         ((and final (guard final)) t))))

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
    (17 "Byte")))

(cl-defgeneric kixtart-doc-string (doc)
  "Return DOC as a string.")

(cl-defmethod kixtart-doc-string ((doc kixtart-doc-macro))
  "Return DOC as a string.
DOC is a `kixtart-doc-macro' structure."
  (let ((description (kixtart-doc-macro-description doc))
        (type (kixtart-doc-type-name (kixtart-doc-macro-type doc))))
    (if type
        (format "(%s) %s" type description)
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
     ,@(mapcar (pcase-lambda (`(,symbols . ,rest))
                 (pcase-exhaustive symbols
                   ((pred listp))
                   ((pred symbolp)
                    (setq symbols (list symbols))))
                 `(push (funcall #',constructor :symbols ',symbols ,@rest)
                        kixtart-doc-docs))
               specs)))

(defvar kixtart-doc-docs nil
  "The list of documentation structures available to search.")

(defun kixtart-doc-search-at-point (&optional predicate)
  "Match documentation structures using PREDICATE.
If any matches are found, return a cons cell containing the
upper-case form of the symbol at point and a list of matching
documentation structures.  The structures are only matched if
they contain the symbol in their symbols slot and PREDICATE
returns a non-nil value when called with the structure and symbol
as its arguments."
  ;; The `always' function isn't available before Emacs 28.
  (unless predicate (setq predicate (lambda (&rest _) t)))
  (and-let* ((thing (kixtart--thing-at-point 'symbol))
             (symbol (and (stringp thing) (intern (upcase thing))))
             (docs (cl-loop for doc in kixtart-doc-docs
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
    (when-let ((start (kixtart--start-of-comment-or-string)))
      (goto-char start))
    (run-hook-with-args-until-success 'kixtart-doc-search-functions)))

(defun kixtart-eldoc-function (callback)
  "Call CALLBACK with a docstring relevant for point."
  (pcase (kixtart-doc-search)
    (`(,thing . ,docs)
     (let ((docstring (mapconcat #'kixtart-doc-string docs "\n")))
       (funcall callback docstring
                :thing (symbol-name thing)
                :face (kixtart-doc-face (car docs))
                :echo (or (and (integerp kixtart-eldoc-echo-truncate)
                               kixtart-eldoc-echo-truncate)
                          (and kixtart-eldoc-echo-truncate
                               (cl-search "\n\n" docstring))))))))

;;;; Completion

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

(defun kixtart-completion-upcase-macros ()
  "Convert macro names in the completion list to uppercase."
  (setq kixtart-completion-list
        (cl-loop for k in kixtart-completion-list
                 collect (if (string-prefix-p "@" k) (upcase k) k))))

(defun kixtart-completion-annotate-macros (string)
  "Return the annotation for STRING when it is a macro name."
  (and (string-prefix-p "@" string)
       (let ((symbol (intern (upcase string))))
         (cl-loop for doc in kixtart-doc-docs
                  when (kixtart-doc-macro-p doc)
                  when (memq symbol (kixtart-doc-macro-symbols doc))
                  return (concat " " (kixtart-doc-macro-description doc))))))

(defun kixtart-completion-at-point-function ()
  "Complete the symbol at point."
  (and (not (kixtart--in-comment-or-string-p))
       (pcase (bounds-of-thing-at-point 'symbol)
         (`(,beg . ,end)
          (list beg end
                (completion-table-dynamic
                 (lambda (string)
                   (let ((kixtart-completion-list kixtart-keywords)
                         (kixtart-completion-input string))
                     (run-hooks 'kixtart-completion-list-hook)
                     kixtart-completion-list)))
                :annotation-function kixtart-completion-annotation-function)))))

;;;; Imenu

(defun kixtart--create-imenu-index ()
  "Build and return an index alist suitable for Imenu.
Functions are added at the top level of the menu.  Labels are
added into a submenu."
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
    (define-key map (kbd "C-c C-d") #'eldoc)
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
  (setq-local syntax-propertize-function kixtart-syntax-propertize-function)
  (setq imenu-create-index-function #'kixtart--create-imenu-index)
  (tempo-use-tag-list 'kixtart-tempo-tags)
  (add-hook 'completion-at-point-functions
            #'kixtart-completion-at-point-function nil t)
  (add-hook 'which-func-functions #'kixtart-which-function nil t)
  (add-to-list 'font-lock-extend-region-functions
               #'kixtart--font-lock-extend-region-function-def t)
  (add-to-list 'syntax-propertize-extend-region-functions
               #'syntax-propertize-wholelines)
  (when kixtart-doc-docs
    (add-hook 'eldoc-documentation-functions #'kixtart-eldoc-function nil t)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kix\\'" . kixtart-mode))

(provide 'kixtart-mode)
;;; kixtart-mode.el ends here
