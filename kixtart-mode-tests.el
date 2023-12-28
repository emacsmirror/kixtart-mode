;;; kixtart-mode-tests.el --- Tests for kixtart-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests to verify the correct operation of kixtart-mode.

;;; Code:

(require 'ert)
(require 'kixtart-mode)

;;;; Helper macros

(defmacro kixtart-mode-tests--with-temp-buffer (buffer-contents &rest body)
  "Evaluate BODY in a temporary KiXtart mode buffer.
The string BUFFER-CONTENTS is inserted into the buffer before
 evaulation."
  (declare (indent 1))
  `(with-temp-buffer
     (kixtart-mode)
     (insert ,buffer-contents)
     ,@body))

(defmacro kixtart-mode-tests--test-indentation (buffer-contents &optional result)
  "Test indentation within a temporary KiXtart mode buffer.
The string BUFFER-CONTENTS is inserted into a temporary buffer
which will be re-indented.  The test succeeds when buffer
contents were unaltered or are now equal to the optional RESULT."
  `(kixtart-mode-tests--with-temp-buffer
       ,buffer-contents
     (let ((inhibit-message t))
       (indent-region (point-min) (point-max)))
     (should (equal (buffer-string) (or ,result ,buffer-contents)))))

(defmacro kixtart-mode-tests--test-block-close (buffer-contents &rest strings)
  "Test block closing within a temporary KiXtart mode buffer.
The string BUFFER-CONTENTS should define an open block which will
be closed by cycling through the insertion of STRINGS."
  (declare (indent 1))
  `(kixtart-mode-tests--with-temp-buffer
       ,buffer-contents
     ;; Test first insert.
     (kixtart-close-command-block)
     (should (equal (car ',strings) (current-word t)))
     ;; Test replacement through a repeated command.
     (dolist (s (cdr ',strings))
       (let ((last-command 'kixtart-close-command-block))
         (kixtart-close-command-block))
       (should (equal s (current-word t))))
     ;; Test undo.
     (let ((last-command 'kixtart-close-command-block))
       (kixtart-close-command-block))
     (should (null (current-word t)))))

(defmacro kixtart-mode-tests--test-imenu-index (buffer-contents &rest alist)
  "Test Imenu index creation within a temporary KiXtart mode buffer.
Check whether the Imenu alist which is genearted for
BUFFER-CONTENTS is equal to ALIST."
  (declare (indent 1))
  `(kixtart-mode-tests--with-temp-buffer
       ,buffer-contents
     (should (equal ,@alist (kixtart--create-imenu-index)))))

;;;; Indentation for individual command blocks

(ert-deftest kixtart-indent-command-block-do ()
  "Increase indentation level inside do blocks."
  (kixtart-mode-tests--test-indentation
   ";; Do loop.
Do
    $var1 = 1
    $var2 = 2
Until $maybe"))

(ert-deftest kixtart-indent-command-block-for ()
  "Increase indentation level inside for blocks."
  (kixtart-mode-tests--test-indentation
   ";; For loop.
For $i = 0 to 10
    $var1 = $i
    $var2 = $i
Next"))

(ert-deftest kixtart-indent-command-block-foreach ()
  "Increase indentation level inside foreach blocks."
  (kixtart-mode-tests--test-indentation
   ";; For Each loop.
For Each $value in $array
    $var1 = $value
    $var2 = $value
Next"))

(ert-deftest kixtart-indent-command-block-function ()
  "Increase indentation level inside function blocks."
  (kixtart-mode-tests--test-indentation
   ";; Function definition.
Function MyFunction($x, $y)
    $var1 = $x
    $var2 = $y
EndFunction"))

(ert-deftest kixtart-indent-command-block-if ()
  "Increase indentation level inside if blocks."
  (kixtart-mode-tests--test-indentation
   ";; If statement.
If $maybe
    $var1 = 1
    $var2 = 2
EndIf"))

(ert-deftest kixtart-indent-command-block-ifelse ()
  "Increase indentation level inside ifelse blocks."
  (kixtart-mode-tests--test-indentation
   ";; If Else statement.
If $maybe
    $var1 = 1
    $var2 = 2
Else
    $var3 = 3
    $var4 = 4
EndIf"))

(ert-deftest kixtart-indent-command-block-select ()
  "Increase indentation level inside select blocks."
  (kixtart-mode-tests--test-indentation
   ";; Select statement.
Select
;; Case statements.
Case $maybe
    $var1 = 1
    $var2 = 2
Case $sometimes
    $var3 = 3
    $var4 = 4
EndSelect"))

(ert-deftest kixtart-indent-command-block-while ()
  "Increase indentation level inside while blocks."
  (kixtart-mode-tests--test-indentation
   ";; While loop.
While $maybe
    $var1 = 1
    $var2 = 2
Loop"))

;;;; Indentation for the beginning of the buffer

(ert-deftest kixtart-indent-beginning-of-buffer ()
  "The first syntax line should indent to column 0."
  (kixtart-mode-tests--test-indentation
   "    If" "If"))

;;;; Indentation for hanging commands

(ert-deftest kixtart-indent-command-block-hanging-case ()
  "Case block indentation is taken from parent select block."
  (kixtart-mode-tests--test-indentation
   ";; Select statement.
Select
;; Case statements.
Case $maybe
    $var1 = 1 Case $sometimes $var2 = 2 Case $always
    $var3 = 3
EndSelect"))

(ert-deftest kixtart-indent-command-block-hanging-else ()
  "Else block indentation is taken from parent if block."
  (kixtart-mode-tests--test-indentation
   ";; If statement.
If $test
    $var1 = 1 Else
    $var2 = 2
EndIf"))

;;;; Indentation for nested command blocks

(ert-deftest kixtart-indent-nested-command-block-do ()
  "Increase relative indentation level inside do blocks."
  (kixtart-mode-tests--test-indentation
   "Function MyFunction
    If 1
        ;; Do loop.
        Do
            $var1 = 1
            $var2 = 2
        Until $maybe
    EndIf
EndFunction"))

(ert-deftest kixtart-indent-nested-command-block-for ()
  "Increase relative indentation level inside for blocks."
  (kixtart-mode-tests--test-indentation
   "Function MyFunction
    If 1
        ;; For loop.
        For $i = 0 to 10
            $var1 = $i
            $var2 = $i
        Next
    EndIf
EndFunction"))

(ert-deftest kixtart-indent-nested-command-block-foreach ()
  "Increase relative indentation level inside foreach blocks."
  (kixtart-mode-tests--test-indentation
   "Function MyFunction
    If 1
        ;; For Each loop.
        For Each $value in $array
            $var1 = $value
            $var2 = $value
        Next
    EndIf
EndFunction"))

(ert-deftest kixtart-indent-nested-command-block-if ()
  "Increase relative indentation level inside if blocks."
  (kixtart-mode-tests--test-indentation
   "Function MyFunction
    If 1
        ;; If statement.
        If $maybe
            $var1 = 1
            $var2 = 2
        EndIf
    EndIf
EndFunction"))

(ert-deftest kixtart-indent-nested-command-block-ifelse ()
  "Increase relative indentation level inside ifelse blocks."
  (kixtart-mode-tests--test-indentation
   "Function MyFunction
    If 1
        ;; If Else statement.
        If $maybe
            $var1 = 1
            $var2 = 2
        Else
            $var3 = 3
            $var4 = 4
        EndIf
    EndIf
EndFunction"))

(ert-deftest kixtart-indent-nested-command-block-select ()
  "Increase relative indentation level inside select blocks."
  (kixtart-mode-tests--test-indentation
   "Function MyFunction
    If 1
        ;; Select statement.
        Select
        ;; Case statements.
        Case $maybe
            $var1 = 1
            $var2 = 2
        Case $sometimes
            $var3 = 3
            $var4 = 4
        EndSelect
    EndIf
EndFunction"))

(ert-deftest kixtart-indent-nested-command-block-while ()
  "Increase relative indentation level inside while blocks."
  (kixtart-mode-tests--test-indentation
   "Function MyFunction
    If 1
        ;; While loop.
        While $maybe
            $var1 = 1
            $var2 = 2
        Loop
    EndIf
EndFunction"))

;;;; Indentation for command blocks

(ert-deftest kixtart-indent-with-inline-command-blocks ()
  "Indentation level is not affected by inline command blocks."
  (kixtart-mode-tests--test-indentation
   "If $sometimes If $maybe
    $var = 1 If $always
        $var = 2
    EndIf
EndIf EndIf"))

;;;; Indentation for parenthesis level

(ert-deftest kixtart-indent-with-parens-inline ()
  "Increase indentation level with parens sharing a line."
  (kixtart-mode-tests--test-indentation
   "(1
    2
    3 (4
        5
        6))"))

(ert-deftest kixtart-indent-with-parens-outline ()
  "Increase indentation level with parens on their own line."
  (kixtart-mode-tests--test-indentation
   "(
    1
    2
    3
    (
        4
        5
        6
    )
)"))

(ert-deftest kixtart-indent-with-inline-parens ()
  "Indentation level is not affected by inline parens."
  (kixtart-mode-tests--test-indentation
   "((1
    2
    3
    ((4
        5
        6))))"))

;;;; Indentation ignores strings

(ert-deftest kixtart-indent-ignores-string-contents ()
  "Indentation level is not affected by string contents."
  (kixtart-mode-tests--test-indentation
   "Function MyFunction
    If $maybe
        $var = 'If $sometimes
If $always'
        $var = 2
    EndIf
EndFunction"))

(ert-deftest kixtart-indent-only-considers-string-start ()
  "Indentation level for a string only applies to its opening."
  (kixtart-mode-tests--test-indentation
   "Function MyFunction
    If $maybe
        $var1 =
        'one
two
three'
    EndIf

    $var2 =
    'four
five
six'
EndFunction"))

;;;; Indentation ignores comments

(ert-deftest kixtart-indent-ignores-single-line-comment ()
  "Indentation level is not affected by a single-line comments."
  (kixtart-mode-tests--test-indentation
   "If $maybe
    ; If $sometimes
    $var = 1
EndIf"))

(ert-deftest kixtart-indent-ignores-multi-line-comment ()
  "Indentation level is not affected by a multi-line comment."
  (kixtart-mode-tests--test-indentation
   "If $maybe
    /*
    If $sometimes
        $var = 2
    */
    $var = 1
EndIf"))

;;;; Indentation with correct syntax parsing

(ert-deftest kixtart-indent-honors-symbol-boundaries ()
  "Indentation level is not affected by symbol boundaries."
  (kixtart-mode-tests--test-indentation
   "Function Function:1
    $var = 1
EndFunction

$var = 2"))

;;;; Indentation for multiline expressions

(ert-deftest kixtart-multiline-separators ()
  "Indentation level is increased for trailing separators."
  (kixtart-mode-tests--test-indentation
   ";; Declare array.
$x = 1, 2,
    3,
    4, 5
$y = 6"))

(ert-deftest kixtart-multiline-separators-with-comments ()
  "Indentation level is increased for trailing separators."
  (kixtart-mode-tests--test-indentation
   ";; Declare array.
$x = 1, 2,
    ;; Third.
    3,
    ;; Fourth and fifth.
    4, 5
$y = 6"))

(ert-deftest kixtart-multiline-separators-in-list ()
  "Indentation is increased by a single level in a separated list."
  (kixtart-mode-tests--test-indentation
   ";; Function call or definition.
Function:1($a,
    $b, $c,
    $x,
    $y, $z
)"))

(ert-deftest kixtart-multiline-separators-in-list-with-comments ()
  "Indentation is increased by a single level in a separated list."
  (kixtart-mode-tests--test-indentation
   ";; Function call or definition.
Function:1($a,
    ;; Second and third.
    $b, $c,
    ;; Forth.
    $x,
    ;; Fifth and sixth.
    $y, $z
)"))

(ert-deftest kixtart-multiline-indicator ()
  "Indentation is increased by the special multiline comment."
  (kixtart-mode-tests--test-indentation
   ";; A multiline command.
Copy ;\\
    $source ;\\
    $desintation ;\\
    /c /h /r /s
Quit"))

(ert-deftest kixtart-multiline-indicator-with-comments ()
  "Indentation is increased by the special multiline comment.
Comments persist the indentation level."
  (kixtart-mode-tests--test-indentation
   ";\\
Copy ;\\
    ;; The source directory.
    $source ;\\
    ;; The destination directory.
    $desintation ;\\
    ;; Flags.
    /c /h /r /s
Quit"))

(ert-deftest kixtart-multiline-indicator-script-line-only ()
  "Indentation is increased by the special multiline comment.
The special multiline comment is only valid and persisted from
the end of most recent line which contains uncommented KiXtart
syntax."
  (kixtart-mode-tests--test-indentation
   ";\\
;\\
$a ;; Not special. \\
$b ;; Special. ;\\
    $c
$d"))

;;;; Beginning of defun

(ert-deftest kixtart-beginning-of-defun-backwards ()
  (kixtart-mode-tests--with-temp-buffer
      "
Function Function1

Function Function2

Function Function:3
    ;; Function Comment
    /*
Function Comment
    */
    $function = '
Function String
'

Function Function4"
    (beginning-of-defun)
    (should (looking-at-p "^Function Function4$"))
    (beginning-of-defun 2)
    (should (looking-at-p "^Function Function2$"))
    (beginning-of-defun 2)
    (should (looking-at-p "^Function Function1$"))
    (beginning-of-defun)
    (should (looking-at-p "^Function Function1$"))))

(ert-deftest kixtart-beginning-of-defun-forwards ()
  (kixtart-mode-tests--with-temp-buffer
      "
Function Function1

Function Function:2
    ;; Function Comment
    /*
Function Comment
    */
    $function = '
Function String
'

Function Function3

Function Function4
"
    (goto-char (point-min))
    (beginning-of-defun -1)
    (should (looking-at-p "^Function Function1$"))
    (beginning-of-defun -2)
    (should (looking-at-p "^Function Function3$"))
    (beginning-of-defun -2)
    (should (looking-at-p "^Function Function4$"))
    (beginning-of-defun -1)
    (should (looking-at-p "^Function Function4$"))))

;;;; End of defun

(ert-deftest kixtart-end-of-defun-forwards ()
  (kixtart-mode-tests--with-temp-buffer
      "
Function Function1
EndFunction

Function Function:2
    ;; Function Comment
    /*
Function Comment
    */
    $function = '
Function String
'
EndFunction

Function Function3 EndFunction

Function Function4
EndFunction"
    (goto-char (point-min))
    (end-of-defun)
    (should (save-excursion
              (forward-line -2)
              (looking-at-p "^Function Function1$")))
    (end-of-defun 2)
    (should (save-excursion
              (forward-line -1)
              (looking-at-p "^Function Function3 EndFunction$")))
    (end-of-defun 2)
    (should (save-excursion
              (forward-line -1)
              (beginning-of-line)
              (looking-at-p "^Function Function4$")))
    (end-of-defun)
    (should (save-excursion
              (forward-line -1)
              (beginning-of-line)
              (looking-at-p "^Function Function4$")))))

(ert-deftest kixtart-end-of-defun-backwards ()
  (kixtart-mode-tests--with-temp-buffer
      "
Function Function1
EndFunction

Function Function:2
    ;; Function Comment
    /*
Function Comment
    */
    $function = '
Function String
'
EndFunction

Function Function3 EndFunction

Function Function4
EndFunction"
    (end-of-defun -1)
    (should (save-excursion
              (forward-line -1)
              (looking-at-p "^Function Function3 EndFunction$")))
    (end-of-defun -2)
    (should (save-excursion
              (forward-line -2)
              (looking-at-p "^Function Function1$")))
    ;; Attempts to overshoot seem to be governed by the internal implementation.
    ;; (end-of-defun -1)
    ;; (should (looking-at-p "^Function Function1$"))
    ))

;;;; Closing open command blocks

(ert-deftest kixtart-close-block-case ()
  "Insert the strings which close an open case block."
  (kixtart-mode-tests--test-block-close
      ";; Select statement.
Select
;; Case statements.
Case $maybe
    $var1 = 1
    $var2 = 2
"
    "Case" "EndSelect"))

(ert-deftest kixtart-close-block-do ()
  "Insert the strings which close an open do block."
  (kixtart-mode-tests--test-block-close
      ";; Do loop.
Do
    $var1 = 1
    $var2 = 2
"
    "Until"))

(ert-deftest kixtart-close-block-for ()
  "Insert the strings which close an open for block."
  (kixtart-mode-tests--test-block-close
      ";; For loop.
For $i = 0 to 10
    $var1 = $i
    $var2 = $i
"
    "Next"))

(ert-deftest kixtart-close-block-foreach ()
  "Insert the strings which close an open foreach block."
  (kixtart-mode-tests--test-block-close
      ";; For Each loop.
For Each $value in $array
    $var1 = $value
    $var2 = $value
"
    "Next"))

(ert-deftest kixtart-close-block-function ()
  "Insert the strings which close an open function block."
  (kixtart-mode-tests--test-block-close
      ";; Function definition.
Function MyFunction($x, $y)
    $var1 = $x
    $var2 = $y
"
    "EndFunction"))

(ert-deftest kixtart-close-block-if ()
  "Insert the strings which close an open if block."
  (kixtart-mode-tests--test-block-close
      ";; If statement.
If $maybe
    $var1 = 1
    $var2 = 2
"
    "Else" "EndIf"))

(ert-deftest kixtart-close-block-ifelse ()
  "Insert the strings which close an open ifelse block."
  (kixtart-mode-tests--test-block-close
      ";; If statement.
If $maybe
    $var1 = 1
    $var2 = 2
Else
    $var3 = 3
    $var4 = 4
"
    "EndIf"))

(ert-deftest kixtart-close-block-select ()
  "Insert the strings which close an open select block."
  (kixtart-mode-tests--test-block-close
      ";; Select statement.
Select
"
    "Case"))

(ert-deftest kixtart-close-block-while ()
  "Insert the strings which close an open while block."
  (kixtart-mode-tests--test-block-close
      ";; While loop.
While $maybe
    $var1 = 1
    $var2 = 2
"
    "Loop"))

(ert-deftest kixtart-close-block-if-downcase ()
  "Insert the downcased strings which close an open if block."
  (kixtart-mode-tests--test-block-close
      ";; If statement.
if $maybe
    $var1 = 1
    $var2 = 2
"
    "else" "endif"))

(ert-deftest kixtart-close-block-if-upcase ()
  "Insert the upcased strings which close an open if block."
  (kixtart-mode-tests--test-block-close
      ";; If statement.
IF $maybe
    $var1 = 1
    $var2 = 2
"
    "ELSE" "ENDIF"))

;;;; Imenu index entries

(ert-deftest kixtart-imenu-functions ()
  "Function names appear at the top level of the index."
  (kixtart-mode-tests--test-imenu-index
      "Function MyFunc1 EndFunction
Function MyFunc2
EndFunction Function MyFunc3
EndFunction
Function

	MyFunc4
EndFunction"
    '(("MyFunc1" . 1)
      ("MyFunc2" . 30)
      ("MyFunc3" . 59)
      ("MyFunc4" . 88))))

(ert-deftest kixtart-imenu-functions-ignoring-comments-and-strings ()
  "Function names are ignored in comments and strings."
  (kixtart-mode-tests--test-imenu-index
      "Function MyFunc1 EndFunction
\"Function MyFunc2\"
EndFunction Function /*MyFunc3
EndFunction
Function*/

	MyFunc4
EndFunction"
    '(("MyFunc1" . 1)
      ("MyFunc4" . 61))))

(ert-deftest kixtart-imenu-labels ()
  "Label names appear in a sub-menu of the index.
The leading colon is removed from each label name."
  (kixtart-mode-tests--test-imenu-index
      ":one :two

  :three
:four"
    '(("/Labels" . (("one"   . 1)
                    ("two"   . 6)
                    ("three" . 14)
                    ("four"  . 21))))))

(ert-deftest kixtart-imenu-labels-ignoring-comments-and-strings ()
  "Labels are ignored in comments and strings."
  (kixtart-mode-tests--test-imenu-index
      "\":one\" :two
/*  :three

:four*/"
    '(("/Labels" . (("two" . 8))))))

;;; kixtart-mode-tests.el ends here
