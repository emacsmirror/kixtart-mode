;;; kixtart-mode-tests.el --- Tests for kixtart-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests to verify the correct operation of kixtart-mode.

;;; Code:

(require 'ert)
(require 'kixtart-mode)

;;; Tests

;;;; Helper macros

(defmacro kixtart-mode-tests--with-temp-buffer (buffer-contents &rest body)
  "Evaluate BODY in a temporary KiXtart mode buffer containing BUFFER-CONTENTS."
  `(with-temp-buffer
     (kixtart-mode)
     (insert ,buffer-contents)
     ,@body))

(defmacro kixtart-mode-tests--test-indentation (buffer-contents)
  "Check indentation of BUFFER-CONTENTS within a temporary KiXtart mode buffer."
  `(kixtart-mode-tests--with-temp-buffer
       ,buffer-contents
     (indent-region (point-min) (point-max))
     (should (equal (buffer-string) ,buffer-contents))))

(defmacro kixtart-mode-tests--test-block-close (buffer-contents &rest strings)
  "Check closing of open blocks within a temporary KiXtart mode buffer.
BUFFER-CONTENTS should define an open block which will be closed by cycling
through STRINGS."
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

(ert-deftest kixtart-indent-command-block-function ()
  "Increase indentation level inside function blocks."
  (kixtart-mode-tests--test-indentation
      ";; Function definition.
Function MyFunction($x, $y)
    $var1 = $x
    $var2 = $y
EndFunction"))

(ert-deftest kixtart-indent-command-block-while ()
  "Increase indentation level inside while blocks."
  (kixtart-mode-tests--test-indentation
      ";; While loop.
While $maybe
    $var1 = 1
    $var2 = 2
Loop"))

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
  "Indentation level for a string only applies to its starting point."
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

(ert-deftest kixtart-close-block-function ()
  "Insert the strings which close an open function block."
  (kixtart-mode-tests--test-block-close
   ";; Function definition.
Function MyFunction($x, $y)
    $var1 = $x
    $var2 = $y
"
   "EndFunction"))

(ert-deftest kixtart-close-block-while ()
  "Insert the strings which close an open while block."
  (kixtart-mode-tests--test-block-close
   ";; While loop.
While $maybe
    $var1 = 1
    $var2 = 2
"
   "Loop"))

;;; kixtart-mode-tests.el ends here
