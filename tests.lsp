;; tests.lsp (SAT Solver testing script)
;; Author: Andrew Lee
;; Class: CS 161 Aritficial Intelligence

(defun reload()
  (and (load "parse_cnf.lsp") (load "hw4.lsp"))
  );end defun


; one-constraint (constraint assignment)
(assert (equal t (pass-constraint '(1 2 3) '(1 -2 -3))))
(assert (equal t (pass-constraint '(-1 -2 -3) '(1 2 -3))))
(assert (equal nil (pass-constraint '(1 2 3) '(-1 -2 -3))))
(assert (equal nil (pass-constraint '(-1 -2 -3) '(1 2 3))))
(print "pass-constraint passed!")


; allDefined (constraint assignment)
(assert (equal t (allDefined '(1 2 3) '(1 -2 -3))))
(assert (equal t (allDefined '() '(-3))))
(assert (equal nil (allDefined '(1 2 3) '(1 -3))))
(assert (equal nil (allDefined '(1 2 3) '(-3))))
(assert (equal nil (allDefined '(1 2 3) '())))
(assert (equal nil (allDefined '(1) '(-3))))
(print "allDefined passed!")


; test-constraints (constraints assignment)
(assert (equal t (pass-all-constraints '((1) (-2 -3) (1 2 3)) '(1 -2 -3))))
(assert (equal t (pass-all-constraints '((-1 3 2) (-1 2) (-1 -2 -3)) '(1 2 -3))))
(assert (equal nil (pass-all-constraints '((2) (1 3) (1 2 3)) '(-1 -2 -3))))
(assert (equal nil (pass-all-constraints '((-1) (-1 -3) (-1 -2 -3)) '(1 2 3))))
(print "pass-all-constraints passed!")


; goal-test (problem assignment)
(assert (equal t (goal-test '(3 ((1) (-2 -3) (1 2 3))) '(1 -2 -3))))
(assert (equal t (goal-test '(3 ((-1 3 2) (-1 2) (-1 -2 -3))) '(1 2 -3))))
(assert (equal nil (goal-test '(3 ((2) (1 3) (1 2 3))) '(-1 -2 -3))))
(assert (equal nil (goal-test '(3 ((-1) (-1 -3) (-1 -2 -3))) '(1 2 3))))
; their test
(assert (equal t (goal-test '(3 ((1 -2 3) (-1) (-2 -3))) '(-1 -2 3))))
(print "goal-test passed!")


; select-unassigned-variable (assignment csp)
; for now, just goes in numerical order
(assert (equal 1 (select-unassigned-variable '() '(3 ((1) (-2 -3) (1 2 3))))))
(assert (equal 2 (select-unassigned-variable '(1) '(3 ((1) (-2 -3) (1 2 3))))))
(assert (equal 3 (select-unassigned-variable '(1 -2) '(3 ((1) (-2 -3) (1 2 3))))))
(assert (equal nil (select-unassigned-variable '(1 -2 3) '(3 ((1) (-2 -3) (1 2 3))))))
(print "select-unassigned-variable passed!")




