;; tests.lsp (SAT Solver testing script)
;; Author: Andrew Lee
;; Class: CS 161 Aritficial Intelligence

(defun reload()
  (and (load "parse_cnf.lsp") (load "hw4.lsp"))
  );end defun

(reload)


; one-constraint (constraint assignment)
(assert (equal t (one-constraint '(1 2 3) '(1 -2 -3))))
(assert (equal t (one-constraint '(-1 -2 -3) '(1 2 -3))))
(assert (equal nil (one-constraint '(1 2 3) '(-1 -2 -3))))
(assert (equal nil (one-constraint '(-1 -2 -3) '(1 2 3))))
(print "one-constraint passed!")


; test-constraints (constraints assignment)
(assert (equal t (test-constraints '((1) (-2 -3) (1 2 3)) '(1 -2 -3))))
(assert (equal t (test-constraints '((-1 3 -2) (-1 2) (-1 -2 -3)) '(1 2 -3))))
(assert (equal nil (test-constraints '((2) (1 3) (1 2 3)) '(-1 -2 -3))))
(assert (equal nil (test-constraints '((-1) (-1 -3) (-1 -2 -3)) '(1 2 3))))
(print "test-constraints passed!")


; goal-test (problem assignment)
(assert (equal t (goal-test '(3 ((1) (-2 -3) (1 2 3))) '(1 -2 -3))))
(assert (equal t (goal-test '(3 ((-1 3 -2) (-1 2) (-1 -2 -3))) '(1 2 -3))))
(assert (equal nil (goal-test '(3 ((2) (1 3) (1 2 3))) '(-1 -2 -3))))
(assert (equal nil (goal-test '(3 ((-1) (-1 -3) (-1 -2 -3))) '(1 2 3))))
; their test
(assert (equal t (goal-test '(3 '((1 -2 3) (-1) (-2 -3))) '(-1 -2 3))))
(print "goal-test passed!")


; select-unassigned-variable (assignment csp)
; for now, just goes in numerical order
(assert (equal 1 (select-unassigned-variable '(1) '(3 ((1) (-2 -3) (1 2 3))))))
(assert (equal 2 (select-unassigned-variable '(1) '(3 ((1) (-2 -3) (1 2 3))))))
(assert (equal 3 (select-unassigned-variable '(1 -2) '(3 ((1) (-2 -3) (1 2 3))))))
(assert (equal nil (select-unassigned-variable '(1 -2 3) '(3 ((1) (-2 -3) (1 2 3))))))

