;; tests.lsp (SAT Solver testing script)
;; Author: Andrew Lee
;; Class: CS 161 Aritficial Intelligence

(load "parse_cnf.lsp")
(load "hw4.lsp")

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
; most constraining variable
(assert (equal 1 (select-unassigned-variable '() '(3 ((1) (-2 -3) (1 2 3))))))
(assert (equal 2 (select-unassigned-variable '(1) '(3 ((1) (-2 -3) (1 2 3))))))
(assert (equal 3 (select-unassigned-variable '(1 -2) '(3 ((1) (-2 -3) (1 2 3))))))
(assert (equal 3 (select-unassigned-variable '(1) '(3 ((1) (-2 -3) (1 2 3) (3) (3) (3) (3))))))
(assert (equal 0 (select-unassigned-variable '(1 -2 3) '(3 ((1) (-2 -3) (1 2 3))))))
(print "select-unassigned-variable passed!")


; provided sat sets
(print "solving provided sat sets")
(time (assert (not (null (solve-cnf "cnfs/sat_0/cnf_10.cnf")))))
(print "passed test (size 10)")
(time (assert (not (null (solve-cnf "cnfs/sat/cnf_20.cnf")))))
(print "passed test (size 20)")
(time (assert (not (null (solve-cnf "cnfs/sat_2/cnf_30.cnf")))))
(print "passed test (size 30)")
(time (assert (not (null (solve-cnf "cnfs/sat_1/cnf_50.cnf")))))
(print "passed test (size 50)")
(time (assert (equal nil (solve-cnf "cnfs/unsat/cnf_12.cnf"))))
(print "passed test (size 12)")
(time (assert (equal nil (solve-cnf "cnfs/unsat/cnf_20.cnf"))))
(print "passed test (size 20)")
(time (assert (equal nil (solve-cnf "cnfs/unsat_0/cnf_30.cnf"))))
(print "passed test (size 30)")
(time (assert (equal nil (solve-cnf "cnfs/unsat/cnf_42.cnf"))))
(print "passed test (size 42)")
(print "all provided sat sets passed!")