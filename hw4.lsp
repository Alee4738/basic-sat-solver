;; hw4.lsp (SAT Solver) 
;; functions, predicates, and operators allowed:
;;  quote ['], car, cdr [cadadr, etc.], first, second [third, etc.], 
;;  rest, cons, list, append, length, numberp, stringp, listp, atom, 
;;  symbolp, oddp, evenp, null, not, and, or, cond, if, equal, defun, 
;;  let, let*, =, <, >, +, -, *, /, butlast, nthcdr, count
;;  Note: you are not permitted to use setq or any looping function
;; Author: Andrew Lee
;; Class: CS 161 Aritficial Intelligence


; goal-test helper
; one-constraint (constraint assignment)
; @param constraint - a single constraint (a list of integers)
; @param assignment - a variable assignment
; return t if the assignment satisfies that constraint, else nil
; logic: basically the member function on each of the elements of constraint
(defun one-constraint (constraint assignment)
  (if (null constraint) nil
    ; return t if first member of constraint is in assignment
    ; or if another member of constraint is in assignment
    (or (>= (count (car constraint) assignment) 1)
      (one-constraint (cdr constraint) assignment));end or
    );end if
  );end defun


; goal-test helper
; test-constraints (constraints assignment)
; @param constraints - list of constraints
; @assignment - a variable assignment
; @return t if assignment satisfies all constraints or if constraints nil, else nil
(defun test-constraints (constraints assignment)
  (if (null constraints) t
    (and (one-constraint (car constraints) assignment)
      (test-constraints (cdr constraints) assignment))
    );end if
  );end defun

; 
; goal-test (problem assignment)
; @param problem - a sat problem, a 2 element list (n delta)
;   where n is the number of variables and delta is a list of clauses
; @param assignment - a variable assignment (list of variables) in any order
; @return t if assignment solves the sat problem, else nil
(defun goal-test (problem assignment)
  (test-constraints (cadr problem) assignment)
  );end defun



