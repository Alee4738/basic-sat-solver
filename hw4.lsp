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



;; model for backtracking search provided in textbook


; select-unassigned-variable helper
; listFromTo (start end)
; @param start - integer for the first number in list
; @param end - integer for the last number in list
; @return a list of integers from start to end (inclusive)
(defun listFromTo (start end)
  (cond 
    ((> start end) nil)
    ((= start end) (list end))
    (t (cons start (listFromTo (+ start 1) end)))
    );end cond
  );end defun

(defun flipSign (num)
  (* -1 num)
  );end defun

; select-unasssigned-variable helper
; nextUnassigned (assignment allNums)
; @param assignment - a list of integers (variable assignment)
; @param allNums - a list of positive integers (all the integers that should be assigned)
; @return an integer not yet assigned in assignment
;   else if all integers were assigned, nil
(defun nextUnassigned (assignment allNums)
  (if (null allNums) nil
    (if (and (= (count (car allNums) assignment) 0)
            (= (count (flipSign (car allNums)) assignment) 0)) (car allNums)
      (nextUnassigned assignment (cdr allNums)))
    );end if
  );end defun

; backtrack helper
; select-unassigned-variable (assignment csp)
; @param assignment - a list of integers (variable assignment)
; @param csp - CNF
; @return a (positive) integer not yet assigned in assignment
(defun select-unassigned-variable (assignment csp)
  (nextUnassigned assignment (listFromTo 1 (car csp)))
  );end defun


; backtrack helper
; complete-assignment (assignment csp)
; @param assignment - a list of integers (variable assignment)
; @param csp - CNF
; @return t if assignment is a complete assignment, else nil
(defun complete-assignment (assignment csp)
  (= (length assignment) (car csp))
  );end defun

; 
; backtrack (assignment csp)
(defun backtrack (assignment csp)
  (if (complete-assignment assignment csp) assignment
    (let* ((var (select-unassigned-variable assignment csp)))
      ; try positive first, then negative
      (cons var assignment)
      (cons (flipSign var) assignment)



      );end let
    );end if
  );end defun



; functions to implement








;
; sat? (n delta)
; @param n - an integer
; @param delta - CNF defined over n variables
; @return a list of n integers, representing a model of delta, 
;   if delta is satisfiable, otherwise it returns NIL
(defun sat? (n delta)
  (backtrack '() (list n delta))
  );end defun

