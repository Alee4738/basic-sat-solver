;; hw4.lsp (SAT Solver) 
;; functions, predicates, and operators allowed:
;;  quote ['], car, cdr [cadadr, etc.], first, second [third, etc.], 
;;  rest, cons, list, append, length, numberp, stringp, listp, atom, 
;;  symbolp, oddp, evenp, null, not, and, or, cond, if, equal, defun, 
;;  let, let*, =, <, >, +, -, *, /, butlast, nthcdr, count
;;  Note: you are not permitted to use setq or any looping function
;; Author: Andrew Lee
;; Class: CS 161 Aritficial Intelligence







; 
; pass-constraint (constraint assignment)
; @param constraint - a single constraint (a list of integers)
; @param assignment - a variable assignment
; return t if the assignment satisfies that constraint, else nil
; logic: basically the member function on each of the elements of constraint
(defun pass-constraint (constraint assignment)
  (if (null constraint) nil
    ; return t if first member of constraint is in assignment
    ; or if another member of constraint is in assignment
    (or (>= (count (car constraint) assignment) 1)
      (pass-constraint (cdr constraint) assignment));end or
    );end if
  );end defun

; 
; allDefined (constraint assignment)
; @param constraint - a list of integers for a constraint
; @param assignment - a list of integers for a variable assignment
; @return t if all variables in constraint are defined, else nil
;   constraint being nil returns t
(defun allDefined (constraint assignment)
  (if (null constraint) t
    (and 
      (or (>= (count (car constraint) assignment) 1) 
          (>= (count (flipSign (car constraint)) assignment) 1))
      (allDefined (cdr constraint) assignment))
    );end if
  );end defun


; 
; no-conflicts (constraints assignment)
; @param constraints - list of constraints
; @param assignment - a list of integers (variable assignment)
; @return t if, for each constraint, constraint is satisfied or
;   not all variables in constraint are defined, meaning could still be satisfied later
;   else nil
(defun no-conflicts (constraints assignment)
  (if (null constraints) t
    (and (or (pass-constraint (car constraints) assignment)
            (not (allDefined (car constraints) assignment)))
      (no-conflicts (cdr constraints) assignment))
    );end if
  );end defun



; goal-test helper
; pass-all-constraints (constraints assignment)
; @param constraints - list of constraints
; @assignment - a variable assignment
; @return t if assignment satisfies all constraints or if constraints nil, else nil
(defun pass-all-constraints (constraints assignment)
  (if (null constraints) t
    (and (pass-constraint (car constraints) assignment)
      (pass-all-constraints (cdr constraints) assignment))
    );end if
  );end defun

; 
; goal-test (problem assignment)
; @param problem - a sat problem, a 2 element list (n delta)
;   where n is the number of variables and delta is a list of clauses
; @param assignment - a variable assignment (list of variables) in any order
; @return t if assignment solves the sat problem, else nil
(defun goal-test (problem assignment)
  (pass-all-constraints (cadr problem) assignment)
  );end defun



;; model for backtracking search provided in textbook

; 
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


; 
; flipSign (num)
; @param num - an integer
; @return the reverse sign of num
(defun flipSign (num)
  (* -1 num)
  );end defun


; select-unasssigned-variable helper
; nextUnassigned (assignment allNums)
; @param assignment - a list of integers (variable assignment)
; @param allNums - a list of positive integers (all the integers that should be assigned)
; @return an integer not yet assigned in assignment
;   else if all integers were assignedse, nil
(defun nextUnassigned (assignment allNums)
  (if (null allNums) nil
    (if (and (= (count (car allNums) assignment) 0)
            (= (count (flipSign (car allNums)) assignment) 0)) (car allNums)
      (nextUnassigned assignment (cdr allNums)))
    );end if
  );end defun


;
; select-unassigned variable helper
; numArcs (num constraints)
; @param num - integer to look for
; @param constraints - a list of lists of integers (positive and negative)
; @return integer, the number of times num or -num appears in constraints
(defun numArcs (num constraints)
  (if (null constraints) 0
    (+ (count num (car constraints)) (count (flipSign num) (car constraints))
      (numArcs num (cdr constraints)))
    );end if
  );end defun


; maxArcs (nums constraints)
; @param nums - list of integers to check
; @param constraints - list of lists of integers
; @param maxNum - current number that has the max instances
; @param maxValue - the number of times currNum or -currNum appears in constraints
; @return integer that appears in most constraints (positive and negative version)
; if there is a tie, chooses the first number that appeared in nums
(defun maxArcs (nums constraints maxNum maxValue)
  ; Base case: nums ran out
  (if (null nums) maxNum
    (let* ((currNum (car nums)) (currValue (numArcs currNum constraints)))
      (if (> currValue maxValue)
        (maxArcs (cdr nums) constraints currNum currValue)
        (maxArcs (cdr nums) constraints maxNum maxValue)
        );end if
      );end let
    );end if
  );end defun


; select-unassigned-variable helper
; listSubtract (l1 l2)
; @param l1 - list 1, list of integers
; @param l2 - list 2, list of integers
; @return list of integers that are in l1 and not in l2 
;   if x is in l2, then -x will be taken out of l1
(defun listSubtract (l1 l2)
  (if (null l1) nil
    (let* ((currNum (car l1)))
      (if (or (>= (count currNum l2) 1) (>= (count (flipSign currNum) l2) 1))
        (listSubtract (cdr l1) l2)
        (cons (car l1) (listSubtract (cdr l1) l2))
        );end if
      );end let
    );end if
  );end defun

; CAN IMPROVE
; backtrack helper
; select-unassigned-variable (assignment csp)
; @param assignment - a list of integers (variable assignment)
; @param csp - CNF
; @return a (positive) integer not yet assigned in assignment
;   if all variables are assigned, return 0
; strategy: most constraining variable - the variable in the most number of clauses
; implementation: maintain the max variable and max count
(defun select-unassigned-variable (assignment csp)
  (maxArcs (listSubtract (listFromTo 1 (car csp)) assignment) (cadr csp) 0 0)
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
; CAN IMPROVE
; backtrack (assignment csp)
; @param assignment - a variable assignment
; @param csp - CNF
; @return a solution to the csp if one exists, starts by looking at assignment
(defun backtrack (assignment csp)
  ; base case
  (if (complete-assignment assignment csp) assignment
    ; inductive case
    (let* ((var (select-unassigned-variable assignment csp)) (notVar (flipSign var)))
      (cond
        ; positive
        ((and (no-conflicts (cadr csp) (cons var assignment))
          (backtrack (cons var assignment) csp)) (backtrack (cons var assignment) csp))
        ; negative
        ((and (no-conflicts (cadr csp) (cons notVar assignment))
          (backtrack (cons notVar assignment) csp)) (backtrack (cons notVar assignment) csp))
        (t nil); no solution works, backtrack
        );end cond
      );end let
    );end if
  );end defun



;
; sat? (n delta)
; @param n - an integer
; @param delta - CNF defined over n variables
; @return a list of n integers, representing a model of delta, 
;   if delta is satisfiable, otherwise it returns NIL
(defun sat? (n delta)
  (backtrack '() (list n delta))
  );end defun
