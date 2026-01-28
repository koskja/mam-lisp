;;;; Zadání úloh pro témátko Lisp - Lambdy a makra

(in-package #:cl-user)

;;; ---------------------------------------------------------------------------
;;; 1. Range (1b)
;;; ---------------------------------------------------------------------------

(defun range (argumenty)
  "Vrátí seznam čísel.
   
   Možnosti volání:
   (range end) -> seznam 0..end-1 (krok 1)
   (range end start) -> seznam start..end-1 (krok 1)
   (range end start step) -> seznam start..end (s krokem step, nezahrnuje end)"
  ;; TODO: Implementujte funkci range s podporou optional parametrů.
  '())

;;; ---------------------------------------------------------------------------
;;; 2. Seznam třetích mocnin (1b)
;;; ---------------------------------------------------------------------------

(defun odd-cubes-example (n)
  "Vrátí seznam třetích mocnin lichých čísel od 1 do n včetně.
   Použijte funkce mapcar, remove-if-not a vaši funkci range."
  ;; TODO: Implementujte pomocí mapcar, remove-if-not a range.
  '())

;; (odd-cubes-example 10) ; => (1 27 125 343 729)

;;; ---------------------------------------------------------------------------
;;; 3. Vlastní implementace funkcí pro seznamy (2b)
;;; ---------------------------------------------------------------------------

(defun my-length (sequence)
  "Vlastní implementace funkce length pro seznamy."
  ;; TODO: vraťte délku seznamu.
  0)

;; (my-length '(1 2 3)) ; => 3

(defun my-map (fn sequence)
  "Vlastní implementace funkce mapcar pro seznamy."
  ;; TODO: vraťte seznam výsledků aplikace funkce fn na všechny prvky seznamu.
  '())

;; (my-map (lambda (x) (+ x 1)) '(1 2 3)) ; => (2 3 4)

(defun my-filter (pred sequence)
  "Vlastní implementace funkce remove-if-not (filter) pro seznamy."
  ;; TODO: vraťte seznam prvků splňujících predikát.
  '())

;; (my-filter #'evenp '(1 2 3 4)) ; => (2 4)

(defun my-left-fold (fn initial sequence)
  "Implementace levého fold (reduce).
   Příklad vyhodnocení pro (a b c) a start: f(f(f(start, a), b), c)."
  ;; TODO: provedťe levý fold nad sekvencí.
  initial)

;; (my-left-fold (lambda (acc x) (cons x acc)) nil '(1 2 3)) ; => (3 2 1)

(defun my-right-fold (fn initial sequence)
  "Implementace pravého fold.
   Příklad vyhodnocení pro (a b c) a start: f(a, f(b, f(c, start)))."
  ;; TODO: provedťe pravý fold nad sekvencí.
  initial)

;; (my-right-fold (lambda (x acc) (cons x acc)) nil '(1 2 3)) ; => (1 2 3)

;;; ---------------------------------------------------------------------------
;;; 4. Makro while (2b)
;;; ---------------------------------------------------------------------------

(defmacro while (condition &body body)
  "Makro pro cyklus while."
  ;; TODO: Implementujte makro while.
  )

;; (let ((x 0)) (while (< x 3) (print x) (incf x))) ; => vypíše 0, 1, 2

;;; ---------------------------------------------------------------------------
;;; 5. Makro replace-symbol (3b)
;;; ---------------------------------------------------------------------------

(defmacro replace-symbol (old new &body body)
  "Nahradí všechny výskyty symbolu old za new v těle body."
  ;; TODO: Implementujte makro replace-symbol.
  )

;; (replace-symbol x y (+ x y z)) ; => (+ y y z)


;;; ---------------------------------------------------------------------------
;;; Test harness utilities
;;; ---------------------------------------------------------------------------

(defstruct test-result
  (task nil)
  (description "")
  (expected nil)
  (actual nil)
  (success nil)
  (error nil))

(defparameter *test-results* '())
(defparameter *task-tests* '())

(defun reset-test-results ()
  (setf *test-results* '()))

(defun record-test-result (task description expected actual success &optional error)
  (push (make-test-result
         :task task
         :description description
         :expected expected
         :actual actual
         :success success
         :error error)
        *test-results*)
  success)

(defun approx= (a b &optional (epsilon 1e-6))
  (and (numberp a)
       (numberp b)
       (<= (abs (- a b)) epsilon)))

(defmacro expect-equal (task description actual expected &key (test #'equal))
  (let ((task-var (gensym "TASK"))
        (description-var (gensym "DESCRIPTION"))
        (expected-var (gensym "EXPECTED"))
        (expected-ready-var (gensym "EXPECTED-READY-P"))
        (actual-var (gensym "ACTUAL"))
        (test-var (gensym "TEST"))
        (condition-var (gensym "CONDITION")))
    `(let ((,task-var ,task)
           (,description-var ,description))
       (let ((,test-var ,test)
             (,expected-var nil)
             (,expected-ready-var nil))
         (labels ((%record (actual success &optional error)
                    (record-test-result ,task-var
                                        ,description-var
                                        (if ,expected-ready-var
                                            ,expected-var
                                            :unavailable)
                                        actual
                                        success
                                        error)))
           (handler-case
               (progn
                 (setf ,expected-var ,expected
                       ,expected-ready-var t)
                 (let ((,actual-var ,actual))
                   (%record ,actual-var
                            (funcall ,test-var ,actual-var ,expected-var))))
             (error (,condition-var)
               (%record nil nil ,condition-var))))))))

(defmacro expect-true (task description actual)
  `(expect-equal ,task ,description ,actual t :test #'eq))

(defmacro expect-false (task description actual)
  `(expect-equal ,task ,description ,actual nil :test #'eq))

(defmacro deftasktest (name (&rest args) &body body)
  `(progn
     (defun ,name ,args ,@body)
     (setf *task-tests*
           (append (remove #',name *task-tests* :test #'eq)
                   (list #',name)))
     ',name))

(defun run-all-task-tests ()
  (reset-test-results)
  (dolist (fn *task-tests*)
    (funcall fn))
  (nreverse *test-results*))

(defun print-test-report (&optional (results (run-all-task-tests)))
  (let* ((total (length results))
         (passed (count-if #'test-result-success results)))
    (format t "~%--- Test Report (~d/~d passed) ---~%" passed total)
    (dolist (result results)
      (format t "~:[FAIL~;PASS~] ~a ~a~%"
              (test-result-success result)
              (test-result-task result)
              (test-result-description result))
      (unless (test-result-success result)
        (format t "  expected: ~s~%  actual: ~s~%"
                (test-result-expected result)
                (test-result-actual result))
        (when (test-result-error result)
          (format t "  error: ~a~%"
                  (test-result-error result)))))
    results))

;;; ---------------------------------------------------------------------------
;;; Tests
;;; ---------------------------------------------------------------------------

(deftasktest test-range ()
  (expect-equal :range "n=5" (range 5) '(0 1 2 3 4))
  (expect-equal :range "n=0" (range 0) '())
  (expect-equal :range "start=2 end=5" (range 5 2) '(2 3 4))
  (expect-equal :range "start=0 end=10 step=2" (range 10 0 2) '(0 2 4 6 8))
  (expect-equal :range "start=10 end=0 step=-2" (range 0 10 -2) '(10 8 6 4 2)))

(deftasktest test-odd-cubes ()
  (let ((expected '(1 27 125 343 729)))
    (expect-equal :odd-cubes "1..9 odd cubes"
                  (odd-cubes-example 9)
                  expected)))

(deftasktest test-my-length ()
  (expect-equal :length "list length" (my-length '(a b c)) 3)
  (expect-equal :length "empty" (my-length '()) 0))

(deftasktest test-my-map ()
  (expect-equal :map "squares" (my-map (lambda (x) (* x x)) '(1 2 3)) '(1 4 9)))

(deftasktest test-my-filter ()
  (expect-equal :filter "evens" (my-filter #'evenp '(1 2 3 4)) '(2 4)))

(deftasktest test-my-folds ()
  (expect-equal :fold "left list construction" 
                (my-left-fold (lambda (acc x) (cons x acc)) nil '(1 2 3)) 
                '(3 2 1))
  (expect-equal :fold "right list construction" 
                (my-right-fold (lambda (x acc) (cons x acc)) nil '(1 2 3)) 
                '(1 2 3)))

(deftasktest test-while ()
  (let ((x 0)
        (res '()))
    (while (< x 5)
      (push x res)
      (incf x))
    (expect-equal :while "collect 0..4 reversed" res '(4 3 2 1 0))))

(deftasktest test-replace-symbol ()
  (expect-equal :replace-symbol "basic replacement"
                (let ((y 2)) (replace-symbol x y (+ x x)))
                4)
  (expect-equal :replace-symbol "nested replacement"
                (let ((y 2)) (replace-symbol x y (* x (+ x x))))
                8))


(progn
  (test-range)
  (print-test-report *test-results*))
