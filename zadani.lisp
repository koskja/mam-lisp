;;;; Zadání úloh pro témátko Lisp

(in-package #:cl-user)

;;; ---------------------------------------------------------------------------
;;; Easy (3 b)
;;; ---------------------------------------------------------------------------

(defun easy-sum-two-numbers (a b)
  ;; TODO: vraťte součet.
  0)

;; (easy-sum-two-numbers 2 3) ; => 5

(defun easy-range-to-n (n)
  ;; TODO: vraťte (list 1 2 ... n).
  '())

;; (easy-range-to-n 5) ; => (1 2 3 4 5)

(defun easy-stats (numbers)
  ;; TODO: dopočítejte statistiky a vraťte (list average min max).
  '(0.0 0 0))

;; (easy-stats '(2 6 4 10)) ; => (5.5 2 10)

;;; ---------------------------------------------------------------------------
;;; Medium
;;; ---------------------------------------------------------------------------

(defun balanced-parentheses-p (text)
  ;; TODO: vraťte T pokud je text vyvážený, jinak NIL.
  nil)

;; (balanced-parentheses-p "(()())") ; => T
;; (balanced-parentheses-p "(()") ; => NIL

(defun my-length (sequence)
  "Vlastní tail-call implementace funkce length pro seznamy."
  ;; TODO: vraťte délku seznamu.
  0)

(defun my-reverse (sequence)
  "Vlastní tail-call implementace funkce reverse pro seznamy."
  ;; TODO: vraťte seznam v opačném pořadí.
  '())

(defun my-map (fn sequence)
  "Vlastní tail-call implementace funkce map pro seznamy."
  ;; TODO: vraťte seznam výsledků aplikace funkce fn na všechny prvky seznamu.
  '())

(defun my-filter (pred sequence)
  "Vlastní tail-call implementace funkce filter pro seznamy."
  ;; TODO: vraťte seznam prvků splňujících predikát.
  '())

(defun my-left-fold (fn initial sequence)
  "Tail-call implementace levého fold."
  ;; TODO: provedťe levý fold nad sekvencí.
  initial)

(defun my-right-fold (fn initial sequence)
  "Tail-call implementace pravého fold."
  ;; TODO: provedťe pravý fold nad sekvencí.
  initial)

;; (my-map #'sqrt '(4 9 16)) ; => (2 3 4)
;; (my-filter #'evenp '(1 2 3 4 5)) ; => (2 4)
;; (my-left-fold #'append '() '((a b) (c) (d e))) ; => (a b c d e)
;; (my-right-fold #'append '() '((a b) (c) (d e))) ; => (d e c a b)

(defun split-string (string delimiter)
  ;; TODO: vraťte seznam podřetězců oddělených daným oddělovačem. Oddělovač je jeden znak. 
  '())

;; (split-string "a,b,c," ",") ; => '("a" "b" "c" "")

(defun my-sort (sequence compare-fn)
  ;; TODO: vraťte sekvenci setříděnou podle porovnávací funkce compare-fn.
  sequence)

(defun make-stack ()
  ;; TODO: vraťte prázdný zásobník, který budou ostatní operace modifikovat in-place.
  '())

(defun stack-push (value stack)
  ;; TODO: přidejte prvek přímo do existujícího zásobníku a vraťte tentýž objekt.
  stack)

(defun stack-pop (stack)
  ;; TODO: odeberte prvek ze zásobníku in-place a vraťte dvojici (prvek stejný-zásobník).
  '(nil stack))

(defun make-queue ()
  ;; TODO: vraťte prázdnou frontu, kterou budou ostatní operace upravovat in-place.
  '())

(defun queue-enqueue (value queue)
  ;; TODO: přidejte prvek do existující fronty a vraťte tentýž objekt.
  queue)

(defun queue-dequeue (queue)
  ;; TODO: vyjměte prvek z čela fronty in-place a vraťte dvojici (prvek stejná-fronta).
  '(nil queue))

;;; ---------------------------------------------------------------------------
;;; Heavy
;;; ---------------------------------------------------------------------------

(defun balanced-multi-brackets-p (text)
  ;; TODO: vraťte T pouze pokud je řetězec vyvážený.
  nil)

;; (balanced-multi-brackets-p "{[()()]([]{[()[]]})}") ; => T
;; (balanced-multi-brackets-p "{[(])}") ; => NIL

(defun bst-make-empty ()
  ;; TODO: vraťte prázdný strom.
  '())

(defun bst-make-copy (tree)
  ;; TODO: vraťte kopii stromu.
  nil)

(defun bst-insert (tree value)
  ;; TODO: vložte hodnotu přímo do existujícího stromu a vraťte tentýž strom.
  nil)

(defun bst-find (tree value)
  ;; TODO: vraťte hledaný uzel nebo NIL.
  nil)

(defun bst-delete (tree value)
  ;; TODO: odstraňte hodnotu ze stromu in-place a vraťte tentýž strom.
  nil)

(defun bst-inorder (tree)
  ;; TODO: vraťte uspořádaný seznam hodnot.
  '())

(defun remove-triple-parentheses (text)
  ;; TODO: vraťte normalizovaný řetězec.
  "")

;; (remove-triple-parentheses "(((abc)))") ; => "abc"
;; (remove-triple-parentheses "(((((abc)d)))e)") ; => "((abc)de)"

;;; ---------------------------------------------------------------------------
;;; Ultra heavy
;;; ---------------------------------------------------------------------------

(defun run-brainfuck (source getchar putchar)
  "Napište interpretr pro Brainfuck. source = BF program, getchar = funkce pro čtení znaku, putchar = funkce pro zápis znaku.
Výstupem může být řetězec nebo struktura s detaily běhu."
  ;; TODO: vykonejte Brainfuck program. Výstup zaznamenejte průběžně pomocí putchar.
  nil)

(defun mini-scheme-repl (getchar putchar)
  "Implementujte vlastní malý Scheme: parser, evaluator a REPL."
  ;; TODO: spusťte REPL pomocí getchar a putchar.
  nil)

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
;;; Test harness definitions
;;; ---------------------------------------------------------------------------

(deftasktest test-easy-sum-two-numbers ()
  (expect-equal :easy/sum "2 + 3 = 5"
                (easy-sum-two-numbers 2 3)
                5
                :test #'=)
  (expect-equal :easy/sum "-10 + 4 = -6"
                (easy-sum-two-numbers -10 4)
                -6
                :test #'=))

(deftasktest test-easy-range-to-n ()
  (expect-equal :easy/range "n = 5"
                (easy-range-to-n 5)
                '(1 2 3 4 5))
  (expect-equal :easy/range "n = 0"
                (easy-range-to-n 0)
                '()))

(deftasktest test-easy-stats ()
  (flet ((stats= (actual expected)
           (and (listp actual)
                (listp expected)
                (= (length actual) 3)
                (= (length expected) 3)
                (approx= (first actual) (first expected))
                (= (second actual) (second expected))
                (= (third actual) (third expected)))))
    (expect-equal :easy/stats "numbers: 2 6 4 10"
                  (easy-stats '(2 6 4 10))
                  '(5.5 2 10)
                  :test #'stats=)
    (expect-equal :easy/stats "single element"
                  (easy-stats '(7))
                  '(7 7 7)
                  :test #'stats=)))

(deftasktest test-balanced-parentheses-p ()
  (expect-true :medium/parens "(()())"
               (balanced-parentheses-p "(()())"))
  (expect-false :medium/parens "(()"
                (balanced-parentheses-p "(()"))
  (expect-false :medium/parens "())("
                (balanced-parentheses-p "())(")))

(deftasktest test-my-length ()
  (expect-equal :medium/length "three elements"
                (my-length '(a b c))
                3
                :test #'=)
  (expect-equal :medium/length "empty list"
                (my-length '())
                0
                :test #'=))

(deftasktest test-my-reverse ()
  (expect-equal :medium/reverse "numbers"
                (my-reverse '(1 2 3 4))
                '(4 3 2 1))
  (expect-equal :medium/reverse "single value"
                (my-reverse '(foo))
                '(foo)))

(deftasktest test-my-map ()
  (expect-equal :medium/map "square numbers"
                (my-map (lambda (x) (* x x)) '(2 3 4))
                '(4 9 16))
  (expect-equal :medium/map "uppercase symbols"
                (my-map #'symbol-name '(foo bar))
                '("FOO" "BAR")))

(deftasktest test-my-filter ()
  (expect-equal :medium/filter "even numbers"
                (my-filter #'evenp '(1 2 3 4 5 6))
                '(2 4 6))
  (expect-equal :medium/filter "keep positives"
                (my-filter (lambda (x) (> x 0)) '(-2 -1 0 1 2))
                '(1 2)))

(deftasktest test-folds ()
  (expect-equal :medium/left-fold "append lists"
                (my-left-fold #'append '() '((a b) (c) (d e)))
                '(a b c d e))
  (expect-equal :medium/right-fold "append lists"
                (my-right-fold #'append '() '((a b) (c) (d e)))
                '(d e c a b)))

(deftasktest test-split-string ()
  (expect-equal :medium/split "comma separated"
                (split-string "a,b,c" ",")
                '("a" "b" "c")))

(deftasktest test-my-sort ()
  (expect-equal :medium/sort "ascending numbers"
                (my-sort '(3 1 4 1 5 9) #'<)
                '(1 1 3 4 5 9))
  (flet ((symbol-desc (x y)
           (string> (symbol-name x) (symbol-name y))))
    (expect-equal :medium/sort "custom predicate desc"
                  (my-sort '(a c b) #'symbol-desc)
                  '(c b a))))

(deftasktest test-stack ()
  (let ((stack (make-stack)))
    (expect-true :medium/stack "push keeps same instance (1)"
                 (eq stack (stack-push 1 stack)))
    (expect-true :medium/stack "push keeps same instance (2)"
                 (eq stack (stack-push 2 stack)))
    (destructuring-bind (top same-stack) (stack-pop stack)
      (expect-equal :medium/stack "last-in pops first"
                    top
                    2
                    :test #'=)
      (expect-true :medium/stack "pop returns same stack instance"
                   (eq same-stack stack))
      (destructuring-bind (next same-stack-2) (stack-pop stack)
        (expect-equal :medium/stack "second pop yields 1"
                      next
                      1
                      :test #'=)
        (expect-true :medium/stack "stack instance reused again"
                     (eq same-stack-2 stack))
        (expect-true :medium/stack "stack remains reusable"
                     (eq stack (stack-push 42 stack)))
        (destructuring-bind (sentinel same-stack-3) (stack-pop stack)
          (expect-equal :medium/stack "pushing after clearing works"
                        sentinel
                        42
                        :test #'=)
          (expect-true :medium/stack "pop still returns same stack"
                       (eq same-stack-3 stack)))))))

(deftasktest test-queue ()
  (let ((queue (make-queue)))
    (expect-true :medium/queue "enqueue keeps same instance (a)"
                 (eq queue (queue-enqueue 'a queue)))
    (expect-true :medium/queue "enqueue keeps same instance (b)"
                 (eq queue (queue-enqueue 'b queue)))
    (expect-true :medium/queue "enqueue keeps same instance (c)"
                 (eq queue (queue-enqueue 'c queue)))
    (destructuring-bind (first same-queue) (queue-dequeue queue)
      (expect-equal :medium/queue "first dequeue returns a"
                    first
                    'a
                    :test #'eq)
      (expect-true :medium/queue "dequeue returns same queue instance"
                   (eq same-queue queue))
      (expect-true :medium/queue "enqueue keeps same instance (d)"
                   (eq queue (queue-enqueue 'd queue)))
      (destructuring-bind (second same-queue-2) (queue-dequeue queue)
        (expect-equal :medium/queue "second dequeue returns b"
                      second
                      'b
                      :test #'eq)
        (expect-true :medium/queue "queue instance reused"
                     (eq same-queue-2 queue))
        (destructuring-bind (third same-queue-3) (queue-dequeue queue)
          (expect-equal :medium/queue "third dequeue returns c"
                        third
                        'c
                        :test #'eq)
          (expect-true :medium/queue "queue instance reused again"
                       (eq same-queue-3 queue))
          (destructuring-bind (fourth same-queue-4) (queue-dequeue queue)
            (expect-equal :medium/queue "fourth dequeue returns d"
                          fourth
                          'd
                          :test #'eq)
            (expect-true :medium/queue "queue instance reused after draining"
                         (eq same-queue-4 queue))
            (expect-true :medium/queue "queue accepts new data after draining"
                         (eq queue (queue-enqueue 'z queue)))
            (destructuring-bind (sentinel same-queue-5) (queue-dequeue queue)
              (expect-equal :medium/queue "sentinel dequeue returns z"
                            sentinel
                            'z
                            :test #'eq)
              (expect-true :medium/queue "queue instance reused after sentinel"
                           (eq same-queue-5 queue)))))))))

(deftasktest test-balanced-multi-brackets ()
  (expect-true :heavy/brackets "all bracket types balanced"
               (balanced-multi-brackets-p "{[()()]()}"))
  (expect-false :heavy/brackets "incorrect nesting"
                (balanced-multi-brackets-p "[(])")))

(deftasktest test-bst ()
  (let* ((values '(5 2 7 1 3 6 8))
         (tree (bst-make-empty)))
    (dolist (value values)
      (expect-true :heavy/bst "bst-insert mutates in place"
                   (eq tree (bst-insert tree value))))
    (expect-equal :heavy/bst "inorder traversal"
                  (bst-inorder tree)
                  '(1 2 3 5 6 7 8))
    (expect-true :heavy/bst "find existing value returns non-nil"
                 (not (null (bst-find tree 3))))
    (expect-false :heavy/bst "find missing value returns nil"
                  (bst-find tree 42))
    (expect-true :heavy/bst "bst-delete mutates in place"
                 (eq tree (bst-delete tree 2)))
    (expect-equal :heavy/bst "delete value"
                  (bst-inorder tree)
                  '(1 3 5 6 7 8))))

(deftasktest test-remove-triple-parentheses ()
  (expect-equal :heavy/triple-parens "simple triple"
                (remove-triple-parentheses "(((abc)))")
                "abc"
                :test #'string=)
  (expect-equal :heavy/triple-parens "longer sequence"
                (remove-triple-parentheses "(((((abc)d)))e)")
                "((abc)de)"
                :test #'string=))

(deftasktest test-run-brainfuck ()
  (flet ((run-with-io (source input)
           (let ((input-chars (copy-list (coerce input 'list)))
                 (output '()))
             (labels ((getchar ()
                        (if input-chars
                            (prog1 (car input-chars)
                              (setf input-chars (cdr input-chars)))
                            nil))
                      (putchar (ch)
                        (push ch output)))
               (run-brainfuck source #'getchar #'putchar)
               (coerce (nreverse output) 'string)))))
    (expect-equal :ultra/brainfuck "echo input"
                  (run-with-io ",." "A")
                  "A"
                  :test #'string=)
    (expect-equal :ultra/brainfuck "add two numbers"
                  (run-with-io ",>,<[->+<]>."
                               (coerce (list (code-char 2) (code-char 3)) 'string))
                  (string (code-char 5))
                  :test #'string=)))

(deftasktest test-mini-scheme-repl ()
  (labels ((run-scheme-script (script)
             (let ((input-chars (copy-list (coerce script 'list)))
                   (output '()))
               (labels ((getchar ()
                          (if input-chars
                              (prog1 (car input-chars)
                                (setf input-chars (cdr input-chars)))
                              nil))
                        (putchar (ch)
                          (push ch output)))
                 (let ((result (mini-scheme-repl #'getchar #'putchar)))
                   (values result (coerce (nreverse output) 'string)))))))
    (multiple-value-bind (result output)
        (run-scheme-script "(define (square x) (* x x))\n(square 5)\n")
      (declare (ignore result))
      (expect-true :ultra/scheme "output mentions 25"
                   (not (null (search "25" output)))))))


(progn
  (reset-test-results)
  (test-easy-sum-two-numbers)
  ; (test-easy-range-to-n)
  ; (test-easy-stats)
  ; (test-balanced-parentheses-p)
  ; (test-my-length)
  ; (test-my-reverse)
  ; (test-my-map)
  ; (test-my-filter)
  ; (test-folds)
  ; (test-split-string)
  ; (test-my-sort)
  ; (test-stack)
  ; (test-queue)
  ; (test-balanced-multi-brackets)
  ; (test-bst)
  ; (test-remove-triple-parentheses)
  ; (test-run-brainfuck)
  ; (test-mini-scheme-repl)
  (print-test-report *test-results*)
)