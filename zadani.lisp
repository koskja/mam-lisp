;;;; Zadání úloh pro témátko Lisp

(in-package #:cl-user)


;; Funkce s předponou easy jsou zjednodušené verze, které neinteragují s uživatelem. 
;; Pokud implementujete POUZE funkci s předponou easy, dostanete za ni 0.5 bodu.
;; Pokud implementujete ALESPOŇ funkci bez předpony easy (tedy variantu interagující s uživatelem), dostanete za ni plný 1 bod.
;; Pokud implementujete variantu s předponou easy a zároveň variantu bez předpony easy, dostanete za ně plný 1 bod.

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

(defun sum-two-numbers ()
  ;; TODO: načtěte vstup od uživatele a vypište součet.
  nil)

(defun range-to-n ()
  ;; TODO: načtěte vstup od uživatele a vypište čísla 1 až n.
  nil)

(defun stats ()
  ;; TODO: načtěte vstup od uživatele a vypište průměr, minimum a maximum.
  nil)

(defparameter *leva-zavorka* #\( )
(defparameter *prava-zavorka* #\) )

(defun je-zavorka-p (znak)
  (or (eq znak *leva-zavorka*) (eq znak *prava-zavorka*)))

(defun balanced-parentheses-p (text)
  ;; TODO: vraťte T pokud je text vyvážený, jinak NIL.
  nil)

;; (balanced-parentheses-p "(()())") ; => T
;; (balanced-parentheses-p "(()") ; => NIL

(defun split-string (string delimiter)
  ;; TODO: vraťte seznam podřetězců oddělených daným oddělovačem. Oddělovač je jeden znak. 
  '())

;; (split-string "a,bb,c," ",") ; => '("a" "b" "c" "")

(defun my-sort (sequence compare-fn)
  ;; TODO: vraťte sekvenci setříděnou podle porovnávací funkce compare-fn. 
  ;; Abyste porovnali dva prvky x a y, zavolejte (funcall compare-fn x y).
  ;; Prvky seřaďte tak, aby porovnávací funkce vrátila T pro každé dva různé sousední prvky. 
  ;; To znamená, že pokud je compare-fn operátor <, prvky seřaďte vzestupně, pokud je operátor >, prvky seřaďte sestupně.
  ;; Detaily přesného chování předávání funkcí jako parametru se budeme věnovat v budoucnu.
  sequence)

;; (my-sort '(3 1 4 1 5 9) #'<) ; => '(1 1 3 4 5 9)

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

(defun balanced-multi-brackets-p (text)
  ;; TODO: vraťte T pouze pokud je řetězec vyvážený.
  nil)

;; (balanced-multi-brackets-p "{[()()]([]{[()[]]})}") ; => T
;; (balanced-multi-brackets-p "{[(])}") ; => NIL

;; Poskytujeme vám test na interpreter Brainfucku.
(defun run-brainfuck (source getchar putchar)
  "Napište interpretr pro Brainfuck. source = BF program, getchar = funkce pro čtení znaku, putchar = funkce pro zápis znaku.
Výstupem může být řetězec nebo struktura s detaily běhu."
  ;; TODO: vykonejte Brainfuck program. Výstup zaznamenejte průběžně pomocí putchar.
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

(progn
  (reset-test-results)
  (test-easy-sum-two-numbers)
  ; (test-easy-range-to-n)
  ; (test-easy-stats)
  ; (test-balanced-parentheses-p)
  ; (test-split-string)
  ; (test-my-sort)
  ; (test-stack)
  ; (test-queue)
  ; (test-balanced-multi-brackets)
  ; (test-run-brainfuck)
  (print-test-report *test-results*)
)