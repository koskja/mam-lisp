(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :usocket)
  (ql:quickload :bordeaux-threads)
  (ql:quickload :trivial-utf-8))

(defpackage :http-server
  (:use :cl)
  (:export #:tcp-server
           #:simple-tcp-server
           #:http-server
           #:request
           #:request-headers
           #:request-protocol
           #:request-method
           #:request-path
           #:request-content-bytes
           #:request-content-string
           #:response
           #:run-server
           #:start-server
           #:stop-server
           #:server-running
           #:server-response-for-request
           #:server-in-debug-mode
           #:server-error-logs
           #:error-log
           #:logged-error
           #:logged-request
           #:bad-request-response
           #:internal-error-response
           #:*default-400-response*
           #:*default-500-response*))

(in-package :http-server)

; Obecnější utility:

(defun split-str-to-n-parts (string delimeter-char &optional (n 2))
  "Rozdělí řetězec na `n' částí, které vrátí jako více hodnot."
  (when (< n 1)
    (error (format nil "Part number must be a positive integer, not ~a." n)))
  (if (> n 1)
      (loop for i from 0 to (- (length string) 1)
            when (char= (char string i) delimeter-char)
              return
              (values-list (cons (subseq string 0 i)
                                 (multiple-value-list
                                  (split-str-to-n-parts (subseq string (+ i 1))
                                                        delimeter-char
                                                        (- n 1))))))
      string))

(defmacro dict (args (&optional inherit preserve) &body key-value-pairs)
  "Vytvoří novou hash table se zadanými prvky.

`args' jsou argumenty předané funkci `make-hash-table'. Je-li zadána hash table
`inherit' zdědí nová hash table jají prvky. Není-li zadáno `preserve' `t', budou
přepsány novými.
Příklad:
(dict (:test 'equal) (another-table t)
  (\"a\" 1)
  (\"b\" 2))"
  (let ((table (gensym))
        (inherit-symbol (gensym))
        (preserve-symbol (gensym)))
    `(let ((,table (make-hash-table ,@args))
           ,@(when inherit
               `((,inherit-symbol ,inherit)))
           ,@(when preserve
               `((,preserve-symbol ,preserve))))
       ,@(loop
           for pair in key-value-pairs
           collect `(setf (gethash ,(first pair) ,table) ,(second pair)))
       ,(when inherit
          `(when ,inherit-symbol
             (loop for k being the hash-key using (hash-value v) of ,inherit-symbol
                   when ,(if preserve
                             `(or ,preserve-symbol
                                  (not (nth-value 1 (gethash k ,table))))
                             `(not (nth-value 1 (gethash k ,table))))
                     do (setf (gethash k ,table) v))))
       ,table)))


; Server

(defclass tcp-server ()
  ((port
    :initarg :port
    :accessor server-port)
   (running
    :initform nil
    :accessor server-running
    :documentation
    "Určuje, má-li server běžet.

Při startu je nastaven na `t'. Je-li nastaven na `nil', server skončí jakmile to
bude možné.")
   (wait-time
    :initarg :wait-time
    :initform 0.5
    :accessor server-wait-time
    :documentation
    "Časová prodleva mezi kontrolami slotu `running' v sekundách.")
   (name
    :initarg :name
    :initform nil
    :accessor server-name)
   (thread
    :initform nil
    :accessor server-thread
    :documentation
    "Vlákno, na kterém server běží, nebo poslední, na němž běžel.")))

(defgeneric handle-connection-for (server socket)
  (:documentation "Metoda volaná pro každé TCP spojení."))
(defgeneric server-description (server))
(defgeneric server-stream-elem-type (server))

(defmethod server-description ((server tcp-server))
  (let ((port (server-port server))
        (name (server-name server)))
    (concatenate 'string
                 "TCP server"
                 (when name " \"") name (when name "\"")
                 (when port " on port ") (write-to-string port))))

(defmethod server-stream-elem-type ((server tcp-server)) 'character)

(defun run-server (server)
  "Vytvoří TCP server, který běží dokud není slot `running' nastaven na `nil'."
  (setf (server-thread server) (bt:current-thread))
  (let* (
         (socket (usocket:socket-listen "127.0.0.1" (server-port server))))
    (unwind-protect
         (progn
           (setf (server-running server) t)
           (loop while (server-running server) do
             (when (usocket:wait-for-input socket :timeout (server-wait-time server) :ready-only t)
               (let ((connection
                       (usocket:socket-accept socket
                                              :element-type (server-stream-elem-type server))))
                 (bt:make-thread
                  (lambda ()
                    (unwind-protect
                         (handle-connection-for server connection)
                      (usocket:socket-close connection)))
                  :name (format nil "Connection handler for ~a" (server-description server)))))))
      (usocket:socket-close socket)
      (setf (server-running server) nil))))

(defun start-server (server)
  "Spustí `run-server' v novém vlákně."
  (bt:make-thread (lambda () (run-server server))
                  :name (server-description server)))

(defun stop-server (server &optional (join t))
  "Zastaví server a je-li `join' `t', počká až se vlákno serveru ukončí."
  (setf (server-running server) nil)
  (when join
    (bt:join-thread (server-thread server))))

; Jednoduchý TCP server

(defclass simple-tcp-server (tcp-server)
  ((connection-handler
    :initarg :connection-handler
    :accessor connection-handler-for
    :documentation
    "Funkce volaná pro každé spojení, (jako argument bere usocket socket)")))

(defmethod handle-connection-for ((server simple-tcp-server) socket)
  (funcall (connection-handler-for server) socket))

; HTTP server

(defclass http-server (tcp-server)
  ((response-for-request
    :initarg :response-for-request
    :accessor server-response-for-request
    :documentation
    "Funkce, která dostane objekt třídy `request' a vrátí objekt `response'.")

   (debug-mode
    :initarg :debug-mode
    :initform nil
    :accessor server-in-debug-mode
    :documentation
    "V debug módu se nebudou chyby ignorovat, ukládat a vracet odpověď 500.

Je-li `nil', v případě chyby při volání slotu `response-for-request' se chyba
spolu s požadavkem, který ji vyvolal, uloží do slotu `error-logs' a jako odpověď
se pošle `*default-500-response*'.")

   (error-logs
    :initarg :error-logs
    :initform nil
    :accessor server-error-logs
    :documentation
    "Seznam zachycenýc chyb a požadavků, které k nim vedly.

Seznam objektů třídy `error-log'.")
   (bad-request-response
    :initarg :bad-request-response
    :initform nil
    :accessor bad-request-response
    :documentation "Odpověď poslaná v případě, že nelze naparsovat požadavek.

Objekt třídy `response'. Není-li zadána, použije se `*default-400-response*'.")
   (internal-error-response
    :initarg :internal-error-response
    :initform nil
    :accessor internal-error-response
    :documentation "Odpověď poslaná při zachycení chyby při zpracování požadavku.

Objekt třídy `response'. Není-li zadána, použije se `*default-500-response*'.")))

(defclass request ()
  ((method
    :initarg :method
    :accessor request-method
    :documentation "HTTP metoda požadavku jako řetězec (GET, POST ...).")
   (path
    :initarg :path
    :accessor request-path)
   (protocol
    :initarg :protocol
    :accessor request-protocol
    :documentation "Například \"HTTP/1.0\".")
   (headers
    :initarg :headers
    :accessor request-headers
    :documentation "Hash table hlaviček (název i hodnota jako řetězec).")
   (content-bytes
    :initarg :content-bytes
    :accessor request-content-bytes
    :documentation "Vektor bytů těla požadavku."))
   (:documentation "Reprezentuje příchozí HTTP požadavek."))

(defclass response ()
  ((status
    :initarg :status
    :initform 200
    :accessor response-status
    :documentation "Stavový kód odpovědi (jako integer).")
   (comment
    :initarg :comment
    :initform "OK"
    :accessor response-comment
    :documentation "Popis stavu, například \"OK\" nebo \"Bad Request\".")
   (content-bytes
    :initarg :content-bytes
    :initform nil
    :documentation
    "Vektor bytů těla požadavku. Není-li zadán, vytvoří se z `content-string'.")
   (content-string
    :initarg :content-string
    :initform nil
    :accessor response-content-string
    :documentation "Text těla požadavku. Ignorován je-li zadán `content-bytes'.")
   (headers
    :initarg :headers
    :initform nil
    :accessor response-headers
    :documentation "Hash table hlaviček požadavku.

Hlavičky Content-Length, Content-Type a Connection se přidají automaticky.")
   (content-type
    :initarg :content-type
    :initform "text/plain"
    :accessor response-content-type
    :documentation "Hodnota hlavičky Content-Type.")))

(defclass error-log ()
  ((error
    :initarg :error
    :accessor logged-error)
   (request
    :initarg :request
    :accessor logged-request)))

(defparameter *default-400-response*
  (make-instance 'response
                 :status 400
                 :comment "Bad Request"
                 :content-string "400 Bad Request"))

(defparameter *default-500-response*
  (make-instance 'response
                 :status 500
                 :comment "Internal Server Error"
                 :content-string "500 Internal Server Error"))

(defmethod server-stream-elem-type ((server http-server)) '(unsigned-byte 8))

(defun get-server-response (server request)
  "Zavolá slot `response-for-request' serveru a zkontroluje typ vrácené hodnoty."
  (let ((response (funcall (server-response-for-request server) request)))
    (check-type response response)
    response))

(defmethod handle-connection-for ((server http-server) socket)
  (let* ((stream (usocket:socket-stream socket))
         (request (handler-case ; Klient ukončil požadavek předčasně.
                      (parse-request stream)
                    (end-of-file ()
                      (return-from handle-connection-for nil)))))
    (if request
        (respond stream
                 (if (server-in-debug-mode server)
                     (get-server-response server request)
                     (handler-case (get-server-response server request)
                       (error (c)
                         (error-log-for-server server
                                               (make-instance 'error-log
                                                              :error c
                                                              :request request))
                         (or (internal-error-response server)
                             *default-500-response*)))))
        (respond stream
                 (or (bad-request-response server) *default-400-response*)))))

(defmethod server-description ((server http-server))
  (let ((port (server-port server))
        (name (server-name server)))
    (concatenate 'string
                 "HTTP server"
                 (when name " \"")
                 name
                 (when name "\"")
                 (when port " on port ")
                 (write-to-string port))))

(defun request-content-string (request)
  "Získá text těla požadavku převedením z bytů."
  (trivial-utf-8:utf-8-bytes-to-string (request-content-bytes request)))

(defun response-content-bytes (response)
  "Vrátí vektor bytů z serveru, případně převede z řetězce nebo vrátí `nil'."
  (let ((maybe-content-bytes (slot-value response 'content-bytes)))
    (if maybe-content-bytes
        maybe-content-bytes
        (let ((maybe-content-string (response-content-string response)))
          (when maybe-content-string
            (trivial-utf-8:string-to-utf-8-bytes
             (response-content-string response)))))))

(defun error-log-for-server (server error-log)
  "Přidá objekt do seznamu logů serveru (slotu `error-logs')"
  (push error-log (server-error-logs server)))

; Menší funkce pro HTTP:

(defun read-crlf-line-from-bytes (byte-stream)
  (let ((line ""))
    (loop for char = (code-char (read-byte byte-stream))
          until (char= char #\Return)
          do (setf line (concatenate 'string line (string char))))
    (read-byte byte-stream)
    line))

(defun read-request-body (byte-stream content-length)
  (let ((buffer (make-array content-length :element-type '(unsigned-byte 8))))
    (read-sequence buffer byte-stream)
    buffer))

(defun parse-header (line)
  ; Vrátí dvě hodnoty, jméno a hodnotu hlavičky.
  (multiple-value-bind (name value) (split-str-to-n-parts line #\:)
    (when (>= (length value) 1)
      (values name (subseq value 1)))))

(defun http-line-bytes (stream line)
  (write-sequence (trivial-utf-8:string-to-utf-8-bytes (format nil
                                                               "~a~c~c"
                                                               line
                                                               #\Return
                                                               #\Linefeed))
                  stream))

(defun http-line-bytes-format (stream &rest rest-args)
  (http-line-bytes stream (apply #'format (cons nil rest-args))))

(defun send-header (stream name value)
  (http-line-bytes-format stream "~a: ~a" name value))

(defun send-headers (stream headers)
  (loop for name being the hash-key using (hash-value value) of headers
        do (send-header stream name value)))

; Funkce pro zpracování požadavku a odpovědi.

(defun parse-request (stream)
  (multiple-value-bind (method path protocol)
      (split-str-to-n-parts (read-crlf-line-from-bytes stream) #\  3)
    (unless protocol
      (return-from parse-request nil))
    (let ((headers (make-hash-table :test 'equal)))
      (loop for line = (read-crlf-line-from-bytes stream)
            until (string= line "") do
              (multiple-value-bind (name value) (parse-header line)
                (unless name
                  (return-from parse-request nil))
                (setf (gethash name headers) value)))
      (let ((content-length-str (gethash "Content-Length" headers)))
        (make-instance 'request
                       :method method
                       :path path
                       :protocol protocol
                       :headers headers
                       :content-bytes (when content-length-str
                                        (read-request-body
                                         stream
                                         (parse-integer
                                          content-length-str))))))))

(defun respond (stream response)
  (let* ((content-bytes (response-content-bytes response))
         (all-headers
           (dict (:test 'equal) ((response-headers response) t)
             ("Connection" "close")
             ("Content-Length" (length content-bytes))
             ("Content-Type" (response-content-type response)))))
    (http-line-bytes-format stream "HTTP/1.0 ~a ~a"
                            (response-status response)
                            (response-comment response))
    (send-headers stream all-headers)
    (http-line-bytes stream "")
    (write-sequence content-bytes stream)
    (force-output stream)))
