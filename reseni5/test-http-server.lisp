(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "http-server.lisp")
  (ql:quickload :yason))

(defparameter *port* 8000)

(defun response-for-request (request)
  (make-instance 'http-server:response
                 :content-type "application/json"
                 :content-string
                 (unless (string= (http-server:request-method request) "HEAD")
                   (with-output-to-string (output)
                     (yason:encode-alist
                      `(("protocol" . ,(http-server:request-protocol request))
                        ("method" . ,(http-server:request-method request))
                        ("path" . ,(http-server:request-path request))
                        ("headers" . ,(http-server:request-headers request))
                        ("contentText" . ,(http-server:request-content-string
                                           request))
                        ("contentBytes" . ,(http-server:request-content-bytes
                                            request)))
                      (yason:make-json-output-stream output))))))

(defvar *server*
  (make-instance 'http-server:http-server
                 :port *port*
                 :response-for-request #'response-for-request
                 :bad-request-response
                 (make-instance 'http-server:response
                                :status 400
                                :comment "Bad Request"
                                :content-type "application/json"
                                :content-string
                                (with-output-to-string (output)
                                  (yason:encode-alist
                                   '(("error" . "400 Bad Request")
                                     ("description" .
                                      "The server wasn't able to parse your request."))
                                   (yason:make-json-output-stream output))))
                 :internal-error-response
                 (make-instance 'http-server:response
                                :status 500
                                :comment "Internal Server Error"
                                :content-type "application/json"
                                :content-string
                                (with-output-to-string (output)
                                  (yason:encode-alist
                                   '(("error" . "500 Internal Server Error")
                                     ("description" .
                                      "An error occured while processing request."))
                                   (yason:make-json-output-stream output))))))

; Z REPLu použij (http-server:start-server *server*), který spustí server na
; novém vlákně a (http-server:stop-server *server*), který jej zastaví.
; Nebo odkomentuj následující řádky a spusť tento soubor přímo.

#+nil
(progn
  (http-server:start-server *server*)

  (format t "Running HTTP server on port ~a.~%" *port*)
  (format t "Press return to stop.~%")
  (read-line)

  (http-server:stop-server *server*))
