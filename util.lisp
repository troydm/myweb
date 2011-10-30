;;; myweb - simple web server written in Common Lisp
;;;
;;; util.lisp
;;;
;;; Author: Dmitry Geurkov <dmitry_627@mail.ru>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.

(in-package :myweb.util)

(defvar *new-line* (concatenate 'string (string #\Return) (string #\Newline)))

(defun parse-request (stream)
  (let ((header (read-utf-8-string stream 10)))
    (if (eq (length header) 0)
	'()
	(if (equal (subseq header 0 4) "POST")
	    (parse-post-header header stream)
	    (parse-get-header header stream)))))

(defun read-utf-8-string (stream &optional (end 0))
  (let ((byte -1)
	(buffer (make-array 1 :fill-pointer 0 :adjustable t)))
    (handler-case 
	(loop do
	     (setq byte (read-byte stream))
	     (if (/= byte end) (vector-push-extend byte buffer))
	   while (/= byte end))
      (end-of-file ()))
    (trivial-utf-8:utf-8-bytes-to-string buffer)))

(defun response-write (text stream)
  (trivial-utf-8:write-utf-8-bytes text stream))

(defun parse-get-header (header stream)
  (cons "GET" 
	(cons (parse-path (subseq header (position #\/ header) (position #\Space header :from-end t)))
	      (parse-headers stream))))

(defun parse-headers (stream)
  (let ((headers nil)
	(header nil))
    (loop do
	 (setq header (read-utf-8-string stream 10))
	 (if (> (length header) 2) (setq headers (cons (parse-header header) headers)))
	 while (> (length header) 2))
    (reverse headers)))

(defun parse-header (header)
  (let ((pos (position #\: header)))
    (if pos (cons (string-downcase (subseq header 0 pos)) (string-trim (concatenate 'string (string #\Space) (string #\Return)) (subseq header (1+ pos)))))))

(defun parse-post-header (header stream)
  (cons "POST" nil))

(defun file-response (filename type request stream)
  (handler-case
      (with-open-file (in (concatenate 'string "web/" filename) :element-type '(unsigned-byte 8))
	(if (equal (get-header "if-modified-since" request) (format-timestring nil (universal-to-timestamp (file-write-date in)) :format +asctime-format+))
	    (http-response "304 Not Modified" nil stream)
	(progn 
	  (http-response "200 OK" 
			 (cons
			  (cons "Last-Modified" (format-timestring nil (universal-to-timestamp (file-write-date in)) :format +asctime-format+))
			  (cons (cons "Content-Type" type) nil))
			 stream)
	  (let ((buf (make-array 4096 :element-type (stream-element-type in))))
	    (loop for pos = (read-sequence buf in)
	       while (plusp pos)
	       do (write-sequence buf stream :end pos)))	 
	)))
    (file-error () 
      (http-404-not-found "404 File Not Found" stream)
      )))

(defun html-template (filename type params request stream)
  (handler-case
      (with-open-file (in (concatenate 'string "web/" filename) :element-type '(unsigned-byte 8))
	(loop for line = (read-utf-8-string in 10)
	   while (and line (> (length line) 0))  
	   do (progn
		(mapcar (lambda (i)
			  (let* ((key (concatenate 'string "${" (car i) "}")))
			    (loop for pos = (search key line)
				 while pos
			       do 
				 (setq line 
				       (concatenate 'string 
						    (subseq line 0 pos) (cdr i) 
						    (subseq line (+ pos (length key)))))
				 )
			  )) params)
		(response-write line stream)
		(response-write (string #\Return) stream))
	   )
	)
    (file-error () 
      (http-404-not-found "404 File Not Found" stream)
      )))

(defun http-response (code headers stream)
  (response-write (concatenate 'string "HTTP/1.1 " code *new-line*)  stream)
  (mapcar (lambda (header)
	    (response-write 
	     (concatenate 'string (car header) ": " (cdr header) *new-line*) stream)) headers)
  (response-write *new-line* stream))

(defun http-404-not-found (message stream)
  (http-response "404 Not Found" nil stream)
  (response-write message stream))	 

(defun parse-path (path)
  (if (position #\? path)
      (cons (subseq path 0 (position #\? path)) (parse-params (subseq path (1+ (position #\? path)))))
      (cons path nil)))

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer (coerce (list c1 c2) 'string) :radix 16 :junk-allowed t)))
    (if code
	(code-char code)
	default)))

(defun decode-param (s)
  (labels ((f (1st)
	     (when 1st
	       (case (car 1st)
		 (#\% (cons (http-char (cadr 1st) (caddr 1st))
			    (f (cdddr 1st))))
		 (#\+ (cons #\Space (f (cdr 1st))))
		 (otherwise (cons (car 1st) (f (cdr 1st))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun decode-kv (s)
  (let ((p1 (position #\= s)))
    (if p1 (cons (decode-param (subseq s 0 p1)) (decode-param (subseq s (1+ p1))))
	     (cons (decode-param s) nil))))

(defun decode-params (s)
  (let ((p1 (position #\& s)))
    (if p1 (cons (decode-kv (subseq s 0 p1)) (parse-params (subseq s (1+ p1))))
	(list (decode-kv s)))))

(defun parse-params (s)
  (let ((params (decode-params s)))
    (remove-duplicates params :test (lambda (x1 x2) (equal (car x1) (car x2))) :from-end nil)))

(defun get-param (name request)
  (cdr (assoc name (cdadr request) :test #'equal)))

(defun get-header (name request)
  (cdr (assoc (string-downcase name) (cddr request) :test #'equal)))

(defvar *log-queue-lock* (bt:make-lock))
(defvar *log-queue-cond* (bt:make-condition-variable))
(defvar *log-queue-cond-lock* (bt:make-lock))
(defvar *log-queue* nil)
(defvar *log-queue-time* (get-universal-time))

(defun log-worker ()
  (bt:with-lock-held (*log-queue-lock*)
    (progn 
      (mapcar (lambda (i) (if (cdr i) (cl-log:log-message (car i) (cdr i)))) (reverse *log-queue*))
      (setq *log-queue* nil)
      ))
  (bt:with-lock-held (*log-queue-cond-lock*)
    (bt:condition-wait *log-queue-cond* *log-queue-cond-lock*)
    )
  (log-worker))

(bt:make-thread #'log-worker :name "log-worker")

(defun log-info (message)
  (bt:with-lock-held (*log-queue-lock*)
    (progn 
      (push (cons :info message) *log-queue*)
      (if (> (- (get-universal-time) *log-queue-time*) 0)
	  (bt:condition-notify *log-queue-cond*))
      )))

(defun log-warning (message)
  (bt:with-lock-held (*log-queue-lock*)
    (progn 
      (push (cons :warning message) *log-queue*)
      (if (> (- (get-universal-time) *log-queue-time*) 0)
	  (bt:condition-notify *log-queue-cond*))
      )))

(defun log-error (message)
  (bt:with-lock-held (*log-queue-lock*)
    (progn 
      (push (cons :error message) *log-queue*)
      (if (> (- (get-universal-time) *log-queue-time*) 0)
	  (bt:condition-notify *log-queue-cond*))
      )))
