;;; myweb - simple web server written in Common Lisp
;;;
;;; web.lisp
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

(in-package :myweb)

(defvar *listen-socket* nil)
(defvar *listen-thread* nil)

(defvar *request-mutex* (make-lock "request-mutex"))
(defvar *request-threads* (list))

(defvar *worker-mutex* (make-lock "worker-mutex"))
(defvar *workers* (list))
(defvar *worker-num* 0)
(defvar *idle-workers* (list))
(defvar *idle-workers-num* 0)
(defvar *request-queue* (list))

(defun start-http (host port &key (worker-limit 10) (idle-workers 1))
  (if (not *listen-socket*)
      (setq *listen-thread* 
	    (make-thread (lambda () (http-acceptor host port worker-limit idle-workers)) :name "socket-acceptor"))
      "http server already started"))

(defun http-acceptor (host port worker-limit idle-workers)
  (setq *listen-socket* (socket-listen host port :reuse-address t :element-type '(unsigned-byte 8) :backlog (* worker-limit 2)))
  (let ((request-id 0)
	(worker-id 0))
    (loop while *listen-thread* do
	 (let* ((socket (socket-accept *listen-socket* :element-type '(unsigned-byte 8))))
	   (progn (setq request-id (1+ request-id))
		  (acquire-lock *worker-mutex*)
		  (if (>= *worker-num* worker-limit)
		      (push (cons request-id socket) *request-queue*)
		      ;; Get worker from idle workers
		      (if (> *idle-workers-num* 0)
			  (progn (push (cons request-id socket) *request-queue*)
				 (condition-notify (caar *idle-workers*)))
		      ;; Add new Worker
		      (progn (setq worker-id (1+ worker-id))
			     (setq *worker-num* (1+ *worker-num*))
			     (setq *workers* (cons (make-thread (lambda () (worker-thread request-id socket idle-workers))
						     :name (concatenate 'string "socket-worker-" (prin1-to-string worker-id))) *workers*)))))
		  (release-lock *worker-mutex*)
		  t)))))

(defun worker-thread (request-id socket idle-workers)
  (if request-id
      ;; Process request if it is not nil
      (progn 
	(with-lock-held (*request-mutex*)
	  (setq *request-threads* (cons (cons request-id (current-thread)) *request-threads*))
	  )
	(http-worker socket)
	(with-lock-held (*request-mutex*)
	  (setq *request-threads* (remove-if (lambda (x) (eq (car x) request-id)) *request-threads*))
	  )
	))
  (acquire-lock *worker-mutex*)
  (if *request-queue*
      (let ((request nil))
	(setq request (car *request-queue*))
	(setq *request-queue* (cdr *request-queue*))
	(release-lock *worker-mutex*)
	(worker-thread (car request) (cdr request) idle-workers))
      (if (< *idle-workers-num* idle-workers)
	  (let ((condition (make-condition-variable))
		(idle-lock (make-lock))
		(request nil))
	    (push (cons condition (current-thread)) *idle-workers*)
	    (setq *idle-workers-num* (1+ *idle-workers-num*))
	    (release-lock *worker-mutex*)
	    (list-workers)
	    (with-lock-held (idle-lock)
	      (condition-wait condition idle-lock)
	      )
	    (with-lock-held (*worker-mutex*)
	      (setq *idle-workers* (cdr *idle-workers*))
	      (setq *idle-workers-num* (1- *idle-workers-num*))
	      (setq request (car *request-queue*))	
	      (setq *request-queue* (cdr *request-queue*))
	      )
	    (worker-thread (car request) (cdr request) idle-workers))
	  (progn (setq *workers* (remove (current-thread) *workers*))
		 (setq *worker-num* (1- *worker-num*))
		 (release-lock *worker-mutex*)))))

(defun http-worker (socket)
  (let* ((stream (socket-stream socket))
	 (request (myweb.util:parse-request stream)))
    (myweb.handler:process-request request stream)
    (finish-output stream)
    (socket-close socket)))

(defun list-workers ()
  (with-lock-held (*worker-mutex*)
    (setq *workers*
	  (remove-if (lambda (w) (not (thread-alive-p w))) *workers*))
    (setq *worker-num* (length *workers*))
	*workers*))

(defun list-requests ()
  (with-lock-held (*request-mutex*)
    (setq *request-threads*
  	(remove-if (lambda (r) (not (thread-alive-p (cdr r)))) *request-threads*))
	*request-threads*))

(defun stop-http ()
  (if *listen-socket*
      (progn (stop-thread) 
	(socket-close *listen-socket*)
	     (setq *listen-socket* nil)
	     (setq *request-queue* nil)
	     (setq *worker-num* 0)
	     (setq *workers* nil)
	     (mapcar (lambda (i) (destroy-thread (cdr i))) *idle-workers*)
	     (setq *idle-workers-num* 0)
	     (setq *idle-workers* nil)
	     (release-lock *worker-mutex*)
	     (setq *request-threads* nil)
	     (release-lock *request-mutex*)
	     (setq *request-mutex* (make-lock "request-mutex"))
	     (setq *worker-mutex* (make-lock "worker-mutex")))))

(defun stop-thread ()
  (if (and *listen-thread* (thread-alive-p *listen-thread*))
      (destroy-thread *listen-thread*)))

      
