;;; myweb - simple web server written in Common Lisp
;;;
;;; handler.lisp
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

(in-package :myweb.handler)

(defun process-request (request stream)
  (let ((path (caadr request)))
    (cond
      ((equal path "/logo.jpg") (myweb.util:file-response "logo.jpg" "image/jpeg" request stream))
      (t 
       (process-index request stream)))))

(defun process-index (request stream)
  (let ((name (myweb.util:get-param "name" request)))
    (if (and name (> (length name) 0))
	(myweb.util:html-template "index.html" "text/html;encoding=UTF-8" `(("name" . ,name)) request stream)
	(myweb.util:html-template "name.html" "text/html;encoding=UTF-8" nil request stream)
      )))
