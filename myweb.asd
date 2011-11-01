;;; myweb - simple web server written in Common Lisp
;;;
;;; myweb.asd
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


;; myweb.asd

(asdf:defsystem #:myweb
  :depends-on ("usocket" "bordeaux-threads" "local-time" "cl-log" "trivial-utf-8")
  :serial t
  :components ((:file "package")
	       (:file "log")
               (:file "util")
	       (:file "web")
	       (:file "handler")))
