;;; myweb - simple web server written in Common Lisp
;;;
;;; load.lisp
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

(in-package :cl-user)

(quicklisp:quickload "swank")
(quicklisp:quickload "usocket")
(quicklisp:quickload "bordeaux-threads")
(quicklisp:quickload "trivial-utf-8")
(quicklisp:quickload "cl-log")
(quicklisp:quickload "local-time")

(pushnew '*default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system 'myweb)

(swank:create-server)
