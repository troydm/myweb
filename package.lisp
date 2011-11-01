;;; myweb - simple web server written in Common Lisp
;;;
;;; package.lisp
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

(defpackage :myweb
  (:use :cl :usocket :bordeaux-threads)
  (:export :start-http :stop-http :stop-thread :list-workers :list-requests))

(defpackage :myweb.util
  (:use :cl :local-time :trivial-utf-8)
  (:export :parse-request :read-utf-8-string :response-write :get-param :get-header :http-response :file-response :html-template :log-info :log-warning :log-error))

(defpackage :myweb.handler
  (:use :cl)
  (:export :process-request))
