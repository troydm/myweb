;;; myweb - simple web server written in Common Lisp
;;;
;;; log.lisp
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

(in-package :cl-log)

(setf (log-manager)
          (make-instance 'log-manager :message-class 'formatted-message))

(start-messenger 'text-file-messenger :filename (merge-pathnames #p"log/web.log" myweb.config:*base-directory*))

(defmethod format-message ((self formatted-message))
  (format nil "~a ~a ~?~&"
	  (local-time:format-timestring nil 
					(local-time:universal-to-timestamp 
					 (timestamp-universal-time (message-timestamp self))))
	  (message-category self)
	  (message-description self)
	  (message-arguments self)))
