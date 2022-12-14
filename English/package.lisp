;;; Copyright (C) Sony Computer Science Laboratories Paris
;;;               Author: Remi van Trijp (www.remivantrijp.eu)
;;;
;;;     This program is free software: you can redistribute it and/or modify
;;;     it under the terms of the GNU General Public License as published by
;;;     the Free Software Foundation, version 3 of the License.
;;; 
;;;     This program is distributed in the hope that it will be useful,
;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;     GNU General Public License for more details.
;;; 
;;;     You should have received a copy of the GNU General Public License
;;;     along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------------

(in-package :common-lisp-user)

(export '(efe-pathname *efe-path*))

(defparameter *efe-path*
  (make-pathname :directory (pathname-directory (or *load-truename*
						    *compile-file-truename*))))

(defun efe-pathname (string)
  "Generates a pathname for a file relative to the frame extractor's root directory."
  (merge-pathnames string
                   *efe-path*))

(defpackage :efe
  (:use :common-lisp :common-lisp-user
   :utils
   :test-framework
   :web-interface
   :monitors
   :nlp-tools
   :fcg)
  (:import-from :common-lisp-user efe-pathname)
  (:import-from :fcg morph lex cxn)
  (:documentation "This package contains the main grammar for semantic frames from English."))