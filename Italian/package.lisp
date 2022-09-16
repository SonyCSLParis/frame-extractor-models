;;; Copyright (C) Sony Computer Science Laboratories Paris
;;;               Authors: Martina Galletti martina.galletti@sony.com
;;;                        Ines Blin ines.blin@sony.com
;;;                        Remi van Trijp (www.remivantrijp.eu)
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

(export '(ife-pathname *ife-path*))

(defparameter *ife-path*
  (make-pathname :directory (pathname-directory (or *load-truename*
						    *compile-file-truename*))))

(defun ife-pathname (string)
  "Generates a pathname for a file relative to the frame extractor's root directory."
  (merge-pathnames string
                   *ife-path*))

(defpackage :ife
  (:use :common-lisp :common-lisp-user
   :utils
   :test-framework
   :web-interface
   :monitors
   :nlp-tools
   :fcg)
  (:import-from :common-lisp-user ife-pathname)
  (:documentation "This package contains the main grammar for semantic frames from Italian."))
