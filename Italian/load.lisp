;;; Copyright (C) Sony Computer Science Laboratories Paris
;;;               Authors: Martina Galletti martina.galletti@sony.com
;;;                        Ines Blin ines.blin@sony.com
;;;                        Remi van Trijp (www.remivantrijp.eu)
;;;                        
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
;;;
;;; Grammar is written and loaded at the end of this file.
;;; ----------------------------------------------------------------------------

(in-package :ife)

;; Helper functions:
(defun lisp-p (pathname)
  (string= "lisp" (pathname-type pathname)))

;; For loading:
(defun load-lisp-files (path)
  (dolist (pathname (directory (ife-pathname path)))
    (if (lisp-p pathname)
      (load pathname))))

(defun load-prepostions ()
  (load-lisp-files "constructions/prepositions/"))
;; (load-prepostions)

(defun load-frame-evoking-elements ()
  (load-lisp-files "constructions/frame-evoking-elements/"))
;; (load-frame-evoking-elements)

(defun load-grammar ()
  (load-lisp-files "constructions/"))

(defun load-italian-frame-extractor ()
  (make-italian-frame-extractor-cxns)
  (load-prepostions)
  (load-frame-evoking-elements)
  (load-grammar)
  *italian-frame-extractor*)

;; Writing and loading:
(progn
  (write-all-constructions)
  (load-italian-frame-extractor))
