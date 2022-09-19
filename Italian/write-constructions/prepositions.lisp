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

(in-package :ife)

(defun write-prepositional-morph-constructions (preposition-list 
                                                &optional (cxn-inventory '*italian-frame-extractor*))
  "Write a file with all constructions for a preposition."
  (let* ((lex-id (first preposition-list))
         (lst (rest preposition-list))
         (pathname (format nil "constructions/prepositions/~(~a~).lisp" lex-id)))
    (with-open-file (out (ensure-directories-exist 
                          (ife-pathname pathname))
                         :direction :output :if-exists :supersede)
      (ife-license-and-copyright-header out)
      (format out "~%~%(in-package :ife)")
      (dolist (string lst)
        (let* ((safestring (safestring string)) 
               (name (format nil "~(~a~)-morph-cxn" safestring))
               (unit-name (format nil "?~(~a~)-word" safestring)))
          (format out "~%~%(def-fcg-cxn ~a" name)
          (format out "~%             ((~(~a~)" unit-name)
          (format out "~%               (lex-id ~a))" lex-id)
          (format out "~%              <-")
          (format out "~%              (~(~a~)" unit-name)
          (format out "~%               --")
          (format out "~%               (hash form ((string ~(~a~) ~s)))" unit-name string)
          (format out "~%               (syn-cat (lex-class preposition))))")
          (format out "~%             :cxn-inventory ~(~a~)" cxn-inventory)
          (format out "~%             :attributes (:string ~s)" string)
          (format out "~%             :cxn-set morph)"))))))
