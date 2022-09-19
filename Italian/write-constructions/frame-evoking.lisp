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

(defun write-causal-frame-lex-constructions (word-list 
                                             &optional (cxn-inventory '*italian-frame-extractor*))
  (with-open-file (out (ensure-directories-exist
                        (ife-pathname "constructions/frame-evoking-elements/causation.lisp"))
                       :direction :output :if-exists :supersede)
    (ife-license-and-copyright-header out)
    (format out "~%~%(in-package :ife)")
    (dolist (word word-list)
      (let* ((lex-id (safestring word))
             (name (format nil "~(~a~)-lex-cxn" lex-id))
             (unit-name (format nil "?~(~a~)-word" lex-id))
             (pathname (ife-pathname (format nil "constructions/frame-evoking-elements/~a.lisp" lex-id))))
        (format out "~%~%(def-fcg-cxn ~a" name)
        (format out "~%             ((~a" unit-name)
        (format out "~%               (sem-frame (causation)))")
        (format out "~%               <-")
        (format out "~%               (~a" unit-name)
        (format out "~%                --")
        (format out "~%                (syn-cat (lemma ~s))" lex-id)
        (format out "~%                (hash form ((string ~a ?surface-form)))))" unit-name)
        (format out "~%             :cxn-inventory ~(~a~)" cxn-inventory)
        (format out "~%             :attributes (:lex-id ~a)" lex-id)
        (format out "~%             :cxn-set lex)")))))

