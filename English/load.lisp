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

(in-package :efe)

(defun extract-english-semantic-frames (utterance
                                        &key (mode :phrase-based)
                                        (cxn-inventory *english-frame-extractor*)
                                        (frame-dev-monitor t))
  "Convenience function for calling extract-semantic-frames."
  (extract-semantic-frames utterance mode
                           :cxn-inventory cxn-inventory
                           :frame-dev-monitor frame-dev-monitor))

(defun load-english-frame-extractor ()
  (setf *english-frame-extractor* (fcg::make-english-base-model-cxns))
  (activate-frame-extractor :cxn-inventory *english-frame-extractor*)
  (load (efe-pathname "constructions/frame-evoking-elements/causation.lisp"))
  (load (efe-pathname "constructions/grammar-causation.lisp")))
;; (load-english-frame-extractor)
