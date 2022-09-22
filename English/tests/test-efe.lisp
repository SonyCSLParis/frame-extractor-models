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

(deftest test-efe-causation ()
  (let ((test-sentences (fe-read-test-data (efe-pathname "tests/test-causation-sentences.txt")))
        (test-solutions (fe-read-test-data (efe-pathname "tests/test-causation-solutions.txt"))))
    (loop for sentence in test-sentences
          for solution in test-solutions
          for extracted-frame = (first (extract-semantic-frames sentence :phrase-based
                                                                :cxn-inventory *english-frame-extractor*
                                                                :frame-dev-monitor nil))
          do (let* ((fe-solutions (cl-ppcre:split "\\|" solution)))
               (test-assert 
                (and extracted-frame
                     (string= (first fe-solutions) (second (assoc 'cause (frame-elements extracted-frame))))
                     (string= (second fe-solutions) (second (assoc 'effect (frame-elements extracted-frame))))))))))
; (test-efe-causation)
