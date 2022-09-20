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

(defun read-test-data (filename)
  (let (data)
    (with-open-file (in (ife-pathname (format nil "tests/~a.txt" filename)))
      (loop for input = (read-line in nil)
            while input
            do (push input data)))
    data))

(deftest test-ife-causation ()
  (let ((test-sentences (read-test-data "test-causation-sentences"))
        (test-solutions (read-test-data "test-causation-solutions")))
    (loop for sentence in test-sentences
          for solution in test-solutions
          for extracted-frame = (first (extract-semantic-frames sentence :dependency-based
                                                                :cxn-inventory *italian-frame-extractor*
                                                                :frame-dev-monitor nil))
          do (let* ((fe-solutions (cl-ppcre:split "\\|" solution)))
               (test-assert 
                (and extracted-frame
                     (string= (first fe-solutions) (second (assoc 'fcg::cause (frame-elements extracted-frame))))
                     (string= (second fe-solutions) (second (assoc 'fcg::effect (frame-elements extracted-frame))))))))))
; (test-ife-causation)
