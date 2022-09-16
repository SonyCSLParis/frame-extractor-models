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

(defun ife-license-and-copyright-header (stream)
  (let ((lines '(";;; Copyright (C) Sony Computer Science Laboratories Paris"
                 "~%;;;               Authors: Martina Galletti martina.galletti@sony.com"
                 "~%;;;                        Ines Blin ines.blin@sony.com"
                 "~%;;;                        Remi van Trijp (www.remivantrijp.eu)"
                 "~%;;;"
                 "~%;;;     This program is free software: you can redistribute it and/or modify"
                 "~%;;;     it under the terms of the GNU General Public License as published by"
                 "~%;;;     This program is distributed in the hope that it will be useful,"
                 "~%;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of"
                 "~%;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
                 "~%;;;     GNU General Public License for more details."
                 "~%;;; "
                 "~%;;;     You should have received a copy of the GNU General Public License"
                 "~%;;;     along with this program.  If not, see <https://www.gnu.org/licenses/>."
                 "~%;;; ----------------------------------------------------------------------------")))
    (dolist (line lines)
      (format stream line))))

(defun safestring (string)
  (remove "'" string :test #'string=))
