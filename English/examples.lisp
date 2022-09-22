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

; (ql:quickload :frame-extractor)
; (ql:quickload :efe)

(in-package :efe)

(activate-monitor trace-fcg)

(extract-semantic-frames "In his speech, he sought to ridicule evidence of growing draught and heat waves due to climate change."
                         :phrase-based
                         :cxn-inventory *english-frame-extractor*
                         :frame-dev-monitor t)