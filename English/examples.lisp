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

;; (ql:quickload :frame-extractor)
;; (ql:quickload :efe)

(in-package :efe)

(activate-monitor trace-fcg)

(extract-english-semantic-frames "This will probably result in much more of an emphasis on countering disinformation and lead to much more intelligence gathering in collaboration with Europeans.")

;; TODDO: handle semantics of double-verb-construction
(extract-english-semantic-frames "Current Coalition Direct Action climate policies are projected to result in increased greenhouse gas emissions, albeit at a level that might allow Australia to meet its 2020 target because of accounting rules.")