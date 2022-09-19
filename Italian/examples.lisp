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

;; First time? Create the grammar files first:
; (write-all-constructions)

;; Load the grammar:
; (load-italian-frame-extractor)

;; Activate the web interface:
; (activate-monitor trace-fcg)

(comprehend "un numero eccessivo di discendenti tale da comportare uno spezzettamento del patrimonio familiare" :cxn-inventory *italian-frame-extractor*)

(extract-semantic-frames "Le infezioni stanno diminuendo perché i vaccini sono efficienti." 
                         :phrase-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)