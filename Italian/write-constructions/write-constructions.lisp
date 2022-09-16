;;; Copyright (C) Sony Computer Science Laboratories Paris
;;;               Authors: Martina Galletti martina.galletti@sony.com
;;;                        Ines Blin ines.blin@sony.com
;;;                        Remi van Trijp (www.remivantrijp.eu)
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

;;; Prepositions
;;; ------------

(defun write-prepositions ()
  (let ((prepositions '((a "a" "alla" "alle" "agli" "al" "all'" "ai" "ad")
                        (da "da" "dalla" "dalle" "dagli" "dal" "dall'" "dai")
                        (di "di" "della" "delle" "dello" "dell'" "degli" "dei" "delle" "del"))))
    (dolist (preposition-list prepositions)
      (write-prepositional-morph-constructions preposition-list))))

(defun write-causal-fees ()
  (let ((lemmas '("perché" "poiché" "comportare" "provocare" "creare" "lasciare" 
                  "rendere" "causare" "causa")))
    (write-causal-frame-lex-constructions lemmas)))

(defun write-all-constructions ()
  (write-prepositions)
  (write-causal-fees))

; (write-all-constructions)
