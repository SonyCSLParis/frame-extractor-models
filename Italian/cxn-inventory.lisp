;;; Copyright (C) Sony Computer Science Laboratories Paris
;;;               Authors: Martina Galletti martina.galletti@sony.com
;;;                        Ines Blin ines.blin@sony.com
;;;                        Remi van Trijp (www.remivantrijp.eu)
;;;
;;;     This program is free software: you can redistribute it and/or modify
;;;     it under the terms of the GNU General Public License as published by
;;;     This program is distributed in the hope that it will be useful,
;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;     GNU General Public License for more details.
;;; 
;;;     You should have received a copy of the GNU General Public License
;;;     along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------------

(in-package :ife)

(defvar *italian-frame-extractor* nil "Global variable for the Italian frame extractor.")

(def-fcg-constructions italian-frame-extractor
  :cxn-inventory *italian-frame-extractor*
  :cxn-inventory-type hashed-fcg-construction-set
  :feature-types ((dependents set)
                  (footprints set)
                  (boundaries set-of-predicates)
                  (form set-of-predicates)
                  (meaning set-of-predicates))
  :fcg-configurations (;; ----------------------------------------------------------------------------------
                       ;; Tagsets
                       ;; ----------------------------------------------------------------------------------

                       ;; Render and De-rendering
                       ;; ----------------------------------------------------------------------------------
                       (:de-render-mode . :italian-hybrid)
                       ;; ----------------------------------------------------------------------------------
                       ;; Form predicates
                       ;; ----------------------------------------------------------------------------------
                       (:form-predicates meets)
                       ;; ----------------------------------------------------------------------------------
                       ;; Construction Sets
                       ;; ----------------------------------------------------------------------------------
                       (:production-order hashed-meaning cxn hashed-lex-id)
                       (:parse-order morph lex-id cxn)
                       (:hashed-labels morph hashed-meaning lex-id)
                       ;; ----------------------------------------------------------------------------------
                       ;; Node tests
                       ;; ----------------------------------------------------------------------------------
                       (:node-tests :update-references :check-duplicate :restrict-nr-of-nodes)
                       (:update-boundaries-feature . constituents)
                       (:parse-goal-tests :no-applicable-cxns)
                       (:production-goal-tests :no-applicable-cxns)
                       ;; ----------------------------------------------------------------------------------
                       ;; Miscellaneous
                       ;; ----------------------------------------------------------------------------------
                       (:draw-meaning-as-network . t)
                       (:use-meta-layer . nil)
                       )
  :visualization-configurations ((:show-constructional-dependencies . nil)
                                 (:with-search-debug-data . t))
  :hierarchy-features (dependents)

  (fcg::configure-tag-sets *italian-frame-extractor*
                           :pos-tag-set fcg::*italian-pos-tag-set*
                           :fcg-categories fcg::*italian-fcg-categories*))
