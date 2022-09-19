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

;;; ----------------------------------------------------------------------------
;;; Temporary de-render method
;;; ----------------------------------------------------------------------------

(defmethod de-render ((utterance string)
                      (mode (eql :italian-hybrid-with-lemmas))
                      &key cxn-inventory (model "it") &allow-other-keys)
  (declare (ignorable mode))
  (let* ((transient-structure (de-render utterance :italian-hybrid
                                         :cxn-inventory cxn-inventory
                                         :model model))
         (units (left-pole-structure transient-structure))
         (root (get-root units))
         (lemmas (get-penelope-lemmas utterance :model model))
         (units-expanded-with-lemmas (loop for unit in (remove root units :test #'equal)
                                           for lemma in lemmas
                                           collect `(,(unit-name unit)
                                                     ,@(loop for feature in (unit-body unit)
                                                             collect (if (string= (symbol-name 
                                                                                   (feature-name feature))
                                                                                  "SYN-CAT")
                                                                       `(,(feature-name feature)
                                                                         ,(cons `(lemma ,lemma)
                                                                                (feature-value feature)))
                                                                       feature)))))
         (new-unit-structure (cons root units-expanded-with-lemmas)))
    (setf (left-pole-structure transient-structure) new-unit-structure)
    transient-structure))

;;; ----------------------------------------------------------------------------
;;; CXN-INVENTORY
;;; ----------------------------------------------------------------------------

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
                       (:de-render-mode . :italian-hybrid-with-lemmas)
                       ;; ----------------------------------------------------------------------------------
                       ;; Form predicates
                       ;; ----------------------------------------------------------------------------------
                       (:form-predicates meets)
                       ;; ----------------------------------------------------------------------------------
                       ;; Construction Sets
                       ;; ----------------------------------------------------------------------------------
                       (:production-order hashed-meaning cxn hashed-lex-id)
                       (:parse-order morph lex cxn)
                       (:hashed-labels morph lex)
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

  ;; Inform fcg-hybrids about which tag-sets are used by the neurostatistical parser:
  (fcg::configure-tag-sets *italian-frame-extractor*
                           :pos-tag-set fcg::*italian-pos-tag-set*
                           :fcg-categories fcg::*italian-fcg-categories*)

  ;; Activate the frame extractor:
  (activate-frame-extractor :cxn-inventory *italian-frame-extractor*
                            :frame-feature 'sem-frame))
