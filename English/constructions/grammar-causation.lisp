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

;; Linking CAUSE-MARKERS such as "because" and "due to"
;; ----------------------------------------------------------------------------
(def-frame-cxn effect-clause-cxn
               (<-
                (?cause-phrase
                 --
                 (sem-cat (phrase-type cause-phrase))
                 (parent ?verb-phrase))
                (?frame-evoking-element
                 (footprints (effect-clause-cxn))
                 --
                 (footprints (not effect-clause-cxn))
                 (sem-frame (causation
                             (cause ?cause-phrase)
                             (effect (unit-append ?subject-phrase ?verb-phrase))))
                 (sem-cat (function cause-marker))
                 (parent ?cause-phrase))
                (?verb-phrase
                 --
                 (syn-cat (phrase-type verb-phrase))
                 (parent ?clause))
                (?clause
                 --
                 (fields (subject-phrase ?subject-phrase)
                         (verb-phrase ?verb-phrase))))
               :cxn-set cxn
               :disable-automatic-footprints t
               :cxn-inventory *english-frame-extractor*
               :description "Links an effect-clause to a cause-phrase such as 'because FCG is the best'")
;; (extract-english-semantic-frames "I love FCG because it is the best")

(def-frame-cxn effect-clause-dep-cxn
               (<-
                (?frame-evoking-element
                 --
                 (sem-cat (function cause-marker))
                 (syn-cat (lex-class adjective))
                 (sem-frame (causation
                             (cause ?cause-phrase)
                             (effect (unit-append ?subject-phrase ?verb-phrase))))
                 (dep-head ?verb))
                (?verb
                 --
                 (syn-cat (verb-form ?verb-form))
                 (parent ?verb-phrase))
                (?verb-phrase
                 --
                 (syn-cat (phrase-type verb-phrase))
                 (parent ?clause))
                (?clause
                 --
                 (fields (subject-phrase ?subject-phrase)
                         (verb-phrase ?verb-phrase))))
               :cxn-set cxn
               :cxn-inventory *english-frame-extractor*
               :description "Links an effect-clause to a deeply nested cause-phrase")
;; (extract-english-semantic-frames "In his speech, he sought to ridicule evidence of growing draught and heat waves due to climate change.")


;; Linking causal prepositional verbs such as "lead to"
;; ----------------------------------------------------------------------------

(def-frame-cxn Cause-PrepVerb-Effect-semsubject-cxn
               (<-
                (?frame-evoking-element
                 (footprints (Cause-PrepVerb-Effect-semsubject-cxn))
                 --
                 (footprints (not Cause-PrepVerb-Effect-semsubject-cxn))
                 (sem-frame (causation
                             (cause ?subject-phrase)
                             (effect ?prepositional-phrase)))
                 (syn-cat (subtype prepositional-verb))
                 (parent ?verb-phrase))
                (?verb-phrase
                 --
                 (syn-cat (phrase-type verb-phrase))
                 (sem-cat (semantic-subject ?subject-phrase))
                 (hash form ((meets ?frame-evoking-element ?prepositional-phrase ?verb-phrase))))
                (?prepositional-phrase
                 --
                 (syn-cat (phrase-type prepositional-phrase))))
               :cxn-set cxn
               :disable-automatic-footprints t
               :cxn-inventory *english-frame-extractor*
               :description "Links a CAUSE (subject) to an EFFECT through a double verb structure.")
; (extract-english-semantic-frames "Current Coalition Direct Action climate policies are projected to result in increased greenhouse gas emissions, albeit at a level that might allow Australia to meet its 2020 target because of accounting rules.")


(def-frame-cxn Cause-PrepVerb-Effect-cxn
               (<-
                (?frame-evoking-element
                 (footprints (Cause-PrepVerb-Effect-cxn))
                 --
                 (footprints (not Cause-PrepVerb-Effect-cxn))
                 (sem-frame (causation
                             (cause ?subject-phrase)
                             (effect ?prepositional-phrase)))
                 (syn-cat (subtype prepositional-verb))
                 (parent ?verb-phrase))
                (?verb-phrase
                 --
                 (syn-cat (phrase-type verb-phrase))
                 (parent ?clause)
                 (hash form ((meets ?frame-evoking-element ?prepositional-phrase ?verb-phrase))))
                (?prepositional-phrase
                 --
                 (syn-cat (phrase-type prepositional-phrase)))
                (?clause
                 --
                 (syn-cat (clause-type ?clause-type))
                 (fields (subject-phrase ?subject-phrase)
                         (verb-phrase ?verb-phrase))))
               :cxn-set cxn
               :disable-automatic-footprints t
               :cxn-inventory *english-frame-extractor*
               :description "Links a CAUSE (subject) to an EFFECT through a prepositional verb.")

