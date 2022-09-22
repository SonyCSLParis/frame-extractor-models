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

;; CAUSAL-PREPOSITIONS
(def-frame-cxn because-lex
               ((?cause-phrase
                 (sem-cat (phrase-type cause-phrase)))
                <-
                (?because
                 (sem-frame (causation
                             (cause ?cause-phrase)
                             (effect ?effect-clause)))
                 (sem-cat (function cause-marker))
                 (lex-id because)
                 --
                 (hash form ((string ?because "because")))
                 (parent ?cause-phrase)))
               :attributes (:label hashed-string :string "because")
               :cxn-inventory *english-frame-extractor*
               :description "Lexical construction for 'because'")
;; (extract-english-semantic-frames "Sea levels have risen because the earth is warming.")
;; (extract-english-semantic-frames "because I love you")

;; BECAUSE OF
(def-frame-cxn because-of-lex
               (<-
                (?because
                 --
                 (lex-id because)
                 (hash form ((string ?of "of")
                             (meets ?because ?of ?cause-phrase)))
                 (parent ?cause-phrase))
                (?of
                 --
                 (dep-head ?because)))
               :attributes (:label hashed-string :string "of")
               :cxn-inventory *english-frame-extractor*
               :description "Lexical construction for 'because of'.")
;; (extract-english-semantic-frames "Sea levels have risen because of climate change.")

;; DUE TO
(def-frame-cxn due-to-lex
               ((?cause-phrase
                 (sem-cat (phrase-type cause-phrase)))
                <-
                (?due
                 (sem-frame (causation
                             (cause ?cause-phrase)
                             (effect ?effect-clause)))
                 (sem-cat (function cause-marker))
                 --
                 (hash form ((string ?due "due")
                             (string ?to "to")
                             (meets ?due ?to ?cause-phrase)))
                 (parent ?cause-phrase))
                (?to
                 --
                 (dep-head ?due)))
               :attributes (:label hashed-string :string "due")
               :cxn-inventory *english-frame-extractor*
               :description "Lexical construction for due to that already assigns cause role.")
;; (extract-english-semantic-frames "Sea levels have risen due to climate change.")


;; ----------------------------------------------------------------------------
;; PREPOSITIONAL VERBS
;; ----------------------------------------------------------------------------
;; LEAD TO
;; ----------------------------------------------------------------------------
(def-fcg-cxn LEAD->leads-morph
             ((?leads-unit
               (footprints (number morph)))
              <-
              (?leads-unit
               (lex-id lead)
               (footprints (not number morph))
               (syn-cat (verb-form base-form)
                        (lex-class verb)
                        (finite +)
                        (agreement (- - + -)))
               --
               (HASH form ((string ?leads-unit "leads")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "leads")
            :cxn-inventory *english-frame-extractor*)

(def-fcg-cxn LEAD->leading-morph
             ((?leading-unit
               (footprints (number morph)))
              <-
              (?leading-unit
               (footprints (not number morph))
               (lex-id lead)
               (syn-cat (verb-form ing-form)
                        (lex-class verb)
                        (finite -)
                        (agreement ?agr))
               --
               (HASH form ((string ?leading-unit "leading")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "leading")
            :cxn-inventory *english-frame-extractor*)

(def-fcg-cxn LEAD->lead-morph
             ((?lead-unit
               (footprints (number morph)))
              <-
              (?lead-unit
               (lex-id lead)
               (footprints (not number morph))
               (syn-cat (verb-form base-form)
                        (lex-class verb)
                        (finite ?finite)
                        (agreement ?agr))
               --
               (HASH form ((string ?lead-unit "lead")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "lead")
            :cxn-inventory *english-frame-extractor*)

(def-fcg-cxn LEAD->led-morph
             ((?led-unit
               (footprints (number morph)))
              <-
              (?led-unit
               (lex-id lead)
               (footprints (not number morph))
               (syn-cat (verb-form ed-form)
                        (lex-class verb)
                        (finite ?finite)
                        (agreement ?agr))
               --
               (HASH form ((string ?led-unit "led")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "led")
            :cxn-inventory *english-frame-extractor*)

(def-frame-cxn led-to-lex
               (<-
                (?lead
                 (sem-frame (causation
                             (cause ?cause)
                             (effect ?effect)))
                 (syn-cat (subtype prepositional-verb))
                 --
                 (parent ?vp)
                 (lex-id lead)
                 (hash form ((string ?to "to")
                             (meets ?lead ?to ?vp)))))
               :attributes (:label hashed-lex-id :lex-id lead)
               :cxn-inventory *english-frame-extractor*)

;; RESULT IN
(def-fcg-cxn RESULT->results-morph
             ((?results-unit
               (footprints (number morph)))
              <-
              (?results-unit
               (lex-id result)
               (footprints (not number morph))
               (syn-cat (verb-form base-form)
                        (lex-class verb)
                        (finite +)
                        (agreement (- - + -)))
               --
               (HASH form ((string ?results-unit "results")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "results")
            :cxn-inventory *english-frame-extractor*)

(def-fcg-cxn RESULT->resulting-morph
             ((?resulting-unit
               (footprints (number morph)))
              <-
              (?resulting-unit
               (footprints (not number morph))
               (lex-id result)
               (syn-cat (verb-form ing-form)
                        (lex-class verb)
                        (finite -)
                        (agreement ?agr))
               --
               (HASH form ((string ?resulting-unit "resulting")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "resulting")
            :cxn-inventory *english-frame-extractor*)

(def-fcg-cxn RESULT->result-morph
             ((?result-unit
               (footprints (number morph)))
              <-
              (?result-unit
               (lex-id result)
               (footprints (not number morph))
               (syn-cat (verb-form base-form)
                        (lex-class verb)
                        (finite ?finite)
                        (agreement ?agr))
               --
               (HASH form ((string ?result-unit "result")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "result")
            :cxn-inventory *english-frame-extractor*)

(def-fcg-cxn RESULT->resulted-morph
             ((?resulted-unit
               (footprints (number morph)))
              <-
              (?resulted-unit
               (lex-id result)
               (footprints (not number morph))
               (syn-cat (verb-form ed-form)
                        (lex-class verb)
                        (finite ?finite)
                        (agreement ?agr))
               --
               (HASH form ((string ?resulted-unit "resulted")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "resulted")
            :cxn-inventory *english-frame-extractor*)

(def-frame-cxn result-in-lex
               (<-
                (?result
                 (sem-frame (causation
                             (cause ?cause-phrase)
                             (effect ?some-parent)))
                 (syn-cat (subtype prepositional-verb))
                 --
                 (lex-id result)
                 (syn-cat (verb-form ?verb-form))
                 (parent ?vp)
                 (hash form ((string ?in "in")
                             (meets ?result ?in ?vp)))))
               :attributes (:label hashed-lex-id :lex-id result)
               :cxn-inventory *english-frame-extractor*)


;; BRING ABOUT
(def-fcg-cxn BRING->brings-morph
             ((?brings-unit
               (footprints (number morph)))
              <-
              (?brings-unit
               (lex-id bring)
               (footprints (not number morph))
               (syn-cat (verb-form base-form)
                        (lex-class verb)
                        (finite +)
                        (agreement (- - + -)))
               --
               (HASH form ((string ?brings-unit "brings")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "brings")
            :cxn-inventory *english-frame-extractor*)

(def-fcg-cxn BRING->bringing-morph
             ((?bringing-unit
               (footprints (number morph)))
              <-
              (?bringing-unit
               (footprints (not number morph))
               (lex-id bring)
               (syn-cat (verb-form ing-form)
                        (lex-class verb)
                        (finite -)
                        (agreement ?agr))
               --
               (HASH form ((string ?bringing-unit "bringing")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "bringing")
            :cxn-inventory *english-frame-extractor*)

(def-fcg-cxn BRING->bring-morph
             ((?bring-unit
               (footprints (number morph)))
              <-
              (?bring-unit
               (lex-id bring)
               (footprints (not number morph))
               (syn-cat (verb-form base-form)
                        (lex-class verb)
                        (finite ?finite)
                        (agreement ?agr))
               --
               (HASH form ((string ?bring-unit "bring")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "bring")
            :cxn-inventory *english-frame-extractor*)

(def-fcg-cxn BRING->brought-morph
             ((?brought-unit
               (footprints (number morph)))
              <-
              (?brought-unit
               (lex-id bring)
               (footprints (not number morph))
               (syn-cat (verb-form ed-form)
                        (lex-class verb)
                        (finite -)
                        (agreement ?agr))
               --
               (HASH form ((string ?brought-unit "brought")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "brought")
            :cxn-inventory *english-frame-extractor*)

(def-frame-cxn bring-about-lex
               (<-
                (?bring
                 (sem-frame (causation
                             (cause ?cause-phrase)
                             (effect ?some-parent)))
                 (syn-cat (subtype prepositional-verb))
                 --
                 (parent ?vp)
                 (lex-id bring)
                 (hash form ((string ?about "about")
                             (meets ?bring ?about ?vp)))))
               :attributes (:label hashed-lex-id :lex-id bring)
               :cxn-inventory *english-frame-extractor*)