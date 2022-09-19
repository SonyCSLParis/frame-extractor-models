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

;;; ----------------------------------------------------------------------------
;;; 1. Comportare
;;; ----------------------------------------------------------------------------

;; 1.1 x-such-as-to-give-rise-to-y-cxn
;; ----------------------------------------------------------------------------
(extract-semantic-frames "un numero eccessivo di discendenti tale da comportare uno spezzettamento del patrimonio familiare"
                         :phrase-based 
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;; 1.2 subject-causes-object-cxn
;; ----------------------------------------------------------------------------
(extract-semantic-frames "un meccanismo del genere possa comportare automaticamente una divergenza radicale nella distribuzione del capitale."
                         :phrase-based 
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;; Note: this is a possible causation. To be added.
(extract-semantic-frames "è possibile che questo equivoco comporti una significativa sottovalutazione delle disuguaglianze." 
                         :phrase-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;; Note: this is a non-necessary causation. To be added.
(extract-semantic-frames "in secondo luogo, per quanto riguarda la quota di redditi da capitale nella composizione del reddito nazionale e mondiale, l'esperienza storica suggerisce che la prevedibile crescita del rapporto capitale/reddito non comporterà necessariamente un ribasso sensibile del rendimento del capitale." 
                         :phrase-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;; Note: Construction missing for second causation frame
(extract-semantic-frames "pare probabile che l'aumento di quasi il 25% del salario minimo previsto negli stati uniti non comporterà perdite d'impiego o le comporterà in misura minima"
                         :phrase-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;; Tricky: dependency parser returns two subjects...
;;         how to handle?
(extract-semantic-frames "l'implosione dell'unione europea che l'arrivo al potere dei partiti nazionalisti potrebbe comportare non farebbe che esasperare ulteriormente la concorrenza fiscale e sociale tra i paesi, l'aumento delle disuguaglianze e la deriva identitaria."
                         :phrase-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;;; ----------------------------------------------------------------------------
;;; 2. Portare a
;;; ----------------------------------------------------------------------------

;;; 2.1 Lead to
(extract-semantic-frames "il fattore principale che portò alla formazione di queste coalizioni di idee e di questa nuova visione del ruolo dello stato fu la delegittimazione del sistema della proprietà privata e della libera concorrenza"
                         :phrase-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;;; 2.2 TODO: Lead  to + possibility
(extract-semantic-frames "un fattore in sé molto positivo ma che può portare a un enorme spreco umano e grandi frustrazioni sociali"
                         :phrase-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

(extract-semantic-frames "ciò potrebbe portare a elaborare deliberazioni e a formare coalizioni di tipo diverso, al di là dello stereotipato gioco delle parti che a volte ha luogo nella cogestione."
                         :phrase-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;; Handle: Two subjects!!
(extract-semantic-frames "un obiettivo comunque non facile da realizzare poiché presuppone un ampio coinvolgimento sociale e politico su questioni che certo riguardano tutti, ma la cui evidente complessità tecnica può portare a affidarsi ad altri ."
                         :phrase-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

