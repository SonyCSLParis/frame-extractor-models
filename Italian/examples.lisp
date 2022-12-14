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

;; Need to re-initialize the grammar?
; (write-all-constructions)
; (load-italian-frame-extractor)

;; Activate the web interface:
; (activate-monitor trace-fcg)

;;; ----------------------------------------------------------------------------
;;; 1. Comportare
;;; ----------------------------------------------------------------------------

;; 1.1 x-such-as-to-give-rise-to-y-cxn
;; ----------------------------------------------------------------------------
(extract-semantic-frames "un numero eccessivo di discendenti tale da comportare uno spezzettamento del patrimonio familiare"
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)


;; 1.2 subject-causes-object-cxn
;; ----------------------------------------------------------------------------
(extract-semantic-frames "un meccanismo del genere possa comportare automaticamente una divergenza radicale nella distribuzione del capitale."
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;; Note: this is a possible causation. To be added.
(extract-semantic-frames "?? possibile che questo equivoco comporti una significativa sottovalutazione delle disuguaglianze." 
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;; Note: this is a non-necessary causation. To be added.
(extract-semantic-frames "in secondo luogo, per quanto riguarda la quota di redditi da capitale nella composizione del reddito nazionale e mondiale, l'esperienza storica suggerisce che la prevedibile crescita del rapporto capitale/reddito non comporter?? necessariamente un ribasso sensibile del rendimento del capitale." 
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;; Note: Construction missing for second causation frame
(extract-semantic-frames "pare probabile che l'aumento di quasi il 25% del salario minimo previsto negli stati uniti non comporter?? perdite d'impiego o le comporter?? in misura minima"
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;; Tricky: dependency parser returns two subjects...
;;         how to handle?
(extract-semantic-frames "l'implosione dell'unione europea che l'arrivo al potere dei partiti nazionalisti potrebbe comportare non farebbe che esasperare ulteriormente la concorrenza fiscale e sociale tra i paesi, l'aumento delle disuguaglianze e la deriva identitaria."
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;;; ----------------------------------------------------------------------------
;;; 2. Portare a
;;; ----------------------------------------------------------------------------

;;; 2.1 Lead to
(extract-semantic-frames "il fattore principale che port?? alla formazione di queste coalizioni di idee e di questa nuova visione del ruolo dello stato fu la delegittimazione del sistema della propriet?? privata e della libera concorrenza"
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;;; 2.2 TODO: Lead  to + possibility
(extract-semantic-frames "un fattore in s?? molto positivo ma che pu?? portare a un enorme spreco umano e grandi frustrazioni sociali"
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

(extract-semantic-frames "ci?? potrebbe portare a elaborare deliberazioni e a formare coalizioni di tipo diverso, al di l?? dello stereotipato gioco delle parti che a volte ha luogo nella cogestione."
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;; Handle: Two subjects!!
(extract-semantic-frames "un obiettivo comunque non facile da realizzare poich?? presuppone un ampio coinvolgimento sociale e politico su questioni che certo riguardano tutti, ma la cui evidente complessit?? tecnica pu?? portare a affidarsi ad altri ."
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;;; ----------------------------------------------------------------------------
;;; 3. poich?? (because)
;;; ----------------------------------------------------------------------------

(extract-semantic-frames "nelle regioni colorate in bianco il paf non ?? calcolabile, poich?? non si sono osservati decessi tra persone con livello di istruzione alto."
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

(extract-semantic-frames "tra il 2008 e il 2018 vi ?? stato anche un calo di circa il 10% negli investimenti reali per singolo studente del terzo livello, poich?? i budget pubblici non hanno tenuto il passo con il numero degli studenti."
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

(extract-semantic-frames "il fatto non pu?? bastare, poich?? il problema degli investimenti internazionali si pone in primo luogo per il futuro."
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

(extract-semantic-frames "ne risulterebbe una forte disuguaglianza dei redditi da lavoro, in parte fittizia, poich?? essa diminuirebbe se si misurassero le disuguaglianze sul pi?? lungo periodo, per esempio su dieci anni e non su uno solo , o anche sul totale della vita degli individui , ipotesi ideale per studiare davvero quelle disuguaglianze di opportunit?? e di destini di cui parlava vautrin, ma purtroppo difficile da realizzare per mancanza di dati."
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

(extract-semantic-frames "qualsiasi contestazione della legittimit?? dei diritti di propriet?? stabiliti in passato avrebbe rischiato di scoperchiare un vaso di pandora da cui la societ?? non sarebbe mai uscita indenne, poich?? nessuno avrebbe pi?? saputo dove fermarsi."
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;; Two causation frames:
(extract-semantic-frames "un obiettivo comunque non facile da realizzare, poich?? presuppone un ampio coinvolgimento sociale e politico su questioni che certo riguardano tutti, ma la cui evidente complessit?? tecnica pu?? portare ad affidarsi  ad altri ."
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

(extract-semantic-frames "esclusione di per s?? assurda, poich?? porta a sottovalutare in maniera del tutto artificiale il livelli di prodotto interno e di reddito nazionale dei paesi che scelgono un sistema sanitario e scolastico pubblico anzich?? un sistema privato"
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

(extract-semantic-frames "nei grafici 6.1,6.4, gli interessi del debito pubblico, che non fanno parte del reddito nazionale e che remunerano un capitale che non fa parte del capitale nazionale (poich?? il debito pubblico rientra nell'attivo per i detentori privati ma ?? un passivo per lo stato), non sono stati presi in considerazione."
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;; To check!
(extract-semantic-frames "questo porta automaticamente a sottovalutare l'entit?? del deprezzamento del capitale naturale e a sopravvalutare la crescita reale, poich?? questa ?? ottenuta in parte attingendo a riserve presenti da sempre e con effetti di inquinamento atmosferico e riscaldamento globale"
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

;; TODO: error spacy
(extract-semantic-frames "i suoi interlocutori erano meno pronti ad accettarlo poich?? l'80% dell'aumento finiva direttamente nelle casse del tesoro pubblico."
                         :dependency-based
                         :cxn-inventory *italian-frame-extractor*
                         :frame-dev-monitor t)

