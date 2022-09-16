;;; Copyright (C) Sony Computer Science Laboratories Paris
;;;               Authors: Remi van Trijp (www.remivantrijp.eu)
;;;                        Martina Galletti martina.galletti@sony.com
;;;                        Ines Blin ines.blin@sony.com
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

(in-package :asdf)

(defsystem :ife
  :author "Martina Galleti <martina.galletti@sony.com>; Ines Blin <ines.blin@sony.com>; Remi van Trijp <remi.vantrijp@sony.com>"
  :version "1.1"
  :license "GPL-3.0"
  :description "A grammar for extracting semantic frames from Italian texts."
  :depends-on (:utils :fcg :cl-ppcre :nlp-tools :test-framework
               :fcg-hybrids :frame-extractor)
  :components ((:file "package")
               (:file "cxn-inventory")
               (:module "write-constructions"
                :serial t
                :components ((:file "output-utils")
                             (:file "prepositions")
                             (:file "write-constructions")))
               (:file "load")))