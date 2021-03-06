;;; realgud-byebug.el --- Realgud front-end to the Ruby byebug debugger -*- lexical-binding: t -*-

;; Author: Rocky Bernstein
;; Version: 1.0.0
;; Package-Type: multi
;; Package-Requires: ((realgud "1.4.5") (load-relative "1.2") (cl-lib "0.5") (emacs "24"))
;; URL: http://github.com/rocky/realgud-byebug
;; Compatibility: GNU Emacs 24.x

;; Copyright (C) 2015, 2016, 2018, 2019 Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; realgud support for the Ruby byebug debugger
;;
;; Until some realgud-specific support code is merged back into byebug,
;; you may using the code in repository https://github.com/rocky/byebug
;; will give a better realgud experience.

;;; Code:

;; Press C-x C-e at the end of the next line configure the program in
;; for building via "make" to get set up.
;; (compile (format "EMACSLOADPATH=:%s:%s:%s:%s ./autogen.sh" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "realgud.elc")) (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "loc-changes.elc"))))

(require 'load-relative)

(defgroup realgud-byebug  nil
  "Realgud interface to Ruby byebug debugger"
  :group 'realgud
  :version "24.3")

(require-relative-list '( "./byebug/byebug" ) "realgud-")
(load-relative "./byebug/byebug.el")

(provide-me)

;;; realgud-byebug.el ends here
