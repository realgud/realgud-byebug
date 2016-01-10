;;; Copyright (C) 2016 Rocky Bernstein <rocky@gnu.org>
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

;;; pry debugger

(eval-when-compile (require 'cl))

(require 'realgud)
(require 'realgud-lang-ruby)
(require 'ansi-color)

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defconst realgud:pry-debugger-name "pry" "Name of debugger")

(defvar realgud:pry-pat-hash (make-hash-table :test 'equal)
  "hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  the values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc "realgud-loc" (a b c d e f))

(defconst realgud:pry-frame-file-regexp (format "\\(.+\\)"))

;; Regular expression that describes a pry location generally shown
;; before a command prompt.
;; For example:
;; From: /Users/mypizza/mypizza-web/config/environments/development.rb @ line 12 :
(setf (gethash "loc" realgud:pry-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^From: %s @ line %s"
		       realgud:pry-frame-file-regexp realgud:regexp-captured-num)
       :file-group 1
       :line-group 2))

;; Regular expression that describes a pry prompt
;; For example:
;;  [7] pry(#<Mypizza::Application>)>
;;  [10] pry(main)>
(setf (gethash "prompt" realgud:pry-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^\\[[0-9]+\\] pry(.*)> " 
       ))

;; Regular expression that describes a Ruby YARV syntax error line.
(setf (gethash "syntax-error" realgud:pry-pat-hash)
      realgud-ruby-YARV-syntax-error-pat)

;; Regular expression that describes a Ruby YARV backtrace line.
;; For example: 
;; 	from /ruby/gems/2.2.0/gems/fog-1.32.0/lib/fog/digitalocean.rb:1:in `<top (required)>'
;; 	from /Users/fog-1.32.0/lib/fog.rb:28:in `require'
;;	from /usr/lib/ruby/gems/rspec/compatibility.rb:6:in `const_missing'
(setf (gethash "lang-backtrace" realgud:pry-pat-hash)
      (make-realgud-loc-pat
       :regexp "^\\(?:[\t]from \\)?\\([^:]+\\):\\([0-9]+\\)\\(?:in `.*'\\)?"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a ruby $! backtrace
(setf (gethash "dollar-bang-backtrace" realgud:pry-pat-hash)
      realgud-ruby-dollar-bang-loc-pat)

;; Regular expression that describes a "breakpoint set" line
;; For example:
;   Breakpoint 1: /Users/rocky/src/environments/development.rb @ 14 (Enabled) 
(setf (gethash "brkpt-set" realgud:pry-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Breakpoint %s \\(.+\\), @ \\([.*]\\) "
		       realgud:regexp-captured-num realgud:regexp-captured-num)
       :num 1
       :file-group 3
       :line-group 4))

;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" realgud:pry-pat-hash)
      realgud-ruby-dollar-bang-loc-pat)


(defconst realgud:pry-frame-num-regexp
  (format "#%s " realgud:regexp-captured-num))


(setf (gethash "pry" realgud-pat-hash) realgud:pry-pat-hash)

;;  Prefix used in variable names (e.g. short-key-mode-map) for
;; this debugger

(setf (gethash "pry" realgud:variable-basename-hash) "realgud:pry")

(defvar realgud:pry-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'continue' and the value is
  the pry command to use, like 'continue'")

(setf (gethash realgud:pry-debugger-name
	       realgud-command-hash) realgud:pry-command-hash)

(setf (gethash "break"    realgud:pry-command-hash) "break %l")
(setf (gethash "continue" realgud:pry-command-hash) "continue")
(setf (gethash "clear"    realgud:pry-command-hash) "*not-implemented*")
(setf (gethash "up"       realgud:pry-command-hash) "*not-implemented*")
(setf (gethash "down"     realgud:pry-command-hash) "*not-implemented*")


(provide-me "realgud:pry-")
