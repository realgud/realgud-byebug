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

;;; byebug debugger

(eval-when-compile (require 'cl-lib))

(require 'realgud)
(require 'realgud-lang-ruby)
(require 'ansi-color)

(defvar realgud:byebug-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defconst realgud:byebug-debugger-name "byebug" "Name of debugger")

(defvar realgud:byebug-pat-hash (make-hash-table :test 'equal)
  "hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  the values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc "realgud-loc" (a b c d e f))

(defconst realgud:byebug-frame-file-regexp "\\(.+\\)")

(defconst realgud:byebug-file-line-regexp "\\([^:]+\\):\\([0-9]+\\)")

;; Regular expression that describes a byebug location generally shown
;; before a command prompt.
;; For example:
;; [10, 19] in /Users/rocky/gcd.rb
;;    1: #!/usr/bin/env/ruby
;; => 4: def gcd
(setf (gethash "loc" realgud:byebug-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^\\[[0-9]+, [0-9]+\\] in %s\n\\(?:.\\|\n\\)*\n=>[ ]*%s: "
		       realgud:byebug-frame-file-regexp realgud:regexp-captured-num)
       :file-group 1
       :line-group 2))

;; Regular expression that describes a byebug prompt
;; For example:
;;  [7] byebug(#<Mypizza::Application>)>
;;  [10] byebug(main)>
(setf (gethash "prompt" realgud:byebug-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^(byebug) "
       ))

;; Regular expression that describes a Ruby YARV syntax error line.
(setf (gethash "syntax-error" realgud:byebug-pat-hash)
      realgud-ruby-YARV-syntax-error-pat)

;; Regular expression that describes a Ruby YARV backtrace line.
;; For example:
;; 	from /ruby/gems/2.2.0/gems/fog-1.32.0/lib/fog/digitalocean.rb:1:in `<top (required)>'
;; 	from /Users/fog-1.32.0/lib/fog.rb:28:in `require'
;;	from /usr/lib/ruby/gems/rspec/compatibility.rb:6:in `const_missing'
(setf (gethash "lang-backtrace" realgud:byebug-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "(^\\(?:[\t]from \\)?%s\\(?:in `.*'\\)?"
		       realgud:byebug-file-line-regexp)

       :file-group 1
       :line-group 2))

;;  Regular expression that describes a ruby $! backtrace
(setf (gethash "dollar-bang-backtrace" realgud:byebug-pat-hash)
      realgud-ruby-dollar-bang-loc-pat)

;; Regular expression that describes a "breakpoint set" line
;; For example:
;;   Successfully created breakpoint with id
(setf (gethash "brkpt-set" realgud:byebug-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Created breakpoint %s at %s"
		       realgud:regexp-captured-num
		       realgud:byebug-file-line-regexp)
       :num 1
       :file-group 2
       :line-group 3
       ))


;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Deleted breakpoint 1.
(setf (gethash "brkpt-del" realgud:byebug-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Deleted breakpoint %s\n"
		       realgud:regexp-captured-num)
       :num 1))

;; Regular expression that describes a debugger "disable" (breakpoint) response.
;; For example:
;;   Breakpoint entry 4 disabled.
(setf (gethash "brkpt-disable" realgud:byebug-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Breakpoint %s disabled"
		       realgud:regexp-captured-num)
       :num 1))

;; Regular expression that describes a debugger "enable" (breakpoint) response.
;; For example:
;;   Breakpoint 4 enabled.
(setf (gethash "brkpt-enable" realgud:byebug-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Breakpoint %s enabled"
		       realgud:regexp-captured-num)
       :num 1))

;;  Regular expression that describes debugger "backtrace" command line.
;;  e.g.
;; --> #0  Object.gcd(a#Fixnum, b#Fixnum) at /home/rocky/realgud-byebug/test/gcd.rb:6
;;     #1  Object.gcd(a#Fixnum, b#Fixnum) at /home/rocky/realgud-byebug/test/gcd.rb:15
;;     #2  <top (required)> at /home/rocky/realgud-byebug/test/gcd.rb:19
(setf (gethash "debugger-backtrace" realgud:byebug-pat-hash)
      (make-realgud-loc-pat
       :regexp 	(concat realgud:trepan-frame-start-regexp " "
			realgud:trepan-frame-num-regexp
			"[ \t]+[^ ]* at \\([^:]*\\):\\([0-9]+\\)"
			)
       :num 2
       :file-group 3
       :line-group 4)
      )

(setf (gethash "font-lock-keywords" realgud:byebug-pat-hash)
      '(
	;; The frame number and first type name, if present.
	("^\\(-->\\|   \\)? #\\([0-9]+\\)[ \t]+\\([^ ]*\\) at \\([^:]*\\):\\([0-9]*\\)"
	 (2 realgud-backtrace-number-face)
	 (3 font-lock-constant-face)        ; e.g. Object
	 (4 realgud-file-name-face)
	 (5 realgud-line-number-face))
	))


;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" realgud:byebug-pat-hash)
      realgud-ruby-dollar-bang-loc-pat)


(defconst realgud:byebug-frame-num-regexp
  (format "#%s " realgud:regexp-captured-num))


(setf (gethash "byebug" realgud-pat-hash) realgud:byebug-pat-hash)

;;  Prefix used in variable names (e.g. short-key-mode-map) for
;; this debugger

(setf (gethash "byebug" realgud:variable-basename-hash) "realgud:byebug")

(defvar realgud:byebug-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'continue' and the value is
  the byebug command to use, like 'continue'")

(setf (gethash realgud:byebug-debugger-name
	       realgud-command-hash) realgud:byebug-command-hash)

(setf (gethash "break"    realgud:byebug-command-hash) "break %X:%l")
(setf (gethash "continue" realgud:byebug-command-hash) "continue")
(setf (gethash "clear"    realgud:byebug-command-hash) "*not-implemented*")
(setf (gethash "disable"  realgud:byebug-command-hash) "disable breakpoints %p")
(setf (gethash "enable"   realgud:byebug-command-hash) "enable breakpoints %p")
(setf (gethash "shell"    realgud:byebug-command-hash) "irb")
(setf (gethash "restart"  realgud:byebug-command-hash) "restart")
(setf (gethash "until"    realgud:byebug-command-hash) "continue %l")

;; Unsupported features:
(setf (gethash "jump"     realgud:byebug-command-hash) "*not-implemented*")

(setf (gethash "byebug" realgud-command-hash) realgud:byebug-command-hash)
(setf (gethash "byebug" realgud-pat-hash) realgud:byebug-pat-hash)

(provide-me "realgud:byebug-")
