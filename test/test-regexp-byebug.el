;; Press C-x C-e at the end of the next line to run this file test non-interactively
; (test-simple-run "emacs -batch -L %s -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "realgud.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(require 'realgud)
(load-file "../byebug/init.el")
(load-file "./regexp-helper.el")

(declare-function cmdbuf-loc-match      'realgud-regexp-helper)
(declare-function loc-match             'realgud-regexp-helper)
(declare-function prompt-match          'realgud-regexp-helper)
(declare-function __FILE__              'load-relative)

(test-simple-start)

(eval-when-compile
  (defvar dbg-name)   (defvar realgud-pat-hash)   (defvar realgud-bt-hash)
  (defvar loc-pat)    (defvar prompt-pat)         (defvar lang-bt-pat)
  (defvar file-group) (defvar line-group)
  (defvar test-dbgr)  (defvar test-text)
)

; Some setup usually done in setting up the buffer.
; We customize this for this debugger.
; FIXME: encapsulate this.
(setq dbg-name "byebug")

(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))
(setq test-dbgr (make-realgud-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (realgud-loc-pat-regexp loc-pat)
		  :file-group (realgud-loc-pat-file-group loc-pat)
		  :line-group (realgud-loc-pat-line-group loc-pat)))

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq test-text "[1, 10] in /home/rocky/realgud-byebug/test/gcd.rb
    1: #!/usr/bin/env ruby
    2:
    3: # GCD. We assume positive numbers
=>  4: def gcd(a, b)
    5:   # Make: a <= b
")
(note "traceback location matching")

(assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "basic location")
(assert-equal "/home/rocky/realgud-byebug/test/gcd.rb"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-text) "extract file name")
(assert-equal "4"
	      (match-string (realgud-cmdbuf-info-line-group test-dbgr)
			    test-text) "extract line number")



(note "traceback location matching")
;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq lang-bt-pat  (gethash "lang-backtrace"
                            realgud:byebug-pat-hash))
(set (make-local-variable 'test-text)
     "	from /usr/local/bin/irb:12:in `<main>'")

(setq test-text "	from /usr/local/bin/irb:12:in `<main>'")
;; (assert-t (numberp (loc-match test-text lang-bt-pat))
;; 	  "basic traceback location")
;; (assert-equal "/usr/local/bin/irb"
;; 	      (match-string (realgud-loc-pat-file-group lang-bt-pat) test-text)
;; 	      "extract traceback file name")
;; (assert-equal "12"
;; 	      (match-string (realgud-loc-pat-line-group
;; 			     lang-bt-pat) test-text)
;; 	      "extract traceback line number")

(note "prompt")
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud:byebug-pat-hash))
(prompt-match "(byebug) ")

(end-tests)
