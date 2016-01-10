;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(require 'realgud-buffer-command)
(load-file "../pry/init.el")
(load-file "./regexp-helper.el")

(declare-function __FILE__              'load-relative)

(test-simple-start)

(eval-when-compile
  (defvar dbg-name)   (defvar realgud-pat-hash)   (defvar realgud-bt-hash)
  (defvar loc-pat)    (defvar prompt-pat)         (defvar s1)
  (defvar file-group) (defvar line-group)         (defvar pos)
  (defvar test-dbgr)  (defvar test-text)
)

; Some setup usually done in setting up the buffer.
; We customize this for this debugger.
; FIXME: encapsulate this.
(setq dbg-name "pry")

(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))
(setq test-dbgr (make-realgud-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (realgud-loc-pat-regexp loc-pat)
		  :file-group (realgud-loc-pat-file-group loc-pat)
		  :line-group (realgud-loc-pat-line-group loc-pat)))

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq test-text "From: /Users/mypizza-web/config/environments/development.rb @ line 12 :")
(note "traceback location matching")

(assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "basic location")
(assert-equal "/Users/mypizza-web/config/environments/development.rb"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-text) "extract file name")
(assert-equal "12"
	      (match-string (realgud-cmdbuf-info-line-group test-dbgr)
			    test-text) "extract line number")
(note "lang-backtrace")
(setq realgud-bt-pat  (gethash "lang-backtrace"
                               realgud:pry-pat-hash))
(setq s1
      "	from /ruby/gems/2.2.0/gems/fog-1.32.0/lib/fog/digitalocean.rb:1:in `<top (required)>
       	from /Users/fog-1.32.0/lib/fog.rb:28:in `require'")

;; FIXME: Fill this stuff in

(note "prompt")
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud:pry-pat-hash))
(prompt-match "[7] pry(#<Mypizza::Application>)> ")

(end-tests)
