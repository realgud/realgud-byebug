;; Copyright (C) 2016 Rocky Bernstein

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

;;  `realgud:byebug' Main interface to byebug via Emacs
(require 'cl)
(require 'load-relative)
(require 'realgud)
(require-relative-list '("core" "track-mode") "realgud:byebug-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:byebug nil
  "The realgud interface to byebug"
  :group 'realgud
  :version "24.5")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom realgud:byebug-command-name
  "byebug"
  "File name for executing the and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:byebug)

(declare-function realgud:byebug-track-mode     'realgud:byebug-track-mode)
(declare-function realgud-command            'realgud:byebug-core)
(declare-function realgud:byebug-parse-cmd-args 'realgud:byebug-core)
(declare-function realgud:byebug-query-cmdline  'realgud:byebug-core)
(declare-function realgud:run-process        'realgud-core)
(declare-function realgud:flatten            'realgud-utils)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:byebug (&optional opt-cmd-line no-reset)
  "Invoke the byebug debugger and start the Emacs user interface.

OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'.

Normally, command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset. See `loc-changes-clear-buffer' to clear
fringe and marginal icons.
"

  (interactive)
  (let* ((cmd-str (or opt-cmd-line (realgud:byebug-query-cmdline "byebug")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (realgud:byebug-parse-cmd-args cmd-args))
	 (script-args (caddr parsed-args))
	 (script-name (car script-args))
	 (parsed-cmd-args
	  (cl-remove-if 'nil (realgud:flatten parsed-args)))
	 (cmd-buf (realgud:run-process realgud:byebug-command-name
				       script-name parsed-cmd-args
				       'realgud:byebug-minibuffer-history
				       nil))
	 )
    (if cmd-buf
	(with-current-buffer cmd-buf
	  (realgud-command "set annotate 1" nil nil nil)
	  )
      )
    )
  )

(provide-me "realgud-")

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
