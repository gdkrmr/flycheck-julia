;;; flycheck-julia.el --- Flycheck: Julia support

;; Copyright (C) 2017  Guido Kraewmer <guido.kraemer@gmx.de>

;; Author: Guido Kraewmer <guido.kraemer@gmx.de>
;; URL: https://github.com/gdkrmr/flycheck-julia
;; Keywords: convenience, tools, languages
;; Version: 0.1
;; Package-Requires: ((emacs "25") (flycheck "0.22")  (json))

;; This file is not part of GNU Emacs.

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

;; This Flycheck extension provides a `Lint.jl' integration for flycheck (see
;; URL `https://github.com/tonyhffong/Lint.jl') to check Julia buffers for
;; errors.
;;
;; # Setup
;;
;; Add the following to your init file:
;;
;;      ;; Enable Flycheck checker
;;      (flycheck-julia-setup))
;;
;;    (add-hook 'julia-mode-hook #'flycheck-mode)
;;
;; # Usage
;;
;; Just use Flycheck as usual in julia-mode buffers. Flycheck will
;; automatically use the `flycheck-julia` syntax checker.
;;

;;; Code:

(require 'json)
(require 'flycheck)


;; TODO: add config variables for server port and julia executable!

(defun flycheck-julia-start-or-query-server (checker callback)
  "Start a Julia syntax check, init the server if necessary.

CHECKER and CALLBACK are flycheck requirements."

  ;; TODO: use (when ...) here and do the query
  (if (not (get-process "flycheck-julia-server"))
      (progn
        (message "no server --- starting")
        (flycheck-julia-server-start)
        (funcall callback 'finished nil))
    (message "server running --- querying")
    ;; TODO don't use globals here:
    (setq the-errors (flycheck-julia-server-query checker))
    (funcall callback 'finished the-errors)))

(defun flycheck-julia-server-start ()
  "Start the julia server for linting."
  ;; make-process is emacs 25 only:
  ;; this one does not work anywayse:
  ;; (make-process
  ;;  :name            "flycheck-julia-server"
  ;;  :buffer          nil
  ;;  :command         '("julia" "-e 'using Lint; lintserver(9999, \"standard-linter-v2\")'")
  ;;  :noquery         t
  ;;  :stop            nil)
  (start-process-shell-command
   "flycheck-julia-server" "*julia-linter*"
   ;; TODO: use pipes or something different than an open port
   ;; TODO: make port a variable and choose sensible default
   "julia -e \'using Lint\; lintserver\(9999\, \"standard-linter-v2\"\)\'"))

(defun flycheck-julia-server-stop ()
  "Kill the julia lint server."
  (kill-process (get-process "flycheck-julia-server")))

(defun flycheck-julia-sever-restart ()
  "Kill the julia lint server and restart it."
  (flycheck-julia-server-stop)
  (flycheck-julia-server-start))

(defun flycheck-julia-server-query (checker)
  "Query a lint for the current buffer.

CHECKER is a flycheck internal."

  ;; TODO: is it better to have the network process running all the time?
  ;; i.e. is there overhead for using a new process every time this function is run?
  (let ((proc (make-network-process
               :name "julia-lint-client"
               :host 'local
               :service 9999))
        (query-list '()))
    ;; capture the process output
    ;; TODO: find a nicer way to do this (i.e. without
    ;; global variables), this is taken from the following page:
    ;; http://www.math.utah.edu/docs/info/elisp_34.html

    ;; Network processes may be return results in different orders, then we are
    ;; screwed, not sure what to do about this? use named pipes? use sockets?
    ;; use priority queues?
    (defun keep-output (process output)
      (setq kept (concat kept output)))
    (setq kept "")
    (set-process-filter proc 'keep-output)
    ;; build the json object
    ;; TODO: find a way to do this in one single expression
    ;; this just seems plain stupid but there is no evaluation inside '(...)
    (setf (alist-get '"show_code"       query-list) t)
    (setf (alist-get '"ignore_warnings" query-list) json-false)
    (setf (alist-get '"ignore_info"     query-list) json-false)
    (setf (alist-get '"code_str"        query-list) (buffer-substring-no-properties
                                                     (point-min) (point-max)))
    (setf (alist-get '"file"            query-list) (buffer-file-name))
    ;; TODO: make this local
    (setq encoded-query-list (json-encode query-list))
    ;; (message encoded-query-list)
    ;; (message (type-of encoded-query-list))
    (process-send-string proc encoded-query-list)
    ;; TODO: because our process is asynchronous, we need to
    ;; 1. to wait and
    ;; 2. the string is sent in 500 char pieces and the results may arrive in a
    ;; different order.
    ;; TODO: change this for something that waits for the process.
    ;; TODO: figure out a way to do this complete asynchronous.
    (sleep-for 0.5)
    ;; (message (nth 0 kept))

    ;; TODO: change this to be local again
    (setq tmp (flycheck-julia-error-parser
               (json-read-array-from-string kept)
               checker
               (current-buffer))))
  tmp)

(defun flycheck-julia-error-parser (errors checker buffer)
  "Parse the error returned from the Julia lint server.

ERRORS is the string returned by the server, it contains a json object.
CHECKER is the julia linter.
BUFFER is the buffer that was checked for errors."

  ;; (message "entered error-parser")
  (mapcar (lambda (it)
            (flycheck-error-new
             :buffer   buffer
             :checker  checker
             :filename (cdr (assoc 'file (cdr (assoc 'location it))))
             ;; Lint.jl returns 0-based line and column numbers
             ;; TODO: figure out, why I have to add 2 to the line numbers
             ;; TODO: these numbers are not right yet!
             :line     (1+ (aref (aref (cdr (assoc 'position (cdr (assoc 'location it)))) 0) 0))
             :column   (1+ (aref (aref (cdr (assoc 'position (cdr (assoc 'location it)))) 0) 1))
             :message  (cdr (assoc 'excerpt it))
             :level (cond
                     ((equal (cdr (assoc 'severity it)) "error")   'error)
                     ((equal (cdr (assoc 'severity it)) "warning") 'warning)
                     ((equal (cdr (assoc 'severity it)) "info")    'info))))
          errors))

;; TODO: add this to the json package
(defun json-read-array-from-string (string)
  "Read the JSON array contained in STRING and return it."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (json-read-array)))

(flycheck-define-generic-checker 'julia-linter
  "A source code linter for Julia using Lint.jl."
  :start     #'flycheck-julia-start-or-query-server
  :modes     '(julia-mode ess-julia-mode)
  ;; TODO: I don't think this is necessary.
  ;; :predicate (lambda () (equal ess-language "julia"))
  )

;;;###autoload
(defun flycheck-julia-setup ()
  "Setup Flycheck Julia.

Add `flycheck-julia' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'julia-linter))

(provide 'flycheck-julia)

;;; flycheck-julia.el ends here
