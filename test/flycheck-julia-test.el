;;; flycheck-julia-test.el --- Flycheck Julia: Test cases  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Guido Kraemer <guido.kraemer@gmx.de>

;; Author: Guido Kraemer <guido.kraemer@gmx.de>
;; URL: https://github.com/gdkrmr/flycheck-julia

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

;; Test cases Flycheck Julia.

;;; Code:


(require 'flycheck-julia)
(require 'flycheck-ert)

(load "ess-autoloads.el")
(require 'ess-site)

(ert-deftest flycheck-julia-start-server ()
  :tags '(server)
  (flycheck-julia-server-start)
  (should (flycheck-julia-server-p))
  (flycheck-julia-server-stop)
  (sleep-for 5)
  (kill-buffer "*julia-linter*"))

(ert-deftest flycheck-julia-kill-server ()
  :tags '(server)
  (flycheck-julia-server-start)
  (sleep-for 5)
  (flycheck-julia-server-stop)
  (sleep-for 5)
  (should (not (flycheck-julia-server-p)))
  (sleep-for 5)
  (kill-buffer "*julia-linter*"))

(ert-deftest flycheck-julia-restart-server ()
  :tags '(server)
  (flycheck-julia-server-start)
  (sleep-for 5)
  (flycheck-julia-server-restart)
  (sleep-for 5)
  (should (flycheck-julia-server-p))
  (flycheck-julia-server-stop)
  (sleep-for 5)
  (kill-buffer "*julia-linter*"))

;; Lint.jl does extensive testing on the correctness of errors, so we only check
;; that querying the server actually works.
(ert-deftest flycheck-julia-test-query ()
  :tags '(query)
  (setq flycheck-julia-max-tries 20000)
  (flycheck-julia-server-start)
  (sleep-for 5)
  (flycheck-julia-client-start)
  (sleep-for 5)
  (with-temp-buffer
    (insert-string "\ny\n")

    (message "test 0")
    (print (flycheck-julia-server-query 'flycheck-julia))
    (print (flycheck-julia-client-p))   ;alive
    (sleep-for 5)
    ;; Seeping causes the network process to close the connection
    ;; Fails, because process is already dead
    (message "test 1")
    (print (flycheck-julia-server-query 'flycheck-julia))

    (sleep-for 5)
    (setq tmp-res nil)
    (message "test 2")
    (ignore-errors (progn (setq tmp-res (flycheck-julia-server-query 'flycheck-julia))
                          (print tmp-res)))
    (message "test 3")
    (print tmp-res)
    (should (cl-search "undeclared symbol" (aref (nth 0 tmp-res) 6))))
  ;; cleanup
  (sleep-for 5)
  (flycheck-julia-server-stop)
  (sleep-for 5)
  (kill-buffer "*julia-linter*"))

(provide 'flycheck-julia-test)

;;; flycheck-julia-test.el ends here


(flycheck-julia-server-p)
flycheck-julia-server-proc
(process-status flycheck-julia-server-proc)
(delete-process flycheck-julia-server-proc)
()
