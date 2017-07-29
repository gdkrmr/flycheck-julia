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


(require 'flycheck)
(require 'flycheck-julia)
(require 'flycheck-ert)

(load "ess-autoloads.el")
(require 'ess-site)
(flycheck-julia-setup)
(setq flycheck-julia-port 3457)

(ert-deftest flycheck-julia-start-server ()
  :tags '(server)
  (flycheck-julia-server-start)
  (sleep-for 5)
  (should (flycheck-julia-server-p))
  (flycheck-julia-server-stop)
  (sleep-for 5)
  (kill-buffer "*julia-linter*"))

(ert-deftest flycheck-julia-kill-server ()
  :tags '(server)
  (flycheck-julia-server-start)
  (sleep-for 5)
  (should (flycheck-julia-server-p))
  (flycheck-julia-server-stop)
  (sleep-for 5)
  (should (not (flycheck-julia-server-p)))
  (sleep-for 5)
  (kill-buffer "*julia-linter*"))

(ert-deftest flycheck-julia-restart-server ()
  :tags '(server)
  (flycheck-julia-server-start)
  (sleep-for 5)
  (should (flycheck-julia-server-p))
  (flycheck-julia-server-restart)
  (sleep-for 5)
  (should (flycheck-julia-server-p))
  (flycheck-julia-server-stop)
  (sleep-for 5)
  (kill-buffer "*julia-linter*"))

;; (ert-deftest flycheck-julia-test-all ()
;;   (with-temp-buffer
;;     (ess-julia-mode)
;;     (flycheck-mode)
;;     (sleep-for 5)
;;     (print (flycheck-may-enable-checker 'flycheck-julia))
;;     (print flycheck-enabled-checkers)
;;     (flycheck- select-checker 'flycheck-julia)
;;     (print (flycheck-may-use-checker 'flycheck-julia))
;;     (insert-string "\ny\n")
;;     (flycheck-buffer)
;;     (sleep-for 5)
;;     (print flycheck-current-errors)
;;     (flycheck-buffer)
;;     (sleep-for 5)
;;     (print flycheck-current-errors)
;;     (sleep-for 5)
;;     (print flycheck-current-errors)
;;     (sleep-for 5)
;;     (print flycheck-current-errors)
;;     (sleep-for 5)
;;     (print flycheck-current-errors)
;;     (should (flycheck-julia-server-p))
;;     )
;;   (sleep-for 5)
;;   (kill-buffer "*julia-linter*"))

;; Lint.jl does extensive testing on the correctness of errors, so we only check
;; that querying the server actually works.
;; (ert-deftest flycheck-julia-test-query ()
;;   :tags '(query)
;;   (flycheck-julia-server-start)
;;   (sleep-for 5)
;;   (with-temp-buffer
;;     (insert-string "\ny\n")
;;     (ess-julia-mode)
;;     (flycheck-mode)
;;     (sleep-for 15)

;;     (message "test 0")
;;     (flycheck-buffer)
;;     (print flycheck-current-errors)
;;     (sleep-for 15)
;;     (print flycheck-current-errors)
;;     ;; Seeping causes the network process to close the connection
;;     ;; Fails, because process is already dead
;;     (message "test 1")
;;     (flycheck-buffer)
;;     (print flycheck-current-errors)
;;     (sleep-for 15)
;;     (print flycheck-current-errors)
;;     (should (cl-search "undeclared symbol" (aref (nth 0 flycheck-current-errors) 6))))
;;   ;; cleanup
;;   (sleep-for 5)
;;   (flycheck-julia-server-stop)
;;   (sleep-for 5)
;;   (kill-buffer "*julia-linter*"))

(provide 'flycheck-julia-test)

;;; flycheck-julia-test.el ends here
