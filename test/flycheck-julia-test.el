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

;; Test cases Flycheck OCaml.

;;; Code:


(require 'flycheck-julia)
(require 'flycheck-ert)

(load "ess-autoloads.el")
(require 'ess-site)

(ert-deftest flycheck-julia-start-server ()
  :tags '(server)
  (flycheck-julia-server-start)
  (should (flycheck-julia-serverp))
  (flycheck-julia-server-stop)
  (sleep-for 5)
  (kill-buffer "*julia-linter*"))

(ert-deftest flycheck-julia-kill-server ()
  :tags '(server)
  (flycheck-julia-server-start)
  (sleep-for 5)
  (flycheck-julia-server-stop)
  (sleep-for 5)
  (should (not (flycheck-julia-serverp)))
  (sleep-for 5)
  (kill-buffer "*julia-linter*"))

(ert-deftest flycheck-julia-restart-server ()
  :tags '(server)
  (flycheck-julia-server-start)
  (sleep-for 5)
  (flycheck-julia-server-restart)
  (sleep-for 5)
  (should (flycheck-julia-serverp))
  (flycheck-julia-server-stop)
  (sleep-for 5)
  (kill-buffer "*julia-linter*"))

;; Lint.jl does extensive testing on the correctness of errors, so we only check
;; that querying the server actually works.
(ert-deftest flycheck-julia-test-query ()
  :tags '(query)
  (flycheck-julia-server-start)
  (sleep-for 15)
  (should
   (with-temp-buffer
     (insert-string "\ny\n")
     (ignore-errors
       (flycheck-julia-server-query 'flycheck-julia)
       (sleep-for 15)
       (flycheck-julia-server-query 'flycheck-julia)
       (sleep-for 15)
       (flycheck-julia-server-query 'flycheck-julia)
       (sleep-for 15)
       (flycheck-julia-server-query 'flycheck-julia)
       (sleep-for 15)
       (flycheck-julia-server-query 'flycheck-julia)
       (sleep-for 15)
       (flycheck-julia-server-query 'flycheck-julia))

     ;; some debug stuff:
     ;; Print out the contents of the julia server process buffer
     (sleep-for 15)
     (let ((oldbuf (current-buffer)))
       (set-buffer (get-buffer "*julia-linter*"))
       (message (buffer-substring-no-properties (point-min) (point-max)))
       (set-buffer oldbuf))
     (message (buffer-name))

     ;; check for the error
     ;; (sleep-for 5)
     (let ((retobj (flycheck-julia-server-query 'flycheck-julia)))
       (sleep-for 5)
       (cl-search
        "undeclared symbol"
        (aref (nth 0 retobj) 6)))))
  ;; cleanup
  (sleep-for 5)
  (flycheck-julia-server-stop)
  (sleep-for 5)
  (kill-buffer "*julia-linter*"))

(provide 'flycheck-julia-test)

;;; flycheck-julia-test.el ends here
