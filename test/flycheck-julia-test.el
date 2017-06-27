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
  (flycheck-julia-server-stop))

(ert-deftest flycheck-julia-kill-server ()
  :tags '(server)
  (flycheck-julia-server-start)
  (sleep-for 5)
  (flycheck-julia-server-stop)
  (sleep-for 5)
  (should (not (flycheck-julia-serverp))))

(ert-deftest flycheck-julia-restart-server ()
  :tags '(server)
  (flycheck-julia-server-start)
  (sleep-for 5)
  (flycheck-julia-server-restart)
  (sleep-for 5)
  (should (flycheck-julia-serverp))
  (flycheck-julia-server-stop))

(provide 'flycheck-julia-test)

;;; flycheck-julia-test.el ends here
