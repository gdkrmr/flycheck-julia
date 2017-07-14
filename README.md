flycheck-julia — Flycheck for Julia
===================================

[![License GPL 3][badge-license]][license]
[![MELPA](https://melpa.org/packages/flycheck-julia-badge.svg)](https://melpa.org/#/flycheck-julia)
[![MELPA Stable](https://stable.melpa.org/packages/flycheck-julia-badge.svg)](https://stable.melpa.org/#/flycheck-julia)
[![Build master](https://api.travis-ci.org/gdkrmr/flycheck-julia.svg?branch=master)](https://travis-ci.org/gdkrmr/flycheck-julia)

- Add a [Julia][] syntax checker for [Emacs][] and [Flycheck][] using [Lint.jl][]

Installation
=====

General instructions
-----

- Install [Lint.jl][] in [Julia][]

  Open julia and run the following commands:
  ```julia
  Pkg.update()
  Pkg.add("Lint")
  ```

- From [MELPA][] or [MELPA Stable][] install the following packages:

  - [ess][] or [julia-mode][].

  - [flycheck][], detailed instructions can be
    found [here](http://www.flycheck.org/en/latest/user/installation.html).

  - [flycheck-julia][].

- Add the following to your configuration:
  ```elisp
  (flycheck-julia-setup)
  ```

Installing from a fresh emacs install
-----

- Setup your package manager

  Add the following to
  your
  [init file](http://www.flycheck.org/en/latest/glossary.html#term-init-file):

  ```elisp
  (require 'package)
  (add-to-list 'package-archives '("MELPA Stable" . "https://stable.melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (package-initialize)
  ```

- Install the required packages

  Restart emacs and run `M-x list-packages` or choose `Options -> Manage
  Packages` from the menu and install `flycheck`, `ess`, and `flycheck-julia`.

- Configure emacs for the use with `julia`, `flycheck`, and `flycheck-julia`

  Add the following lines at the end of your init file:

  ```elisp
  ;; loads ess, which contains ess-julia-mode
  (require 'ess-site)
  ;; enable flycheck globally
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; tell flycheck about the julia linter
  (flycheck-julia-setup)
  ```

Installing when using [Spacemacs][]
-----

- Add the following to your `.spacemacs`:
  - the [syntax-checking][] and [ess][] layers.
  - to `dotspacemacs-additional-packages` add `flycheck-julia`
  - to the `dotspacemacs/user-config` function add the following lines:
    ```elisp
    (flycheck-julia-setup)
    (add-to-list 'flycheck-global-modes 'julia-mode)
    (add-to-list 'flycheck-global-modes 'ess-julia-mode)
    ```
- Restart Emacs, this should automatically install `ess`, `flycheck`, and `flycheck-julia`

Usage
-----

If you configured your Emacs with the instructions above, linting
of Julia files should start automatically. If you did not enable
`global-flycheck-mode`, you can enable linting of Julia files by enabling
`flycheck-mode`.

Interaction with errors is done through `flycheck`, see
the [manual](http://www.flycheck.org/en/latest/user/quickstart.html) for
details.

License
-------

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

See [LICENSE][] for details.

[Spacemacs]: https://spacemacs.org
[Emacs]: https://www.gnu.org/software/emacs/
[flycheck-julia]: https://github.com/gdkrmr/flycheck-julia
[Julia]: https://julialang.org
[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg?dummy
[LICENSE]: https://github.com/gdkrmr/flycheck-julia/blob/master/LICENSE
[Flycheck]: http://www.flycheck.org
[Lint.jl]: https://github.com/tonyhffong/Lint.jl
[MELPA]: https://melpa.org
[MELPA Stable]: https://stable.melpa.org
[ess]: http://ess.r-project.org/Manual/ess.html#Installation
[julia-mode]: https://github.com/JuliaEditorSupport/julia-emacs/blob/master/julia-mode.el
[syntax-checking]: http://spacemacs.org/layers/+checkers/syntax-checking/README.html
[ess]: http://spacemacs.org/layers/+lang/ess/README.html
