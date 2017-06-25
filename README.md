flycheck-julia — Flycheck for Julia
===================================

[![License GPL 3][badge-license]][license]
[![MELPA](https://melpa.org/packages/flycheck-julia-badge.svg)](https://melpa.org/#/flycheck-julia)
[![MELPA Stable](https://stable.melpa.org/packages/flycheck-julia-badge.svg)](https://stable.melpa.org/#/flycheck-julia)

Add Julia support to [Flycheck][]:

- Add a `julia` syntax checker using [Lint.jl][]

Installation
------------

Install `flycheck-julia` from [MELPA][] or [MELPA Stable][]
and add the following to your `init.el`:

```elisp
(flycheck-julia-setup)
```

Usage
-----

Simply start linting by enabling `flycheck-mode`. If you use
`flycheck-global-mode` and want `flycheck-julia` enabled automatically, then add
the following to your `init.del`:

```elisp
(add-to-list 'flycheck-global-modes 'julia-mode)
(add-to-list 'flycheck-global-modes 'ess-julia-mode)
```

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

See [`LICENSE`][license] for details.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg?dummy
[LICENSE]: https://github.com/gdkrmr/flycheck-julia/blob/master/LICENSE
[Flycheck]: http://www.flycheck.org
[Lint.jl]: https://github.com/tonyhffong/Lint.jl
[MELPA]: https://melpa.org
[MELPA Stable]: https://stable.melpa.org
