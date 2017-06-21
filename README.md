flycheck-julia — Flycheck for Julia
===================================

[![License GPL 3][badge-license]][license]

Add Julia support to [Flycheck][]:

- Add a `julia` syntax checker using [Lint.jl][]

Installation
------------

For now manual.

Usage
-----

Just use Flycheck as usual in Julia Mode buffers. Flycheck will automatically
use the `flycheck-julia` syntax checker if Julia Mode is enabled.

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
[LICENSE]: https://github.com/gdkrmr/flycheck-julia/blob/master/COPYING
[Flycheck]: http://www.flycheck.org
[Lint.jl]: https://github.com/tonyhffong/Lint.jl
