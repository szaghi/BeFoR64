# BeFoR64

>#### Base64 encoding/decoding library for Fortran
> A KISS pure Fortran 2003+ library for encoding and decoding any intrinsic type — integers, reals, characters, and unlimited polymorphic variables — to and from Base64 strings.

[![GitHub tag](https://img.shields.io/github/v/tag/szaghi/BeFoR64)](https://github.com/szaghi/BeFoR64/tags)
[![GitHub issues](https://img.shields.io/github/issues/szaghi/BeFoR64)](https://github.com/szaghi/BeFoR64/issues)
[![CI](https://github.com/szaghi/BeFoR64/actions/workflows/ci.yml/badge.svg)](https://github.com/szaghi/BeFoR64/actions/workflows/ci.yml)
[![coverage](https://img.shields.io/endpoint?url=https://szaghi.github.io/BeFoR64/coverage.json)](https://github.com/szaghi/BeFoR64/actions/workflows/ci.yml)

| 🔢 **All intrinsic types**<br>Integers `I1P`–`I8P`, reals `R4P`/`R8P`/opt. `R16P`, characters, unlimited polymorphic | 📐 **Scalars & arrays**<br>Scalar and rank-1 array support for all types | 🔗 **Pack mixed data**<br>Heterogeneous arrays via `pack_data` | 🔄 **Bidirectional**<br>Symmetric encode and decode |
|:---:|:---:|:---:|:---:|
| 🎯 **KISS API**<br>`b64_init`, `b64_encode`, `b64_decode` — that's it | ⚡ **Pure Fortran 2003+**<br>No C, no external deps; tested with gfortran & ifort | 🔧 **Two build systems**<br>fpm and FoBiS.py — static, shared, and test modes | 🔓 **Open & documented**<br>GPL v3 · BSD · MIT; full API reference & usage guide |

For full documentations (guide, tutorial, examples, etc...) see the [BeFoR64 website](https://szaghi.github.io/BeFoR64/).

---

## Authors

- Stefano Zaghi — [@szaghi](https://github.com/szaghi)

Contributions are welcome — see the [Contributing](/guide/contributing) page.

## Copyrights

This project is distributed under a multi-licensing system:

- **FOSS projects**: [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html)
- **Closed source / commercial**: [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause), [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause), or [MIT](http://opensource.org/licenses/MIT)

> Anyone interested in using, developing, or contributing to this project is welcome — pick the license that best fits your needs.

---

## A taste of BeFoR64

```fortran
use befor64

call b64_init()

character(len=:), allocatable :: code
call b64_encode(n=1.0_R8P, code=code)        ! encode a scalar real
call b64_encode(n=[1_I4P, 2_I4P], code=code) ! or an integer array

real(R8P) :: val
call b64_decode(code='AAAAAAAA8D8=', n=val)  ! decode back
```

---

## Usage

BeFoR64 exposes four public procedures and one flag:

| Symbol | Description |
|--------|-------------|
| `b64_init` | Initialise the library — call once before any encode/decode |
| `is_b64_initialized` | Logical flag, true after `b64_init` |
| `b64_encode` / `b64_encode_up` | Encode intrinsic or unlimited polymorphic variable to Base64 |
| `b64_decode` / `b64_decode_up` | Decode a Base64 string back to an intrinsic or polymorphic variable |
| `pack_data` | Pack two numeric arrays of different kinds into a byte stream for mixed-type encoding |

Encoded strings are returned as `character(len=:), allocatable` — a Fortran 2003 feature required by the compiler.

```fortran
! heterogeneous data: pack first, then encode
real(R8P)               :: a(12)
real(R4P)               :: b(7)
integer(I1P), allocatable :: packed(:)
character(len=:), allocatable :: code

call pack_data(a1=a, a2=b, packed=packed)
call b64_encode(n=packed, code=code)
```

See the full [Usage guide](https://szaghi.github.io/BeFoR64/guide/usage) for all supported type combinations.

---

## Install

### FoBiS.py

**Standalone** — clone, build, and install in one command:

```bash
FoBiS.py install szaghi/BeFoR64 -mode static-gnu
FoBiS.py install szaghi/BeFoR64 -mode static-gnu --prefix /path/to/prefix
```

**As a project dependency** — declare BeFoR64 in your `fobos` and run `fetch`:

```ini
[dependencies]
deps_dir = src/third_party
PENF     = https://github.com/szaghi/BeFoR64
```

```bash
FoBiS.py fetch           # fetch and build
FoBiS.py fetch --update  # re-fetch and rebuild
```

### fpm

Add to your `fpm.toml`:

```toml
[dependencies]
BeFoR64 = { git = "https://github.com/szaghi/BeFoR64" }
```

### CMake

```bash
cmake -B build && cmake --build build
```

### Makefile

```bash
make              # static library
make TESTS=yes    # build and run tests
```
