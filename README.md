# BeFoR64 — Base64 encoding/decoding library for Fortran

A KISS pure Fortran library for encoding and decoding any intrinsic type — integers, reals, characters, and unlimited polymorphic variables — to and from Base64 strings.

- Pure Fortran (KISS), Fortran 2003+ standard compliant;
- Encodes/decodes scalars and arrays of any numeric or character kind;
- Free and Open Source, multi-licensed.

**[Documentation](https://szaghi.github.io/BeFoR64/)** | **[API Reference](https://szaghi.github.io/BeFoR64/api/)**

---

## Copyrights

BeFoR64 is distributed under a multi-licensing system:

- **FOSS projects**: [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html)
- **Closed source / commercial**: [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause), [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause), or [MIT](http://opensource.org/licenses/MIT)

Anyone interested in using, developing, or contributing to BeFoR64 is welcome — pick the license that best fits your needs.

---

## A taste of BeFoR64

```fortran
use befor64

call b64_init()

character(len=:), allocatable :: code
call b64_encode(n=1.0_R8P, code=code)   ! encode a scalar real
call b64_encode(n=[1_I4P, 2_I4P], code=code)  ! or an integer array

real(R8P) :: val
call b64_decode(code='AAAAAAAA8D8=', n=val)    ! decode back
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

### fpm

```bash
fpm build
fpm test
```

Or add BeFoR64 as a dependency in your `fpm.toml`:

```toml
[dependencies]
BeFoR64 = { git = "https://github.com/szaghi/BeFoR64" }
```

### FoBiS.py

```bash
pip install FoBiS.py
git clone https://github.com/szaghi/BeFoR64 && cd BeFoR64
git submodule update --init
FoBiS.py build -mode static-gnu   # or shared-gnu, tests-gnu, …
bash scripts/run_tests.sh
```
