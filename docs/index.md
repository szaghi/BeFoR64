---
layout: home

hero:
  name: BeFoR64
  text: Base64 for Fortran
  tagline: Pure Fortran 2003+ library for base64 encoding and decoding of intrinsic types
  actions:
    - theme: brand
      text: Guide
      link: /guide/
    - theme: alt
      text: API Reference
      link: /api/
    - theme: alt
      text: View on GitHub
      link: https://github.com/szaghi/BeFoR64

features:
  - icon: 🔢
    title: All intrinsic types
    details: Encode and decode real, integer, and character variables — both scalar and array — of any kind. Unlimited polymorphic variables supported too.
  - icon: 📄
    title: Heterogeneous data
    details: Pack two arrays of different numeric kinds (real/real, real/integer, integer/integer) into a single byte stream before encoding, via the pack_data helper.
  - icon: 🧩
    title: Fortran 2003+ Compliant
    details: Fully standard-compliant library. Tested with GNU (≥ 4.9.2) and Intel (≥ 12.x) compilers.
  - icon: 🆓
    title: Free & Open Source
    details: Multi-licensed — GPLv3 for FOSS projects, BSD 2/3-Clause or MIT for commercial use. Any contributor is welcome.
---

## Quick start

```fortran
use befor64

call b64_init() ! initialise once

! encode a scalar real
character(len=:), allocatable :: code
call b64_encode(n=1.0_R8P, code=code)

! encode an integer array
call b64_encode(n=[12_I4P, 1_I4P], code=code)

! decode back
real(R8P) :: val
call b64_decode(code='AAAAAAAA8D8=', n=val)
```

> `b64_init` must be called at the very beginning of encode/decode operations (just once). The flag
> `is_b64_initialized` can be checked at any time to confirm the library is ready.

## Authors

- Stefano Zaghi — [@szaghi](https://github.com/szaghi)

Contributions are welcome — see the [Contributing](/guide/contributing) page.

## Copyrights

BeFoR64 is distributed under a multi-licensing system:

| Use case | License |
|---|---|
| FOSS projects | [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html) |
| Closed source / commercial | [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause) |
| Closed source / commercial | [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause) |
| Closed source / commercial | [MIT](http://opensource.org/licenses/MIT) |
