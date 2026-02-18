---
title: Features
---

# Features

## Encoding / Decoding

- [x] Encode real/integer scalar variables
- [x] Decode real/integer scalar variables
- [x] Encode real/integer array variables
- [x] Decode real/integer array variables
- [x] Encode character scalar variables
- [x] Decode character scalar variables
- [x] Encode character array variables
- [x] Decode character array variables
- [x] Encode unlimited polymorphic scalar variables
- [x] Decode unlimited polymorphic scalar variables
- [x] Encode unlimited polymorphic array variables
- [x] Decode unlimited polymorphic array variables

## Packing Heterogeneous Data

Pack two arrays of differing numeric kinds into a single `integer(I1P)` byte stream
before encoding with `b64_encode`:

- [ ] pack integer/integer (different kinds) scalars
- [x] pack integer/integer (different kinds) arrays
- [ ] pack real/real (different kinds) scalars
- [x] pack real/real (different kinds) arrays
- [ ] pack integer/real scalars
- [x] pack integer/real arrays

## Planned

- [ ] Errors trapping mechanism

Any feature request is welcome — open an issue on
[GitHub](https://github.com/szaghi/BeFoR64/issues).
