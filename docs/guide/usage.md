---
title: Usage
---

# Usage

## Setup

Import the main module and initialise the library once before any encode/decode call:

```fortran
use befor64

call b64_init()
```

The module-level flag `is_b64_initialized` can be queried at any time:

```fortran
if (.not. is_b64_initialized) call b64_init()
```

## Encoding

All encode calls return an allocatable character string (`character(len=:), allocatable`).
The compiler must support this Fortran 2003 feature.

### Scalar real

```fortran
character(len=:), allocatable :: code

call b64_encode(n=12._R8P, code=code)
print '(A)', code   ! AAAAAAAA8D8=
```

### Integer array

```fortran
character(len=:), allocatable :: code

call b64_encode(n=[12_I4P, 1_I4P], code=code)
print '(A)', code
```

### Character scalar

```fortran
character(len=:), allocatable :: code

call b64_encode(n='hello', code=code)
```

### Unlimited polymorphic

```fortran
class(*), allocatable :: val
character(len=:), allocatable :: code

allocate(val, source=3.14_R8P)
call b64_encode_up(n=val, code=code)
```

## Decoding

You must know the type of the encoded data ahead of time.

### Scalar real

```fortran
real(R8P) :: decoded

call b64_decode(code='AAAAAAAA8D8=', n=decoded)
```

### Integer array

```fortran
integer(I8P) :: decoded(1:4)

call b64_decode(code='FwAAAAAAAABEAQAAAAAAABBwhAEAAAAAAgAAAAAAAAA=', n=decoded)
```

### Unlimited polymorphic

```fortran
class(*), allocatable :: val

allocate(real(R8P) :: val)
call b64_decode_up(code=code, n=val)
```

## Encoding Heterogeneous Data

To encode two arrays of different kinds together, first pack them with `pack_data`,
then pass the resulting byte array to `b64_encode`:

```fortran
real(R8P)               :: a(1:12)
real(R4P)               :: b(-1:5)
integer(I1P), allocatable :: packed(:)
character(len=:), allocatable :: code

call pack_data(a1=a, a2=b, packed=packed)
call b64_encode(n=packed, code=code)
```

Supported array combinations for `pack_data`:

| a1 kind  | a2 kind  |
|----------|----------|
| real(R8P)| real(R4P), integer(I8P/I4P/I2P/I1P) |
| real(R4P)| real(R8P), integer(I8P/I4P/I2P/I1P) |
| integer(any) | real(any), integer(any) |

> For heterogeneous **scalar** packing and character mixing, see the
> [Features](features) page — those are not yet implemented.

## Module Summary

| Symbol | Description |
|--------|-------------|
| `b64_init` | Initialise the library (required before first use) |
| `is_b64_initialized` | Logical flag — true after `b64_init` |
| `b64_encode` | Encode any intrinsic type to a Base64 string |
| `b64_encode_up` | Encode unlimited polymorphic variable |
| `b64_decode` | Decode a Base64 string to an intrinsic type |
| `b64_decode_up` | Decode to unlimited polymorphic variable |
| `pack_data` | Pack two numeric arrays of different kinds into `I1P` bytes |
