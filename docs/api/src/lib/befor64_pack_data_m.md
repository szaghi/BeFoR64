---
title: befor64_pack_data_m
---

# befor64_pack_data_m

> KISS library for packing heterogeneous data into single (homogeneous) packed one.

**Source**: `src/lib/befor64_pack_data_m.F90`

**Dependencies**

```mermaid
graph LR
  befor64_pack_data_m["befor64_pack_data_m"] --> penf["penf"]
```

## Contents

- [pack_data](#pack-data)
- [pack_data_R8_R4](#pack-data-r8-r4)
- [pack_data_R8_I8](#pack-data-r8-i8)
- [pack_data_R8_I4](#pack-data-r8-i4)
- [pack_data_R8_I2](#pack-data-r8-i2)
- [pack_data_R8_I1](#pack-data-r8-i1)
- [pack_data_R4_R8](#pack-data-r4-r8)
- [pack_data_R4_I8](#pack-data-r4-i8)
- [pack_data_R4_I4](#pack-data-r4-i4)
- [pack_data_R4_I2](#pack-data-r4-i2)
- [pack_data_R4_I1](#pack-data-r4-i1)
- [pack_data_I8_R8](#pack-data-i8-r8)
- [pack_data_I8_R4](#pack-data-i8-r4)
- [pack_data_I8_I4](#pack-data-i8-i4)
- [pack_data_I8_I2](#pack-data-i8-i2)
- [pack_data_I8_I1](#pack-data-i8-i1)
- [pack_data_I4_R8](#pack-data-i4-r8)
- [pack_data_I4_R4](#pack-data-i4-r4)
- [pack_data_I4_I8](#pack-data-i4-i8)
- [pack_data_I4_I2](#pack-data-i4-i2)
- [pack_data_I4_I1](#pack-data-i4-i1)
- [pack_data_I2_R8](#pack-data-i2-r8)
- [pack_data_I2_R4](#pack-data-i2-r4)
- [pack_data_I2_I8](#pack-data-i2-i8)
- [pack_data_I2_I4](#pack-data-i2-i4)
- [pack_data_I2_I1](#pack-data-i2-i1)
- [pack_data_I1_R8](#pack-data-i1-r8)
- [pack_data_I1_R4](#pack-data-i1-r4)
- [pack_data_I1_I8](#pack-data-i1-i8)
- [pack_data_I1_I4](#pack-data-i1-i4)
- [pack_data_I1_I2](#pack-data-i1-i2)

## Interfaces

### pack_data

Pack different kinds of data into single I1P array.

 This is useful for encoding different (heterogeneous) kinds variables into a single (homogeneous) stream of bits.
 @note This procedure exploits the `transfer` builtin function, that from the standard (2003+) is defined as
 `TRANSFER(SOURCE, MOLD [, SIZE])`. Data object having a physical representation identical to that of `SOURCE` but with the type
 and type parameters of `MOLD`. The result is of the same type and type parameters as `MOLD`.
 If `MOLD` is an array and `SIZE` is absent, the result is an array and of rank one. Its size is as small as possible such
 that its physical representation is not shorter than that of `SOURCE`.

 Presently, the following combinations are available:

* [ ] Arrays-Arrays:
    * [X] real(any)-real(any);
    * [X] real(any)-integer(any);
    * [X] integer(any)-integer(any);
    * [X] integer(any)-real(any);
    * [ ] real(any)-character;
    * [ ] character-real(any);
    * [ ] integer(any)-character;
    * [ ] character-integer(any);
* [ ] Scalars-Scalars:
    * [ ] real(any)-real(any);
    * [ ] real(any)-integer(any);
    * [ ] integer(any)-integer(any);
    * [ ] integer(any)-real(any);
    * [ ] real(any)-character;
    * [ ] character-real(any);
    * [ ] integer(any)-character;
    * [ ] character-integer(any);

### Examples of usage

#### Packing two real arrays, one with kind R8P and one with R4P
```
real(R8P)::                 array_r8(1:12)
real(R4P)::                 array_r4(-1:5)
integer(I1P), allocatable:: rpack
...
call pack_data(a1=array_r8,a2=array_r4,packed=rpack)
```
#### Packing two arrays, one real with kind R4P and one integer with I4P
```
real(R4P)::                 array_r4(2)
integer(I4P)::              array_i4(0:2)
integer(I1P), allocatable:: rpack
...
call pack_data(a1=array_r4,a2=array_i4,packed=rpack)
```

**Module procedures**: [`pack_data_R8_R4`](/api/src/lib/befor64_pack_data_m#pack-data-r8-r4), [`pack_data_R8_I8`](/api/src/lib/befor64_pack_data_m#pack-data-r8-i8), [`pack_data_R8_I4`](/api/src/lib/befor64_pack_data_m#pack-data-r8-i4), [`pack_data_R8_I2`](/api/src/lib/befor64_pack_data_m#pack-data-r8-i2), [`pack_data_R8_I1`](/api/src/lib/befor64_pack_data_m#pack-data-r8-i1), [`pack_data_R4_R8`](/api/src/lib/befor64_pack_data_m#pack-data-r4-r8), [`pack_data_R4_I8`](/api/src/lib/befor64_pack_data_m#pack-data-r4-i8), [`pack_data_R4_I4`](/api/src/lib/befor64_pack_data_m#pack-data-r4-i4), [`pack_data_R4_I2`](/api/src/lib/befor64_pack_data_m#pack-data-r4-i2), [`pack_data_R4_I1`](/api/src/lib/befor64_pack_data_m#pack-data-r4-i1), [`pack_data_I8_R8`](/api/src/lib/befor64_pack_data_m#pack-data-i8-r8), [`pack_data_I8_R4`](/api/src/lib/befor64_pack_data_m#pack-data-i8-r4), [`pack_data_I8_I4`](/api/src/lib/befor64_pack_data_m#pack-data-i8-i4), [`pack_data_I8_I2`](/api/src/lib/befor64_pack_data_m#pack-data-i8-i2), [`pack_data_I8_I1`](/api/src/lib/befor64_pack_data_m#pack-data-i8-i1), [`pack_data_I4_R8`](/api/src/lib/befor64_pack_data_m#pack-data-i4-r8), [`pack_data_I4_R4`](/api/src/lib/befor64_pack_data_m#pack-data-i4-r4), [`pack_data_I4_I8`](/api/src/lib/befor64_pack_data_m#pack-data-i4-i8), [`pack_data_I4_I2`](/api/src/lib/befor64_pack_data_m#pack-data-i4-i2), [`pack_data_I4_I1`](/api/src/lib/befor64_pack_data_m#pack-data-i4-i1), [`pack_data_I2_R8`](/api/src/lib/befor64_pack_data_m#pack-data-i2-r8), [`pack_data_I2_R4`](/api/src/lib/befor64_pack_data_m#pack-data-i2-r4), [`pack_data_I2_I8`](/api/src/lib/befor64_pack_data_m#pack-data-i2-i8), [`pack_data_I2_I4`](/api/src/lib/befor64_pack_data_m#pack-data-i2-i4), [`pack_data_I2_I1`](/api/src/lib/befor64_pack_data_m#pack-data-i2-i1), [`pack_data_I1_R8`](/api/src/lib/befor64_pack_data_m#pack-data-i1-r8), [`pack_data_I1_R4`](/api/src/lib/befor64_pack_data_m#pack-data-i1-r4), [`pack_data_I1_I8`](/api/src/lib/befor64_pack_data_m#pack-data-i1-i8), [`pack_data_I1_I4`](/api/src/lib/befor64_pack_data_m#pack-data-i1-i4), [`pack_data_I1_I2`](/api/src/lib/befor64_pack_data_m#pack-data-i1-i2)

## Subroutines

### pack_data_R8_R4

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 real(R8P)                 :: a1(1)
 real(R4P)                 :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(size(pack, dim=1))
```

**Attributes**: pure

```fortran
subroutine pack_data_R8_R4(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | real(kind=[R8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Firs data stream. |
| `a2` | real(kind=[R4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_R8_I8

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 real(R8P)                 :: a1(1)
 integer(I8P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(9)
```

**Attributes**: pure

```fortran
subroutine pack_data_R8_I8(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | real(kind=[R8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_R8_I4

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 real(R8P)                 :: a1(1)
 integer(I4P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(9)
```

**Attributes**: pure

```fortran
subroutine pack_data_R8_I4(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | real(kind=[R8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_R8_I2

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 real(R8P)                 :: a1(1)
 integer(I2P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(9)
```

**Attributes**: pure

```fortran
subroutine pack_data_R8_I2(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | real(kind=[R8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I2P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_R8_I1

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 real(R8P)                 :: a1(1)
 integer(I1P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(9)
```

**Attributes**: pure

```fortran
subroutine pack_data_R8_I1(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | real(kind=[R8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_R4_R8

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 real(R4P)                 :: a1(1)
 real(R8P)                 :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(size(pack, dim=1))
```

**Attributes**: pure

```fortran
subroutine pack_data_R4_R8(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | real(kind=[R4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Firs data stream. |
| `a2` | real(kind=[R8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_R4_I8

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 real(R4P)                 :: a1(1)
 integer(I8P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(5)
```

**Attributes**: pure

```fortran
subroutine pack_data_R4_I8(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | real(kind=[R4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_R4_I4

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 real(R4P)                 :: a1(1)
 integer(I4P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(5)
```

**Attributes**: pure

```fortran
subroutine pack_data_R4_I4(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | real(kind=[R4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_R4_I2

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 real(R4P)                 :: a1(1)
 integer(I2P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(5)
```

**Attributes**: pure

```fortran
subroutine pack_data_R4_I2(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | real(kind=[R4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I2P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_R4_I1

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 real(R4P)                 :: a1(1)
 integer(I1P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(5)
```

**Attributes**: pure

```fortran
subroutine pack_data_R4_I1(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | real(kind=[R4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I8_R8

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I8P)              :: a1(1)
 real(R8P)                 :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(size(pack, dim=1))
```

**Attributes**: pure

```fortran
subroutine pack_data_I8_R8(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | real(kind=[R8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I8_R4

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I8P)              :: a1(1)
 real(R4P)                 :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(size(pack, dim=1))
```

**Attributes**: pure

```fortran
subroutine pack_data_I8_R4(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | real(kind=[R4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I8_I4

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I8P)              :: a1(1)
 integer(I4P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(9)
```

**Attributes**: pure

```fortran
subroutine pack_data_I8_I4(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I8_I2

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I8P)              :: a1(1)
 integer(I2P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(9)
```

**Attributes**: pure

```fortran
subroutine pack_data_I8_I2(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I2P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I8_I1

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I8P)              :: a1(1)
 integer(I1P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(9)
```

**Attributes**: pure

```fortran
subroutine pack_data_I8_I1(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I4_R8

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I4P)              :: a1(1)
 real(R8P)                 :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(size(pack, dim=1))
```

**Attributes**: pure

```fortran
subroutine pack_data_I4_R8(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | real(kind=[R8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I4_R4

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I4P)              :: a1(1)
 real(R4P)                 :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(size(pack, dim=1))
```

**Attributes**: pure

```fortran
subroutine pack_data_I4_R4(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | real(kind=[R4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I4_I8

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I4P)              :: a1(1)
 integer(I8P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(5)
```

**Attributes**: pure

```fortran
subroutine pack_data_I4_I8(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I4_I2

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I4P)              :: a1(1)
 integer(I2P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(5)
```

**Attributes**: pure

```fortran
subroutine pack_data_I4_I2(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I2P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I4_I1

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I4P)              :: a1(1)
 integer(I1P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(5)
```

**Attributes**: pure

```fortran
subroutine pack_data_I4_I1(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I2_R8

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I2P)              :: a1(1)
 real(R8P)                 :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(size(pack, dim=1))
```

**Attributes**: pure

```fortran
subroutine pack_data_I2_R8(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I2P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | real(kind=[R8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I2_R4

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I2P)              :: a1(1)
 real(R4P)                 :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(size(pack, dim=1))
```

**Attributes**: pure

```fortran
subroutine pack_data_I2_R4(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I2P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | real(kind=[R4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I2_I8

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I2P)              :: a1(1)
 integer(I8P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(3)
```

**Attributes**: pure

```fortran
subroutine pack_data_I2_I8(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I2P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I2_I4

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I2P)              :: a1(1)
 integer(I4P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(3)
```

**Attributes**: pure

```fortran
subroutine pack_data_I2_I4(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I2P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I2_I1

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I2P)              :: a1(1)
 integer(I1P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(3)
```

**Attributes**: pure

```fortran
subroutine pack_data_I2_I1(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I2P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I1_R8

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I1P)              :: a1(1)
 real(R8P)                 :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(size(pack, dim=1))
```

**Attributes**: pure

```fortran
subroutine pack_data_I1_R8(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | real(kind=[R8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I1_R4

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I1P)              :: a1(1)
 real(R4P)                 :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(size(pack, dim=1))
```

**Attributes**: pure

```fortran
subroutine pack_data_I1_R4(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | real(kind=[R4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I1_I8

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I1P)              :: a1(1)
 integer(I8P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(2)
```

**Attributes**: pure

```fortran
subroutine pack_data_I1_I8(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I8P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I1_I4

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I1P)              :: a1(1)
 integer(I4P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(2)
```

**Attributes**: pure

```fortran
subroutine pack_data_I1_I4(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I4P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |

### pack_data_I1_I2

Pack different kinds of data into single I1P array.

```fortran
 use befor64
 use penf
 integer(I1P)              :: a1(1)
 integer(I2P)              :: a2(1)
 integer(I1P), allocatable :: pack(:)
 a1(1) = 0
 a2(1) = 1
 call pack_data(a1=a1, a2=a2, packed=pack)
 print *, pack(2)
```

**Attributes**: pure

```fortran
subroutine pack_data_I1_I2(a1, a2, packed)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a1` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | First data stream. |
| `a2` | integer(kind=[I2P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | in |  | Second data stream. |
| `packed` | integer(kind=[I1P](/api/src/third_party/PENF/src/lib/penf_global_parameters_variables)) | inout | allocatable | Packed data into I1P array. |
