# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

BeFoR64 is a pure Fortran 2003+ library for Base64 encoding/decoding of Fortran intrinsic types (integers, reals, characters, and unlimited polymorphic variables) in their binary representation.

## Build Systems

The project supports two build systems:

### FoBiS.py (primary, via `fobos` config file)

```bash
# Initialize submodules first
git submodule update --init

# Build static library (GNU)
FoBiS.py build -mode static-gnu

# Build shared library (GNU)
FoBiS.py build -mode shared-gnu

# Build and run doctests (GNU)
FoBiS.py build -mode tests-gnu

# Debug variants
FoBiS.py build -mode static-gnu-debug
FoBiS.py build -mode tests-gnu-debug

# Intel compiler variants: replace 'gnu' with 'intel'
FoBiS.py build -mode static-intel

# List all available modes
FoBiS.py build -lmodes

# Run coverage analysis (builds tests-gnu-debug with gcov)
FoBiS.py rule -ex makecoverage
```

### fpm (Fortran Package Manager, via `fpm.toml`)

```bash
fpm build
fpm test
```

## Running Tests

After building with `tests-gnu` mode, run all tests:

```bash
bash scripts/run_tests.sh
```

Tests are auto-generated doctests located in `src/tests/befor64/` and `src/tests/befor64_pack_data_m/`. Each test is a standalone program that prints pass/fail results; `run_tests.sh` scans `./exe/` for executables and runs them.

## Source Layout

```
src/lib/
  befor64.F90              # Main library module (public API)
  befor64_pack_data_m.F90  # Auxiliary module: packs heterogeneous data into I1P arrays
src/tests/
  befor64/                 # Doctests for befor64 module
  befor64_pack_data_m/     # Doctests for pack_data module
  validation.py            # Python validation script
src/third_party/PENF/      # Git submodule: Portable ENvironment for Fortran (type kinds)
```

Compiled output goes to `./static/`, `./shared/`, `./exe/`, `./mod/`, `./obj/` depending on mode.

## Architecture

**Module dependency chain**: `befor64` → `befor64_pack_data_m` → `penf`

- **`penf`** (PENF submodule): Provides portable kind parameters: `R8P`, `R4P`, `R16P`, `I8P`, `I4P`, `I2P`, `I1P`, etc. These are used throughout for all numeric type specifications.
- **`befor64_pack_data_m`**: Provides `pack_data` — packs two arrays of differing numeric kinds into a single `integer(I1P)` array using Fortran's `transfer` intrinsic.
- **`befor64`**: Main module. Exports `b64_init`, `is_b64_initialized`, `b64_encode`, `b64_encode_up`, `b64_decode`, `b64_decode_up`, and re-exports `pack_data`.

**Initialization**: `b64_init` must be called before any encode/decode operations. `is_b64_initialized` (module-level logical) tracks this.

**Preprocessing flags** (used via `-cpp` or `-D` options):
- `_R16P`: Enable 128-bit real (quad precision) support — conditionally compiles `b64_encode_R16` / `b64_decode_R16` variants.
- `_ASCII_SUPPORTED`, `_UCS4_SUPPORTED`: Enable character kind variants in PENF.

## Key API Patterns

```fortran
use befor64

call b64_init                                    ! Must call first
call b64_encode(n=12._R8P, code=code64)          ! Scalar real
call b64_encode(n=[12_I4P, 1_I4P], code=code64) ! Integer array
call b64_decode(code=code64, n=val)              ! Decode back

! Unlimited polymorphic variants
call b64_encode_up(n=scalar, code=code64)
call b64_decode_up(code=code64, n=scalar)

! Pack heterogeneous data before encoding
call pack_data(a1=real_array, a2=int_array, packed=i1_array)
call b64_encode(n=i1_array, code=code64)
```

## Documentation

Built with Ford:
```bash
FoBiS.py rule -ex makedoc
# or directly:
ford doc/main_page.md
```
