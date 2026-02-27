---
title: Installation
---

# Installation

## Requirements

A modern Fortran compiler with Fortran 2003+ support.

Dependencies live in `src/third_party/` and are fetched automatically
by each build path below.

## Option 1 — fpm (recommended)

With [Fortran Package Manager](https://fpm.fortran-lang.org) no manual setup is needed:

```bash
git clone https://github.com/szaghi/BeFoR64
cd BeFoR64
fpm build
fpm test
```

To use BeFoR64 as a dependency in your own fpm project, add to your `fpm.toml`:

```toml
[dependencies]
BeFoR64.git = "https://github.com/szaghi/BeFoR64"
```

## Option 2 — FoBiS.py

[FoBiS.py](https://github.com/szaghi/FoBiS) reads the `fobos` file at the repository
root and handles all inter-module dependencies automatically.

```bash
pip install FoBiS.py

git clone https://github.com/szaghi/BeFoR64
cd BeFoR64
FoBiS.py fetch
```

### Building the library

```bash
# Static library (release)
FoBiS.py build -mode static-gnu

# Shared library (release)
FoBiS.py build -mode shared-gnu

# Debug variants
FoBiS.py build -mode static-gnu-debug
FoBiS.py build -mode shared-gnu-debug
```

Output is written to `./static/` or `./shared/`; `.mod` files go to `./mod/`.

Replace `gnu` with `intel` for the Intel compiler (`static-intel`, etc.).

### Building and running the doctests

```bash
FoBiS.py build -mode tests-gnu
bash scripts/run_tests.sh
```

### Listing all available modes

```bash
FoBiS.py build -lmodes
```

### Coverage analysis

```bash
FoBiS.py rule -ex makecoverage
```

### Generating API documentation

Requires [Ford](https://github.com/cmacmackin/ford):

```bash
pip install ford
FoBiS.py rule -ex makedoc
```
