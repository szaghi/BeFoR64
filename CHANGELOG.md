# Changelog

All notable changes to this project are documented here.
Versions follow [Semantic Versioning](https://semver.org/).
Format follows [Keep a Changelog](https://keepachangelog.com/).

## [1.2.2] — 2026-10-04
### Fixed
- **tests**: Make the R16P doctests hold with quadruple precision

- **build**: Sync fpm.toml version and guard tracked doctests in CI

- **build**: Track PENF default branch in fpm manifest

- **scripts**: Check doctest output against expected results


## [1.2.1] — 2026-10-02
### Fixed
- **cmake**: Load PENF from the package config and fix the version


## [1.2.0] — 2026-10-02
### Fixed
- **fobos**: Repeat --exclude_from_doctests flag for each excluded file

- **fobos**: Remove redundant gcov rule that fails on unexpanded glob

- **docs**: Untrack package-lock and pin esbuild for lock-free vite build

- **befor64**: Rename _R16P guard to PENF_R16P for quad generics


## [1.1.14] — 2026-02-27
### Documentation
- **readme**: Overhaul hero table and install section


### Fixed
- **deps**: Correct FoBiS dependency config key from dependon to src


## [1.1.11] — 2026-02-22
### Documentation
- **guide**: Add coverage analysis page to Project section


## [1.1.10] — 2026-02-21
### Documentation
- **befor64**: Fix typo in fortran code fence markers


## [1.1.9] — 2026-02-21
### Documentation
- **pack_data**: Fix typo in fortran code fence markers


## [1.1.7] — 2026-02-18
### Fixed
- **docs**: Use absolute path for contributing link in landing page


## [1.1.5] — 2026-02-18
### Documentation
- Add VitePress site with guide pages, landing page, and API ref


### Fixed
- **len overflow**: Fix bug in issue #14



