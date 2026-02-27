# Changelog
## [v1.1.15](https://github.com/szaghi/PENF/tree/v1.1.15) (2026-02-27)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.1.14...v1.1.15)
### CI/CD
- Move dep fetch after build env setup ([`0b6786f`](https://github.com/szaghi/PENF/commit/0b6786f4f162997eec69c460f79e6bf945b1c32d))

## [v1.1.14](https://github.com/szaghi/PENF/tree/v1.1.14) (2026-02-27)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.1.13...v1.1.14)
### Bug fixes
- Correct FoBiS dependency config key from dependon to src ([`33d6189`](https://github.com/szaghi/PENF/commit/33d618944a0fda90b37b174fc6d2def16cd3ca8e))

### CI/CD
- Extract coverage into composite action and guard fetch on deps file ([`da65cb6`](https://github.com/szaghi/PENF/commit/da65cb68aaf3cc53a91bbb6e181077b2c8827b64))

### Documentation
- Overhaul hero table and install section ([`4352f66`](https://github.com/szaghi/PENF/commit/4352f66c443c5fe4870f54e2cc42271187fc0596))

## [v1.1.13](https://github.com/szaghi/PENF/tree/v1.1.13) (2026-02-27)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.1.12...v1.1.13)
### CI/CD
- Add FoBiS.py fetch step and remove legacy CI artefacts ([`4f4c389`](https://github.com/szaghi/PENF/commit/4f4c3898a18b245d58b43743453c2c88139a19fd))

## [v1.1.12](https://github.com/szaghi/PENF/tree/v1.1.12) (2026-02-27)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.1.11...v1.1.12)
### CI/CD
- Replace Codecov with self-hosted coverage badge via GitHub Pages ([`a7b0971`](https://github.com/szaghi/PENF/commit/a7b097196a44b66a19fbaf3ecef4319ddf4d149c))

### Miscellaneous
- Replace PENF git submodule with FoBiS dependency management ([`2df9870`](https://github.com/szaghi/PENF/commit/2df98706dfcda0a436e2ce1eb6eab814096a45aa))

## [v1.1.11](https://github.com/szaghi/PENF/tree/v1.1.11) (2026-02-22)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.1.10...v1.1.11)
### Documentation
- Add coverage analysis page to Project section ([`73e3dce`](https://github.com/szaghi/PENF/commit/73e3dced04c69941d6b8466de4de1f1856c27c58))

## [v1.1.10](https://github.com/szaghi/PENF/tree/v1.1.10) (2026-02-21)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.1.9...v1.1.10)
### Documentation
- Fix typo in fortran code fence markers ([`f721f13`](https://github.com/szaghi/PENF/commit/f721f13107ab1bc97e581dcaab16168a0d11d887))

## [v1.1.9](https://github.com/szaghi/PENF/tree/v1.1.9) (2026-02-21)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.1.8...v1.1.9)
### Documentation
- Fix typo in fortran code fence markers ([`d028b8c`](https://github.com/szaghi/PENF/commit/d028b8c7b51cd1855629fd449ad95edf377cc6f1))

## [v1.1.8](https://github.com/szaghi/PENF/tree/v1.1.8) (2026-02-18)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.1.7...v1.1.8)
### Miscellaneous
- Remove legacy ford main_page.md ([`92ffb69`](https://github.com/szaghi/PENF/commit/92ffb694cc6d6ab86d23b8ae45f421903b659f18))

## [v1.1.7](https://github.com/szaghi/PENF/tree/v1.1.7) (2026-02-18)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.1.6...v1.1.7)
### Bug fixes
- Use absolute path for contributing link in landing page ([`0612fc5`](https://github.com/szaghi/PENF/commit/0612fc5ce10f6dd982dbbd215cc58b1ce2d7ea9e))

## [v1.1.6](https://github.com/szaghi/PENF/tree/v1.1.6) (2026-02-18)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.1.5...v1.1.6)
### CI/CD
- Add GitHub Actions workflows and refactor README ([`3d9227b`](https://github.com/szaghi/PENF/commit/3d9227b742ba3daa7fab275cef1404990547fed9))

## [v1.1.5](https://github.com/szaghi/PENF/tree/v1.1.5) (2026-02-18)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.1.4...v1.1.5)
### Bug fixes
- Fix bug in issue [#14](https://github.com/szaghi/PENF/issues/14) ([`9ab3664`](https://github.com/szaghi/PENF/commit/9ab36643225b134faf77c0864ed88537871da04a))

### Documentation
- Add VitePress site with guide pages, landing page, and API ref ([`109d4b1`](https://github.com/szaghi/PENF/commit/109d4b1519c3850f26d93549b5559f97dab07ff1))

### Miscellaneous
- Merge tag 'v1.1.4' into develop

Trim out anacronistic R16P support check

Trim out anacronistic R16P support check

Why:

All modern computers/compilers support R16P kind precision, no reason to check for
it anymore. ([`41aaf36`](https://github.com/szaghi/PENF/commit/41aaf36a49b7e3690149d4257a245014002a81e9))
- Add initial support of cmake ([`42f5196`](https://github.com/szaghi/PENF/commit/42f519695780fa9e9851ce5f05491c872ecf0943))
- Merge pull request [#12](https://github.com/szaghi/PENF/issues/12) from kostyfisik/cmake

Cmake ([`2e92eec`](https://github.com/szaghi/PENF/commit/2e92eec330bb6fd2fd0a687181c8db04804318c8))
- Update submodules ([`bc8a9ff`](https://github.com/szaghi/PENF/commit/bc8a9ffab4b8b94984e8f12d58f33087c4084280))
- Merge branch 'master' into develop ([`fddde48`](https://github.com/szaghi/PENF/commit/fddde48e740c2dcdeb22cddbd4ccd8e9678a3fd7))
- Adapt CMake from PENF ([`e8b964e`](https://github.com/szaghi/PENF/commit/e8b964e6eb12c19a473e6ff8e08d47903ebc0ff8))
- Add missing files ([`e95c39b`](https://github.com/szaghi/PENF/commit/e95c39b7a785bf99a2ee3bd07d483696312155b5))
- Merge pull request [#13](https://github.com/szaghi/PENF/issues/13) from kostyfisik/cmake

Cmake ([`c8f28f7`](https://github.com/szaghi/PENF/commit/c8f28f7a74513db1e8471be611d42874ad29a906))
- Update submodule ([`8c3001c`](https://github.com/szaghi/PENF/commit/8c3001c1b85b6e2809328930d752da46ed0ceddf))
- Merge branch 'master' into develop ([`0b920cd`](https://github.com/szaghi/PENF/commit/0b920cd46656c0b913b71262a68eee84c61ff488))
- Re-add pre processing flag for R16P unsupported

Re-add pre processing flag for R16P unsupported. ([`e222ede`](https://github.com/szaghi/PENF/commit/e222ede7f1207f74acafd30c6c6e1d3585b5bb74))
- Add fpm support ([`8ebbb9e`](https://github.com/szaghi/PENF/commit/8ebbb9ef492890b9b53bfeccf7392572f4370501))
- Merge pull request [#15](https://github.com/szaghi/PENF/issues/15) from zoziha/add-fpm-support

Add fpm support ([`09b95c0`](https://github.com/szaghi/PENF/commit/09b95c08a2ee8995d19cf8551c22b783193f6246))
- Update submodules ([`e18fbc6`](https://github.com/szaghi/PENF/commit/e18fbc6e916359343c45b363baa762f20aee830a))

## [v1.1.4](https://github.com/szaghi/PENF/tree/v1.1.4) (2019-09-11)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.1.3...v1.1.4)
### Miscellaneous
- Extract doctests as standalone ([`879f39e`](https://github.com/szaghi/PENF/commit/879f39e023b4940b2d0b82fa1fd358c89372a89e))
- Amend .travis.yml ([`2f82f01`](https://github.com/szaghi/PENF/commit/2f82f01d595fa68a3f3b044eae6a2d4189086ebd))
- Update submodules ([`7355ddc`](https://github.com/szaghi/PENF/commit/7355ddc2cd1b7ae79023968ac68bbbdc9174121d))
- Merge tag 'v1.1.3' into develop

Improve coverage by means of doctests

Stable release, fully backward compatible. ([`8f83520`](https://github.com/szaghi/PENF/commit/8f83520cf64697d9d517f3e805f3f4f89db2cd80))
- Merge branch 'master' into develop ([`b0a3fbd`](https://github.com/szaghi/PENF/commit/b0a3fbd41734177a7e165d9ec354e8a5eae872a9))
- Update submodules ([`c959a52`](https://github.com/szaghi/PENF/commit/c959a522b364d2b7a140278b153aa958c88b5506))
- Trim out anacronistic R16P support check

Trim out anacronistic R16P support check

Why:

All modern computers/compilers support R16P kind precision, no reason to check for
it anymore. ([`fefaed7`](https://github.com/szaghi/PENF/commit/fefaed7caba53eeb0173c8c84ab9a91c0a0eb06d))
- Update travis config ([`5738aca`](https://github.com/szaghi/PENF/commit/5738aca1b5a171c6d574fea1002492cd7860fda4))
- Merge branch 'release/1.1.4' ([`d2be41f`](https://github.com/szaghi/PENF/commit/d2be41faa804c5b1b811351c5384cdb6c58ce431))

## [v1.1.3](https://github.com/szaghi/PENF/tree/v1.1.3) (2017-06-26)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.1.2...v1.1.3)
### Miscellaneous
- Resolve git merge conflicts ([`b463b4c`](https://github.com/szaghi/PENF/commit/b463b4c9f27be127872afc504b0b45e42752db55))
- Merge branch 'master' into develop ([`bde4e1e`](https://github.com/szaghi/PENF/commit/bde4e1ed0cc1664709dfe90e2d4ad85f2168f671))
- Merge branch 'release/1.1.3' ([`96753b8`](https://github.com/szaghi/PENF/commit/96753b84a672de3f3eadafa45ac0d9ad23867ba8))

## [v1.1.2](https://github.com/szaghi/PENF/tree/v1.1.2) (2017-06-17)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.1.1...v1.1.2)
### Miscellaneous
- Merge tag 'v1.1.1' into develop

Stable release, not backward compatible due to module name changing. ([`4b33d49`](https://github.com/szaghi/PENF/commit/4b33d49c210604ae8eea8f9e88d9187f77443278))
- Update to last PENF ([`e848472`](https://github.com/szaghi/PENF/commit/e848472f589b29fb7336bed750d80e09037d35c1))
- Merge branch 'master' into develop ([`880b9bf`](https://github.com/szaghi/PENF/commit/880b9bf43ee83b5c3880af30568833d507e422f1))
- Updated PENF ([`46cc62d`](https://github.com/szaghi/PENF/commit/46cc62d4e93c4d384f087a279b6e808e43359cb9))
- Merge branch 'master' into develop ([`2542478`](https://github.com/szaghi/PENF/commit/2542478cf800f1b9c22a0a4ba413d9010e575c91))
- Update to new PENF and try GNU 6.x

Update to new PENF and try GNU 6.x on Travis CI ([`6df5b1d`](https://github.com/szaghi/PENF/commit/6df5b1de5d0aa4d531ddf21b97b41008d14fc3ae))
- Try to fix pip issue on Travis ([`a6c66ff`](https://github.com/szaghi/PENF/commit/a6c66ff53d8d0272c5a1d9d4bab6d22254af6158))
- Try to fix pip issue on Travis ([`4dbe8be`](https://github.com/szaghi/PENF/commit/4dbe8be62e8a0ee3c95d05b5366d04fb08d14280))
- Try to fix codecov issue on Travis ([`af7b25f`](https://github.com/szaghi/PENF/commit/af7b25f03b32013ec121ef9a73472342858b0028))
- Config codecov ([`5652efe`](https://github.com/szaghi/PENF/commit/5652efe6c5d7221d6416278b8330ebdf59df2725))
- Select master branch for submodules ([`e0f0043`](https://github.com/szaghi/PENF/commit/e0f0043bfde2646bed5d37258333b7b37f59f63e))
- Update submodules ([`758c54e`](https://github.com/szaghi/PENF/commit/758c54e2be3168f5d7fd4d249c8d323f15b6b169))
- Update readme ([`b98d629`](https://github.com/szaghi/PENF/commit/b98d629718265e762c7cc08d7f7656161dce9b9d))
- Update submodule ([`a55a769`](https://github.com/szaghi/PENF/commit/a55a769d26ac9ca995535c581bcc8ca39da26423))
- Merge branch 'master' into develop ([`7f1a760`](https://github.com/szaghi/PENF/commit/7f1a760a53fccd8e8778fbdaee5e32022f57bb29))
- Trim out dangerous recursive git clone/update ([`8f1c15d`](https://github.com/szaghi/PENF/commit/8f1c15d6754ce3361133b8f3e576b7fb63d46acc))
- Merge branch 'master' into develop ([`8e5c3d4`](https://github.com/szaghi/PENF/commit/8e5c3d445afff1933304c8ecca1a2bb13a7e7b00))
- Update submodule ([`6fbaf3e`](https://github.com/szaghi/PENF/commit/6fbaf3e841646fbb3f24e69eb8ddd9e16ec9aad8))
- Merge branch 'master' into develop ([`f1a9e38`](https://github.com/szaghi/PENF/commit/f1a9e3858d9b9983ba60ec60d6dcae36994e6602))
- Update submodules ([`d58a4f9`](https://github.com/szaghi/PENF/commit/d58a4f932008ab74f00f1d33d58e540270d5ddbe))
- Merge branch 'master' into develop ([`46141b5`](https://github.com/szaghi/PENF/commit/46141b5fe1b618a74c7c10a40ae6459cc491d669))
- Update submodules ([`1cc9115`](https://github.com/szaghi/PENF/commit/1cc91153308557cda097637efc46d69c2942965e))
- Improve coverage by means of doctests

Improve coverage by means of doctests

Sanitize pack data procedures.

Add install script and travis deployment. ([`c7fffd6`](https://github.com/szaghi/PENF/commit/c7fffd6fb17d0f7c02b1ee99bc68b5cb2f3a100c))
- Merge branch 'feature/improve-coverage' into develop ([`a27e0d5`](https://github.com/szaghi/PENF/commit/a27e0d58f60b296af91fe1398af604c3ea998a08))
- Merge branch 'release/1.1.2' ([`29a0356`](https://github.com/szaghi/PENF/commit/29a0356fca36102898ae4c4359d966549a3492f1))
- Merge tag 'v1.1.2' into develop

Adopt doctests introspective TDD

Stable release, fully backward compatible. ([`fc713cb`](https://github.com/szaghi/PENF/commit/fc713cb84560079c0b743efe94090dc1a41a9155))
- Update submodules ([`209a3a0`](https://github.com/szaghi/PENF/commit/209a3a0ea50261a5a07d64ebbf06b538e0c0a08f))
- General sanitize ([`fcf2597`](https://github.com/szaghi/PENF/commit/fcf2597b92a481163ba1d82595c6f6df40c57b54))
- Merge branch 'master' into develop ([`1eda60e`](https://github.com/szaghi/PENF/commit/1eda60e28de306ad2a61fef1e275f751d81e07ff))
- Merge branch 'release/1.1.2' ([`ccfe448`](https://github.com/szaghi/PENF/commit/ccfe4487f2d835a8bada7d4c3f98b0b9065bef9d))

## [v1.1.1](https://github.com/szaghi/PENF/tree/v1.1.1) (2016-05-24)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.1.0...v1.1.1)
### Miscellaneous
- Merge tag 'v1.1.0' into develop

Stable release, not fully backward compatible.

Update to the most recent PENF library.

Not backward compatible:

+ main module name changed into `befor64`;
+ main module file name changed into `befor64.F90`;
+ `b64_initialized` global variable changed to `is_b64_initialized`; ([`780cb5b`](https://github.com/szaghi/PENF/commit/780cb5bb64aeae65cccdc8a1db9f09ad4571d3b2))
- Update licenses date ([`e279b15`](https://github.com/szaghi/PENF/commit/e279b1556ce91c30cf74c84fccf0c5c1953d0e60))
- Merge branch 'master' into develop ([`c485ccb`](https://github.com/szaghi/PENF/commit/c485ccbb42205ab5e953937b0c0f97db1144a4e0))
- Update to last PENF version ([`fce1dbe`](https://github.com/szaghi/PENF/commit/fce1dbee6ef7de9bf4485e8c723b3f388da96e77))
- Sanitize file names

Sanitize file names prefixing befor64 to all sources ([`004d3c7`](https://github.com/szaghi/PENF/commit/004d3c71d262240f621719142c368dcab37a3034))
- Merge branch 'release/1.1.1' ([`efe505d`](https://github.com/szaghi/PENF/commit/efe505da0deda7fcef50902294f0e07b05dee211))

## [v1.1.0](https://github.com/szaghi/PENF/tree/v1.1.0) (2016-03-30)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.0.6...v1.1.0)
### Miscellaneous
- Merge branch 'release/1.0.6' ([`c671488`](https://github.com/szaghi/PENF/commit/c6714888f294cca9d4d339fac3ae8e273ac9fc71))
- Update makedoc.sh ([`47e6b49`](https://github.com/szaghi/PENF/commit/47e6b496ac4133cd73412a10a8976c94004a3fe5))
- Update makedoc.sh script ([`6e566a1`](https://github.com/szaghi/PENF/commit/6e566a1ffd6a7683a270cba08195fd7517a38d09))
- Update to the most recent PENF library

Update to the most recent PENF library.

Not backward compatible:

+ main module name changed into `befor64`;
+ main module file name changed into `befor64.F90`;
+ `b64_initialized` global variable changed to `is_b64_initialized`; ([`c4e4046`](https://github.com/szaghi/PENF/commit/c4e4046d79a2c904dc04101f8c2541dec531702c))
- Merge branch 'release/1.1.0' ([`b7cd885`](https://github.com/szaghi/PENF/commit/b7cd8859a96514c8c9394e984fbf6953531cbbf5))

## [v1.0.6](https://github.com/szaghi/PENF/tree/v1.0.6) (2015-07-28)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.0.5...v1.0.6)
### Miscellaneous
- Merge tag 'v1.0.5' into develop

Submodularize IR_Precision, stable release, fully backward compatible. ([`6735e92`](https://github.com/szaghi/PENF/commit/6735e9252b0af45873be0af51d0349484f795432))
- Sanitize submodules

Place git submodules into external dir outside src and symlink
the necessary sources. ([`8d43fef`](https://github.com/szaghi/PENF/commit/8d43fef9f0e0c9f1f310eb0508399cb4e307c961))

## [v1.0.5](https://github.com/szaghi/PENF/tree/v1.0.5) (2015-07-27)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.0.4...v1.0.5)
### Bug fixes
- Fixing travis issue ([`4e7919d`](https://github.com/szaghi/PENF/commit/4e7919dce5b4d105864233542ef2cdaef97c3545))
- Fixing travis issue ([`6c293f8`](https://github.com/szaghi/PENF/commit/6c293f89550fdc37c4d189c66b4232dfb6adeed6))
- Fixing travis issue ([`6ab6b6d`](https://github.com/szaghi/PENF/commit/6ab6b6d1fcf1047287e5098077b711d792f40629))

### Miscellaneous
- Correct travis ci makedoc script ([`981bd36`](https://github.com/szaghi/PENF/commit/981bd366ac89dcb1d7235306cf6c8c69aa8c5d35))
- Try fix travis push issue ([`44c1ad2`](https://github.com/szaghi/PENF/commit/44c1ad2511770022afae5e86c25e4bf4874ac586))
- Generalize makedoc.sh ([`c33cd37`](https://github.com/szaghi/PENF/commit/c33cd37551e70782559202010496081d99bf335f))
- Modify BSD3 license ([`d78e5c9`](https://github.com/szaghi/PENF/commit/d78e5c920ff3f4795e7576dabd9f49585f6b3fbe))
- Commit before submodularize IR_Precision ([`2360f78`](https://github.com/szaghi/PENF/commit/2360f78fba13f3fea9a0106df8457ef5f0dfa639))
- Submodularize IR_Precision ([`58952c2`](https://github.com/szaghi/PENF/commit/58952c29c6c4b87405dfee8050f72f9e12bdcb92))
- Submodularize IR_Precision ([`b724187`](https://github.com/szaghi/PENF/commit/b724187f85c213933245c5f6e85eceb3abb1950d))
- Merge branch 'feature/submodularize-ir_precision' into develop ([`a96c4b9`](https://github.com/szaghi/PENF/commit/a96c4b94634b0c8672f4398e24d93de76cb7cfe1))
- Purge out ChangeLog.md references ([`a88fd80`](https://github.com/szaghi/PENF/commit/a88fd8061ce295fdc8ff1f32e820f415cfda258b))
- Merge branch 'release/1.0.5' ([`51aa1e0`](https://github.com/szaghi/PENF/commit/51aa1e0cfc8d0cda3a0629e44d741f84759a9b5f))

## [v1.0.4](https://github.com/szaghi/PENF/tree/v1.0.4) (2015-06-09)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.0.3...v1.0.4)
### Bug fixes
- Fix big problem with security... ([`7539198`](https://github.com/szaghi/PENF/commit/7539198cd015b510945e9d16e6c99e22e378fa55))

### Miscellaneous
- Add automatic changelog generator ([`0c3fb67`](https://github.com/szaghi/PENF/commit/0c3fb6777f944f5489705ea79bee59072bd5df1b))
- Switch off coveralls for codecov.io ([`2ad8c86`](https://github.com/szaghi/PENF/commit/2ad8c86ea5049fb9cd8a431986bba141d0ff8217))
- Update README ([`b43e3e9`](https://github.com/szaghi/PENF/commit/b43e3e9c4bca55a0f6745474424619b2693385c9))

## [v1.0.3](https://github.com/szaghi/PENF/tree/v1.0.3) (2015-06-08)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.0.2...v1.0.3)
### Miscellaneous
- Correct travis ([`cf3978d`](https://github.com/szaghi/PENF/commit/cf3978d5ea1f7ba4568ba12d2d70c738df462dfa))
- Correct travis 2 ([`c28f917`](https://github.com/szaghi/PENF/commit/c28f9179d135b8f1de8f16763f70759ff6a84a52))
- Correct travis 3 ([`811f114`](https://github.com/szaghi/PENF/commit/811f114150416b7aaab1f294ee65ed0ebeebb74c))
- Correct travis 4 ([`87073b6`](https://github.com/szaghi/PENF/commit/87073b66931500ed34acb972d6dd370f6a677472))
- Add buil status badge ([`f38be07`](https://github.com/szaghi/PENF/commit/f38be07fbf339b1e6481cf3eb1ef639d03e62be7))
- Add badges ([`5d68cf6`](https://github.com/szaghi/PENF/commit/5d68cf606be3bcba18b20e1af1b013ad73d772d3))
- Change README format ([`31a1ce2`](https://github.com/szaghi/PENF/commit/31a1ce26dcd016db6a87c8c676d54461d66b4d49))
- Add new IR_Precision readme ([`98e6767`](https://github.com/szaghi/PENF/commit/98e676760992631278b7e95d0b763e6548055527))
- Add API doc autodeployment by travis ([`6308afd`](https://github.com/szaghi/PENF/commit/6308afd6a413df4dab2af01e07f10486ecaa6712))
- Trying coverage with gfortran ([`e1a7709`](https://github.com/szaghi/PENF/commit/e1a770975019105a311c5051e31e7edd8ed50749))
- Testing coverage ([`7f52f68`](https://github.com/szaghi/PENF/commit/7f52f68ffb15d3897c10c6b11b7ec47c57939ff1))
- Testing coverage again ([`14debfa`](https://github.com/szaghi/PENF/commit/14debfa9254477555ee022355b021c23957f87c8))
- Testing coverage over and over ([`6bae9a7`](https://github.com/szaghi/PENF/commit/6bae9a73d23cc2e2de0d9111387ea0d2ee045771))
- Testing coverage AGAIN? ([`e7ce23a`](https://github.com/szaghi/PENF/commit/e7ce23a3e2d78e0af9357a940404a029a3cbe744))
- Ufffff testing coverage ([`0103236`](https://github.com/szaghi/PENF/commit/0103236397dde54ccfe039ede862e87d28e6e10e))
- Add coverage badge ([`9d761b6`](https://github.com/szaghi/PENF/commit/9d761b6740bea88bc3bb7a34c7ce49de0d7d3a78))
- Testing travis cache ([`a6f7c04`](https://github.com/szaghi/PENF/commit/a6f7c04969a81e05d4c6ed651f9d6f5d0bbc1848))
- Continue testing travis cache ([`b8c9381`](https://github.com/szaghi/PENF/commit/b8c9381ba33a861f3ae77f6aa762aa8ad576102f))
- Continue testing cache travis ([`4f083ee`](https://github.com/szaghi/PENF/commit/4f083eec9ee37f54ea6fcdfeff306c612bf381cb))
- Disable travis cache ([`68628b3`](https://github.com/szaghi/PENF/commit/68628b3661cb0e10a1fdb3765d3959ffbf1f154a))
- Refactor autotests ([`20e9f6c`](https://github.com/szaghi/PENF/commit/20e9f6cb60f66df3f3349dd5f7d8046ccdff07d9))
- Add rule for coverage analysis reports making ([`a83b160`](https://github.com/szaghi/PENF/commit/a83b160838c5d293978498cf6eb0470657b1e002))
- Fix pure/elemental bug for new F08 standard, issue[#8](https://github.com/szaghi/PENF/issues/8) ([`aac1e54`](https://github.com/szaghi/PENF/commit/aac1e54a3ea27dff0248d7a7b347013d94f9ed81))

## [v1.0.2](https://github.com/szaghi/PENF/tree/v1.0.2) (2015-02-12)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.0.1...v1.0.2)
### Miscellaneous
- Minor bugs fix and travis support ([`c2d939d`](https://github.com/szaghi/PENF/commit/c2d939d4738e40dbb5d9841d587ee1ae6458146f))

## [v1.0.1](https://github.com/szaghi/PENF/tree/v1.0.1) (2015-02-03)
[Full Changelog](https://github.com/szaghi/PENF/compare/v1.0.0...v1.0.1)
### Miscellaneous
- Correct changelog ([`55ceed6`](https://github.com/szaghi/PENF/commit/55ceed6d38842a9edce77f72f342e238f49792e0))
- Improve doc ([`4ca96c7`](https://github.com/szaghi/PENF/commit/4ca96c70c2b9d7272a90970c97ed45455d3bc002))
- Add initialized flag ([`c17259e`](https://github.com/szaghi/PENF/commit/c17259e306adccaa23ee18a88069a82ab0f35c67))
- Add character and unlimited polymorphic support

Stable release. Fully backward compatible:

+ add scalar/array encoders for character;
+ add scalar/array decoders for character;
+ add scalar/array encoders for unlimited polymorphic;
+ add scalar/array decoders for unlimited polymorphic. ([`d674e4e`](https://github.com/szaghi/PENF/commit/d674e4e3341c0b5fad8e133d2f25109ded360d76))

## [v1.0.0](https://github.com/szaghi/PENF/tree/v1.0.0) (2015-02-02)
[Full Changelog](https://github.com/szaghi/PENF/compare/v0.1.1...v1.0.0)
### Miscellaneous
- Add decoders

Add scalar encorders for numbers. Add scalar/array decoders for
numbers. ([`4428773`](https://github.com/szaghi/PENF/commit/44287737d681b9d011fa3b1e33479efce62a588e))

## [v0.1.1](https://github.com/szaghi/PENF/tree/v0.1.1) (2015-01-29)
[Full Changelog](https://github.com/szaghi/PENF/compare/v0.0.1...v0.1.1)
### Miscellaneous
- Commit for FRA ([`c5fc651`](https://github.com/szaghi/PENF/commit/c5fc6515f48ca805ef5844180a07db6ea4ac8901))
- Refactor encoding procedures

Refactor encoding procedures: the nB parameter is no longer required.
However, the library must be properly initialized (by means of the
new b64_init procedure). ([`9389c64`](https://github.com/szaghi/PENF/commit/9389c64b8b790d0faff4be7f6253bf803a07ad0e))
- Implement scalars encoding and API change

The encoders of scalars number have been implemented. The API is
changed: the returned encoded string is a `character(len=:), allocatable`
variable! The backward compatibility is broken. ([`55c930a`](https://github.com/szaghi/PENF/commit/55c930ad861df976772b114c22fe6b3a68280ab2))

## [v0.0.1](https://github.com/szaghi/PENF/tree/v0.0.1) (2015-01-27)
### Miscellaneous
- Init versioning ([`ec7bdef`](https://github.com/szaghi/PENF/commit/ec7bdef6521e9a53fb5a607d2dc90954528fd7c6))


