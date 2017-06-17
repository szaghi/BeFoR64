!< Testing program for BeFoR64, Base64 encoding/decoding library for FoRtran poor men
program befor64_test
!< Testing program for BeFoR64, Base64 encoding/decoding library for FoRtran poor men
!<
!<### Usage
!<```bash
!< ./Test_Driver
!<```
use befor64, only : autotest ! Autotesting procedure.

implicit none

call autotest
endprogram befor64_test
