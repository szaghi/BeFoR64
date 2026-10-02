program volatile_doctest
use befor64
 use befor64
 use penf
 character(len=:), allocatable :: code64
 call b64_encode(n=134.231_R16P, code=code64)
 if (R16P==R8P) then
 print "(L1)", code64=='CKwcWmTHYEA='
 else
 print "(L1)", code64=='O99PjZduEoPAyqFFdgwGQA=='
 endif
endprogram volatile_doctest