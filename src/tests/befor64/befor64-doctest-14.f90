program volatile_doctest
use befor64
 use befor64
 use penf
 character(len=:), allocatable :: code64
 call b64_encode(n=[121._R16P,2.32_R16P], code=code64)
 if (R16P==R8P) then
 print "(L1)", code64=='AAAAAABAXkCPwvUoXI8CQA=='
 else
 print "(L1)", code64=='AAAAAAAAAAAAAAAAAOQFQI/C9Shcj8L1KFyPwvUoAEA='
 endif
endprogram volatile_doctest