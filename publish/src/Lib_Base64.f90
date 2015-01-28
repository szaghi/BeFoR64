!< BeFoR64, Base64 encoding/decoding library for FoRtran poor men
module Lib_Base64
!-----------------------------------------------------------------------------------------------------------------------------------
!< BeFoR64, Base64 encoding/decoding library for FoRtran poor men
!<{!README-BeFoR64.md!}
!<
!<### ChangeLog
!<
!<{!ChangeLog-BeFoR64.md!}
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision  ! Integers and reals precision definition.
USE Lib_Pack_Data ! Library for packing heterogeneous data into single (homogeneous) packed one.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public:: b64_encode
public:: pack_data
public:: autotest
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
character(64):: base64="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" !< Base64 alphabet.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
interface b64_encode
!< Procedure for encoding numbers (integer and real) to base64.
  module procedure b64_encode_R8_a, &
                   b64_encode_R4_a, &
                   b64_encode_I8_a, &
                   b64_encode_I4_a, &
                   b64_encode_I2_a, &
                   b64_encode_I1_a
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  pure subroutine encode_bits(bits,padd,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Subroutine for encoding bits (must be multiple of 24 bits) into base64 charcaters code (of length multiple of 4).
  !<
  !< @note The bits stream are encoded in chunks of 24 bits as the following example (in little endian order):
  !<```
  !< +--first octet--+-second octet--+--third octet--+
  !< |7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|
  !< +-----------+---+-------+-------+---+-----------+
  !< |5 4 3 2 1 0|5 4 3 2 1 0|5 4 3 2 1 0|5 4 3 2 1 0|
  !< +--1.index--+--2.index--+--3.index--+--4.index--+
  !<```
  !< The 4 indexes are stored into 4 elements 8 bits array, thus 2 bits of each array element are not used.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I1P), intent(IN)::  bits(1:)  !< Bits to be encoded.
  integer(I4P), intent(IN)::  padd      !< Number of padding characters ('=').
  character(1), intent(OUT):: code(1:)  !< Characters code.
  integer(I1P)::              sixb(1:4) !< 6 bits slices (stored into 8 bits integer) of 24 bits input.
  integer(I8P)::              c         !< Counter.
  integer(I8P)::              e         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  c = 1_I8P
  do e=1_I8P,size(bits,dim=1,kind=I8P),3_I8P ! loop over array elements: 3 bytes (24 bits) scanning
    sixb = 0_I1P
    call mvbits(bits(e  ),2,6,sixb(1),0)
    call mvbits(bits(e  ),0,2,sixb(2),4) ; call mvbits(bits(e+1),4,4,sixb(2),0)
    call mvbits(bits(e+1),0,4,sixb(3),2) ; call mvbits(bits(e+2),6,2,sixb(3),0)
    call mvbits(bits(e+2),0,6,sixb(4),0)
    sixb = sixb + 1_I1P
    code(c  :c  )(1:1) = base64(sixb(1):sixb(1))
    code(c+1:c+1)(1:1) = base64(sixb(2):sixb(2))
    code(c+2:c+2)(1:1) = base64(sixb(3):sixb(3))
    code(c+3:c+3)(1:1) = base64(sixb(4):sixb(4))
    c = c + 4_I8P
  enddo
  if (padd>0) code(size(code,dim=1)-padd+1:)(1:1)='='
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine encode_bits

  pure subroutine b64_encode_R8_a(nB,n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding array numbers to base64 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P),              intent(IN)::  nB                                 !< Number of bytes of single element of n.
  real(R8P),                 intent(IN)::  n(1:)                              !< Array of numbers to be encoded.
  character(1), allocatable, intent(OUT):: code(:)                            !< Encoded array.
  integer(I1P)::                           nI1P(1:((size(n,dim=1)*nB+2)/3)*3) !< One byte integer array containing n.
  integer(I4P)::                           padd                               !< Number of padding characters ('=').
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(code)) deallocate(code) ; allocate(code(1:((size(n,dim=1)*nB+2)/3)*4)) ! allocating code chars
  nI1P = transfer(n,nI1P)                                                              ! casting n to integer array of 1 byte elem
  padd = mod((size(n,dim=1)*nB),3_I4P) ; if (padd>0_I4P) padd = 3_I4P - padd           ! computing the number of padding characters
  call encode_bits(bits=nI1P,padd=padd,code=code)                                      ! encoding bits
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_R8_a

  pure subroutine b64_encode_R4_a(nB,n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding array numbers to base64 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P),              intent(IN)::  nB                                 !< Number of bytes of single element of n.
  real(R4P),                 intent(IN)::  n(1:)                              !< Array of numbers to be encoded.
  character(1), allocatable, intent(OUT):: code(:)                            !< Encoded array.
  integer(I1P)::                           nI1P(1:((size(n,dim=1)*nB+2)/3)*3) !< One byte integer array containing n.
  integer(I4P)::                           padd                               !< Number of padding characters ('=').
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(code)) deallocate(code) ; allocate(code(1:((size(n,dim=1)*nB+2)/3)*4)) ! allocating code chars
  nI1P = transfer(n,nI1P)                                                              ! casting n to integer array of 1 byte elem
  padd = mod((size(n,dim=1)*nB),3_I4P) ; if (padd>0_I4P) padd = 3_I4P - padd           ! computing the number of padding characters
  call encode_bits(bits=nI1P,padd=padd,code=code)                                      ! encoding bits
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_R4_a

  pure subroutine b64_encode_I8_a(nB,n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding array numbers to base64 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P),              intent(IN)::  nB                                 !< Number of bytes of single element of n.
  integer(I8P),              intent(IN)::  n(1:)                              !< Array of numbers to be encoded.
  character(1), allocatable, intent(OUT):: code(:)                            !< Encoded array.
  integer(I1P)::                           nI1P(1:((size(n,dim=1)*nB+2)/3)*3) !< One byte integer array containing n.
  integer(I4P)::                           padd                               !< Number of padding characters ('=').
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(code)) deallocate(code) ; allocate(code(1:((size(n,dim=1)*nB+2)/3)*4)) ! allocating code chars
  nI1P = transfer(n,nI1P)                                                              ! casting n to integer array of 1 byte elem
  padd = mod((size(n,dim=1)*nB),3_I4P) ; if (padd>0_I4P) padd = 3_I4P - padd           ! computing the number of padding characters
  call encode_bits(bits=nI1P,padd=padd,code=code)                                      ! encoding bits
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_I8_a

  pure subroutine b64_encode_I4_a(nB,n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding array numbers to base64 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P),              intent(IN)::  nB                                 !< Number of bytes of single element of n.
  integer(I4P),              intent(IN)::  n(1:)                              !< Array of numbers to be encoded.
  character(1), allocatable, intent(OUT):: code(:)                            !< Encoded array.
  integer(I1P)::                           nI1P(1:((size(n,dim=1)*nB+2)/3)*3) !< One byte integer array containing n.
  integer(I4P)::                           padd                               !< Number of padding characters ('=').
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(code)) deallocate(code) ; allocate(code(1:((size(n,dim=1)*nB+2)/3)*4)) ! allocating code chars
  nI1P = transfer(n,nI1P)                                                              ! casting n to integer array of 1 byte elem
  padd = mod((size(n,dim=1)*nB),3_I4P) ; if (padd>0_I4P) padd = 3_I4P - padd           ! computing the number of padding characters
  call encode_bits(bits=nI1P,padd=padd,code=code)                                      ! encoding bits
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_I4_a

  pure subroutine b64_encode_I2_a(nB,n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding array numbers to base64 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P),              intent(IN)::  nB                                 !< Number of bytes of single element of n.
  integer(I2P),              intent(IN)::  n(1:)                              !< Array of numbers to be encoded.
  character(1), allocatable, intent(OUT):: code(:)                            !< Encoded array.
  integer(I1P)::                           nI1P(1:((size(n,dim=1)*nB+2)/3)*3) !< One byte integer array containing n.
  integer(I4P)::                           padd                               !< Number of padding characters ('=').
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(code)) deallocate(code) ; allocate(code(1:((size(n,dim=1)*nB+2)/3)*4)) ! allocating code chars
  nI1P = transfer(n,nI1P)                                                              ! casting n to integer array of 1 byte elem
  padd = mod((size(n,dim=1)*nB),3_I4P) ; if (padd>0_I4P) padd = 3_I4P - padd           ! computing the number of padding characters
  call encode_bits(bits=nI1P,padd=padd,code=code)                                      ! encoding bits
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_I2_a

  pure subroutine b64_encode_I1_a(nB,n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding array numbers to base64 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P),              intent(IN)::  nB                                 !< Number of bytes of single element of n.
  integer(I1P),              intent(IN)::  n(1:)                              !< Array of numbers to be encoded.
  character(1), allocatable, intent(OUT):: code(:)                            !< Encoded array.
  integer(I1P)::                           nI1P(1:((size(n,dim=1)*nB+2)/3)*3) !< One byte integer array containing n.
  integer(I4P)::                           padd                               !< Number of padding characters ('=').
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(code)) deallocate(code) ; allocate(code(1:((size(n,dim=1)*nB+2)/3)*4)) ! allocating code chars
  nI1P = transfer(n,nI1P)                                                              ! casting n to integer array of 1 byte elem
  padd = mod((size(n,dim=1)*nB),3_I4P) ; if (padd>0_I4P) padd = 3_I4P - padd           ! computing the number of padding characters
  call encode_bits(bits=nI1P,padd=padd,code=code)                                      ! encoding bits
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_I1_a

  subroutine autotest()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for autotesting the library functionalities.
  !<
  !< @warning The auto-testing capability fails if the -O2+ (Intel Fortran Compiler) optimizations are enabled. It seems due to the
  !< *inlining* of encoding/decoding procedures.
  !<
  !< @note Into the *src* directory there is a small python script (*validation.py) that can be used to validate the library
  !< correctness by a comparison with other widely used tools such as the python builtin module *struct*.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  real(R8P)::                 array_R8(1:1) = [1._R8P]   !< Real input to be encoded.
  real(R4P)::                 array_R4(1:1) = [0._R4P]   !< Real input to be encoded.
  integer(I8P)::              array_I8(1:1) = [23_I8P]   !< Integer input to be encoded.
  integer(I4P)::              array_I4(1:1) = [2023_I4P] !< Integer input to be encoded.
  integer(I2P)::              array_I2(1:1) = [-203_I2P] !< Integer input to be encoded.
  integer(I1P)::              array_I1(1:1) = [120_I1P]  !< Integer input to be encoded.
  character(1), allocatable:: code(:)                    !< Base64 encoded array.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call IR_Init

  call b64_encode(nB=int(BYR8P,I4P),n=array_R8,code=code)
  print "(A,1X,L1)", '+ Code of '//trim(str(n=array_R8(1)))//': "'//stringify(code)//&
    '", Is it correct?',trim(stringify(code))=='AAAAAAAA8D8='

  call b64_encode(nB=int(BYR4P,I4P),n=array_R4,code=code)
  print "(A,1X,L1)", '+ Code of '//trim(str(n=array_R4(1)))//': "'//stringify(code)//&
    '", Is it correct?',trim(stringify(code))=='AAAAAA=='

  call b64_encode(nB=int(BYI8P,I4P),n=array_I8,code=code)
  print "(A,1X,L1)", '+ Code of '//trim(str(n=array_I8(1)))//': "'//stringify(code)//&
    '", Is it correct?',trim(stringify(code))=='FwAAAAAAAAA='

  call b64_encode(nB=int(BYI4P,I4P),n=array_I4,code=code)
  print "(A,1X,L1)", '+ Code of '//trim(str(n=array_I4(1)))//': "'//stringify(code)//&
    '", Is it correct?',trim(stringify(code))=='5wcAAA=='

  call b64_encode(nB=int(BYI2P,I4P),n=array_I2,code=code)
  print "(A,1X,L1)", '+ Code of '//trim(str(n=array_I2(1)))//': "'//stringify(code)//&
    '", Is it correct?',trim(stringify(code))=='Nf8='

  call b64_encode(nB=int(BYI1P,I4P),n=array_I1,code=code)
  print "(A,1X,L1)", '+ Code of '//trim(str(n=array_I1(1)))//': "'//stringify(code)//&
    '", Is it correct?',trim(stringify(code))=='eA=='
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    pure function stringify(string) result (char_string)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Function for converting array of 1 character to a string of characters.
    !<
    !< It is used for writing the stream of base64 encoded data.
    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    character(1), intent(IN)::      string(1:)  !< Array of 1 character.
    character(size(string,dim=1)):: char_string !< String of characters.
    integer(I4P)::                  i           !< Counter.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    forall(i = 1:size(string,dim=1))
       char_string(i:i) = string(i)
    endforall
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endfunction stringify
  endsubroutine autotest

  !!> @brief Procedure for decoding array numbers from base64 (R8P).
  !pure subroutine b64_decode_R8_a(code,n)
  !!--------------------------------------------------------------------------------------------------------------------------------
  !implicit none
  !real(R8P),                      intent(OUT):: n(1:) !< Number to be decoded.
  !character(ncR8P*size(n,dim=1)), intent(IN)::  code  !< Encoded number.
  !integer(I4P)::                                c,d   !< Counters.
  !!--------------------------------------------------------------------------------------------------------------------------------

  !!--------------------------------------------------------------------------------------------------------------------------------
  !d = 1_I4P
  !do c=1,len(code),ncR8P
  !  call b64_decode_R8_s(code=code(c:c+ncR8P-1),n=n(d))
  !  d = d + 1_I4P
  !enddo
  !return
  !!--------------------------------------------------------------------------------------------------------------------------------
  !endsubroutine b64_decode_R8_a
endmodule Lib_Base64
