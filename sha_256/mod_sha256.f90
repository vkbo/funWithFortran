module mod_sha256

  use, intrinsic :: iso_fortran_env, only : int8, int32, int64, real64

  implicit none

  integer(kind=int32), parameter :: kk(64) = (/ &
     1116352408,  1899447441, -1245643825,  -373957723,   961987163,  1508970993, -1841331548, -1424204075, &
     -670586216,   310598401,   607225278,  1426881987,  1925078388, -2132889090, -1680079193, -1046744716, &
     -459576895,  -272742522,   264347078,   604807628,   770255983,  1249150122,  1555081692,  1996064986, &
    -1740746414, -1473132947, -1341970488, -1084653625,  -958395405,  -710438585,   113926993,   338241895, &
      666307205,   773529912,  1294757372,  1396182291,  1695183700,  1986661051, -2117940946, -1838011259, &
    -1564481375, -1474664885, -1035236496,  -949202525,  -778901479,  -694614492,  -200395387,   275423344, &
      430227734,   506948616,   659060556,   883997877,   958139571,  1322822218,  1537002063,  1747873779, &
     1955562222,  2024104815, -2067236844, -1933114872, -1866530822, -1538233109, -1090935817,  -965641998 /)

  integer(kind=int32),              public,  save :: hh(8)
  integer(kind=int32), allocatable, private, save :: wBuf(:)
  integer(kind=int64),              private, save :: nBlock, nBuf

contains

subroutine sha256_init(inWord)

  character(len=*), intent(in) :: inWord

  character(len=:), allocatable :: inWordP
  integer(kind=int64) :: wLen, wMod, wPad, i, c
  integer(kind=int8)  :: fWord(4)

  if(allocated(wBuf)) deallocate(wBuf)

  ! Initialise hh() with the fractional part
  !  of the square root of the first 8 prime numbers
  hh = [ &
    1779033703,-1150833019,1013904242,-1521486534, &
    1359893119,-1694144372, 528734635, 1541459225  ]

  ! Which is equivalent to doing this:
  ! hh(1) = transfer(shiftr(transfer(sqrt( 2.0_real64),   1_int64),20), 1_int32)
  ! hh(2) = transfer(shiftr(transfer(sqrt( 3.0_real64),   1_int64),20), 1_int32)
  ! hh(3) = transfer(shiftr(transfer(sqrt( 5.0_real64)-1, 1_int64),20), 1_int32)
  ! hh(4) = transfer(shiftr(transfer(sqrt( 7.0_real64)-1, 1_int64),20), 1_int32)
  ! hh(5) = transfer(shiftr(transfer(sqrt(11.0_real64)-2, 1_int64),20), 1_int32)
  ! hh(6) = transfer(shiftr(transfer(sqrt(13.0_real64)-2, 1_int64),20), 1_int32)
  ! hh(7) = transfer(shiftr(transfer(sqrt(17.0_real64)-3, 1_int64),20), 1_int32)
  ! hh(8) = transfer(shiftr(transfer(sqrt(19.0_real64)-3, 1_int64),20), 1_int32)

  ! Calculate padding needed until the message is
  !  (length+1)%512 = 448 bit
  wLen = len(inWord)
  wMod = mod(wLen+1,64_int64)
  if(wMod > 56) then
    wPad = 120-wMod
  else
    wPad = 56-wMod
  end if

  ! Count the number of 512 bit blocks and
  !  allocate the buffer of 32 bit words
  nBlock = (wLen+wPad+9)/64
  nBuf   = nBlock * 16
  allocate(wBuf(nBuf))

  ! Add the padding of a single 1 and rest 0
  inWordP = inWord//char(128)//repeat(char(0),wPad)

  ! Loop over all characters in the string in order 4,3,2,1,8,7,6,5,...
  !  and store them as int32 in little endian order
  do i=1,nBuf-2
    c = 4*(i-1)
    fWord(1) = transfer(inWordP(c+4:c+4),1_int8)
    fWord(2) = transfer(inWordP(c+3:c+3),1_int8)
    fWord(3) = transfer(inWordP(c+2:c+2),1_int8)
    fWord(4) = transfer(inWordP(c+1:c+1),1_int8)
    wBuf(i)  = transfer(fWord(1:4),int8)
  end do
  ! Append the length as an int64 spanning 2 words at the end
  wBuf(nBuf:nBuf-1:-1) = transfer(wLen*8,int32,2)

end subroutine sha256_init

subroutine sha256_hash

  integer(kind=int64) :: i,j
  integer(kind=int32) :: h(8),t1,t2
  integer(kind=int32) :: w(64)

  do i=1,nBlock ! Loop over blocks

    ! Initialise the block digest variables
    h = hh

    do j=1,16 ! Read the 16 words in the current block
      w(j) = wBuf(16*(i - 1) + j)
    end do
    do j=17,64 ! Compute the remaining 48 words from the first 16
      w(j) = sigmaRRS1(w(j-2)) + w(j-7) + sigmaRRS0(w(j-15)) + w(j-16)
    end do

    do j=1,64 ! Digest the 64 words
      t1     = h(8) + sigmaRRR1(h(5)) + ch(h(5),h(6),h(7)) + kk(j) + w(j)
      t2     = sigmaRRR0(h(1)) + maj(h(1),h(2),h(3))
      h(2:8) = h(1:7)    ! Shift all the values
      h(5)   = h(5) + t1 ! Add the new word
      h(1)   = t1 + t2   ! Add the new word
    end do

    ! Add the results for this block to the message digest
    hh = h + hh

  end do

end subroutine sha256_hash

subroutine sha256_digest(outWord)
  character(len=64), intent(out) :: outWord
  write(outWord,"(8(z8.8))") hh
end subroutine sha256_digest

! (X AND Y) XOR ((NOT X) AND Z)
pure elemental integer(kind=int32) function ch(x,y,z)
  integer(kind=int32), intent(in) :: x,y,z
  ch = ieor(iand(x,y),iand(not(x),z))
end function ch

! (X AND Y) XOR (X AND Z) XOR (Y AND Z)
pure elemental integer(kind=int32) function maj(x,y,z)
  integer(kind=int32), intent(in) :: x,y,z
  maj = ieor(ieor(iand(x,y),iand(x,z)),iand(y,z))
end function maj

! RotR(X,2) XOR RotR(X,13) XOR RotR(X,22)
pure elemental integer(kind=int32) function sigmaRRR0(x)
  integer(kind=int32), intent(in) :: x
  sigmaRRR0 = ieor(ieor(dshiftr(x,x,2),dshiftr(x,x,13)),dshiftr(x,x,22))
end function sigmaRRR0

! RotR(X,6) XOR RotR(X,11) XOR RotR(X,25)
pure elemental integer(kind=int32) function sigmaRRR1(x)
  integer(kind=int32), intent(in) :: x
  sigmaRRR1 = ieor(ieor(dshiftr(x,x,6),dshiftr(x,x,11)),dshiftr(x,x,25))
end function sigmaRRR1

! RotR(X,7) XOR RotR(X,18) XOR ShR(X,3)
pure elemental integer(kind=int32) function sigmaRRS0(x)
  integer(kind=int32), intent(in) :: x
  sigmaRRS0 = ieor(ieor(dshiftr(x,x,7),dshiftr(x,x,18)),shiftr(x,3))
end function sigmaRRS0

! RotR(X,17) XOR RotR(X,19) XOR ShR(X,10)
pure elemental integer(kind=int32) function sigmaRRS1(x)
  integer(kind=int32), intent(in) :: x
  sigmaRRS1 = ieor(ieor(dshiftr(x,x,17),dshiftr(x,x,19)),shiftr(x,10))
end function sigmaRRS1

end module mod_sha256
