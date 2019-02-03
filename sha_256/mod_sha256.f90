module mod_sha256

  use, intrinsic :: iso_fortran_env, only : int8, int32, int64

  implicit none

  integer(kind=int32), parameter :: kk(64) = (/ &
    transfer(z'428a2f98',int32),transfer(z'71374491',int32),transfer(z'b5c0fbcf',int32),transfer(z'e9b5dba5',int32), &
    transfer(z'3956c25b',int32),transfer(z'59f111f1',int32),transfer(z'923f82a4',int32),transfer(z'ab1c5ed5',int32), &
    transfer(z'd807aa98',int32),transfer(z'12835b01',int32),transfer(z'243185be',int32),transfer(z'550c7dc3',int32), &
    transfer(z'72be5d74',int32),transfer(z'80deb1fe',int32),transfer(z'9bdc06a7',int32),transfer(z'c19bf174',int32), &
    transfer(z'e49b69c1',int32),transfer(z'efbe4786',int32),transfer(z'0fc19dc6',int32),transfer(z'240ca1cc',int32), &
    transfer(z'2de92c6f',int32),transfer(z'4a7484aa',int32),transfer(z'5cb0a9dc',int32),transfer(z'76f988da',int32), &
    transfer(z'983e5152',int32),transfer(z'a831c66d',int32),transfer(z'b00327c8',int32),transfer(z'bf597fc7',int32), &
    transfer(z'c6e00bf3',int32),transfer(z'd5a79147',int32),transfer(z'06ca6351',int32),transfer(z'14292967',int32), &
    transfer(z'27b70a85',int32),transfer(z'2e1b2138',int32),transfer(z'4d2c6dfc',int32),transfer(z'53380d13',int32), &
    transfer(z'650a7354',int32),transfer(z'766a0abb',int32),transfer(z'81c2c92e',int32),transfer(z'92722c85',int32), &
    transfer(z'a2bfe8a1',int32),transfer(z'a81a664b',int32),transfer(z'c24b8b70',int32),transfer(z'c76c51a3',int32), &
    transfer(z'd192e819',int32),transfer(z'd6990624',int32),transfer(z'f40e3585',int32),transfer(z'106aa070',int32), &
    transfer(z'19a4c116',int32),transfer(z'1e376c08',int32),transfer(z'2748774c',int32),transfer(z'34b0bcb5',int32), &
    transfer(z'391c0cb3',int32),transfer(z'4ed8aa4a',int32),transfer(z'5b9cca4f',int32),transfer(z'682e6ff3',int32), &
    transfer(z'748f82ee',int32),transfer(z'78a5636f',int32),transfer(z'84c87814',int32),transfer(z'8cc70208',int32), &
    transfer(z'90befffa',int32),transfer(z'a4506ceb',int32),transfer(z'bef9a3f7',int32),transfer(z'c67178f2',int32) /)

  integer(kind=int32),              public,  save :: hh(8)
  integer(kind=int32), allocatable, private, save :: wBuf(:)
  integer(kind=int64),              private, save :: nBlock, nBuf

contains

subroutine sha256_init(inWord)

  character(len=*), intent(in) :: inWord

  character(len=:), allocatable :: inWordP
  integer(kind=int64) :: wLen, wMod, wPad, i, c
  integer(kind=int8)  :: fLen(8), fWord(4)

  if(allocated(wBuf)) deallocate(wBuf)

  ! Initialise hh() with the fractional part
  !  of the square root of the first 8 prime numbers
  hh(1) = transfer(z'6a09e667',int32)
  hh(2) = transfer(z'bb67ae85',int32)
  hh(3) = transfer(z'3c6ef372',int32)
  hh(4) = transfer(z'a54ff53a',int32)
  hh(5) = transfer(z'510e527f',int32)
  hh(6) = transfer(z'9b05688c',int32)
  hh(7) = transfer(z'1f83d9ab',int32)
  hh(8) = transfer(z'5be0cd19',int32)

  ! Calculate padding needed until the message is
  !  (length+1)%512 = 448 bit
  wLen = len(inWord)
  wMod = mod(wLen+1,64)
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
    fWord(1) = transfer(inWordP(c+4:c+4),int8)
    fWord(2) = transfer(inWordP(c+3:c+3),int8)
    fWord(3) = transfer(inWordP(c+2:c+2),int8)
    fWord(4) = transfer(inWordP(c+1:c+1),int8)
    wBuf(i)  = transfer(fWord(1:4),int8)
  end do
  ! Append the length as an int64 spanning 2 words at the end
  wBuf(nBuf:nBuf-1:-1) = transfer(wLen*8,int32,2)

end subroutine sha256_init

subroutine sha256_hash

  integer             :: i,j
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
