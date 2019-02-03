module mod_sha256

  use, intrinsic :: iso_fortran_env, only : int32

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

  integer(kind=int32), private, save :: hh(8)

contains

subroutine sha256_init(inWord)

  character(len=*), intent(in) :: inWord

  write(*,"(a)") "SHA256> Init"

  write(*,"(a,z8.8)") "SHA256> ",kk(1)
  write(*,"(a,z8.8)") "SHA256> ",kk(2)
  write(*,"(a,z8.8)") "SHA256> ",kk(3)
  write(*,"(a,z8.8)") "SHA256> ",kk(4)

  open(unit=77,file="test.bin",access="stream",status="replace")
  write(77) kk(1:64)
  close(77)

end subroutine sha256_init

! (X AND Y) XOR ((NOT X) AND Z)
pure integer function ch(x,y,z)
  integer, intent(in) :: x,y,z
  ch = ieor(iand(x,y),iand(not(x),z))
end function ch

! (X AND Y) XOR (X AND Z) XOR (Y AND Z)
pure integer function maj(x,y,z)
  integer, intent(in) :: x,y,z
  maj = ieor(ieor(iand(x,y),iand(x,z)),iand(y,z))
end function maj

! RotR(X,2) XOR RotR(X,13) XOR RotR(X,22)
pure integer function sigmaRRR0(x)
  integer, intent(in) :: x
  sigmaRRR0 = ieor(ieor(ishftc(x,-2),ishftc(x,-13)),ishftc(x,-22))
end function sigmaRRR0

! RotR(X,6) XOR RotR(X,11) XOR RotR(X,25)
pure integer function sigmaRRR1(x)
  integer, intent(in) :: x
  sigmaRRR1 = ieor(ieor(ishftc(x,-6),ishftc(x,-11)),ishftc(x,-25))
end function sigmaRRR1

! RotR(X,7) XOR RotR(X,18) XOR ShR(X,3)
pure integer function sigmaRRS0(x)
  integer, intent(in) :: x
  sigmaRRS0 = ieor(ieor(ishftc(x,-7),ishftc(x,-18)),ishft(x,-3))
end function sigmaRRS0

! RotR(X,17) XOR RotR(X,19) XOR ShR(X,10)
pure integer function sigmaRRS1(x)
  integer, intent(in) :: x
  sigmaRRS1 = ieor(ieor(ishftc(x,-17),ishftc(x,-19)),ishft(x,-10))
end function sigmaRRS1

end module mod_sha256
