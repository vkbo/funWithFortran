program main

  use mod_sha256

  implicit none

  integer, parameter  :: n = 3, num = 1000000
  character(len=1024) :: wBuffer, theMessage(n)
  character(len=64)   :: hashDigest, hashCorrect(n)
  integer :: i, j
  real    :: tStart, tEnd

  open(11,file="lipsum.txt")
  read(11,"(a1024)") wBuffer
  close(11)
  theMessage(1)  =  wBuffer
  hashCorrect(1) = "B6ACA4145A1DB0A151E1569F3B197E17ABD6329807BFFC531AB22053554066A7"

  theMessage(2)  = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
  hashCorrect(2) = "248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1"

  theMessage(3)  = "abc"
  hashCorrect(3) = "BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD"

  do i=1,n
    call cpu_time(tStart)
    do j=1,num
      call sha256_init(trim(theMessage(i)))
      call sha256_hash
    end do
    call cpu_time(tEnd)
    call sha256_digest(hashDigest)

    if(hashDigest /= hashCorrect(i)) then
      write(*,"(a)") "SHA-256 Failed :("
    end if
    write(*,"(a,i0,a,i3,a,f10.6,a)") "Digested ",num," messages of ",len_trim(theMessage(i))," bytes in ",tEnd-tStart," seconds."
  end do

end program main
