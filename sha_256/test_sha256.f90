program main

  use mod_sha256

  implicit none

  integer, parameter  :: n = 4
  character(len=1024) :: wBuffer, theMessage(n)
  character(len=64)   :: hashDigest, hashCorrect(n)
  integer :: i

  theMessage(1)  = "abc"
  hashCorrect(1) = "BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD"

  theMessage(2)  = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
  hashCorrect(2) = "248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1"

  theMessage(3)  = "The quick brown fox jumps over the lazy dog"
  hashCorrect(3) = "D7A8FBB307D7809469CA9ABCB0082E4F8D5651E46D3CDB762D02D0BF37C9E592"

  open(11,file="lipsum.txt")
  read(11,"(a1024)") wBuffer
  close(11)
  theMessage(4)  =  wBuffer
  hashCorrect(4) = "B6ACA4145A1DB0A151E1569F3B197E17ABD6329807BFFC531AB22053554066A7"

  do i=1,n

    call sha256_init(trim(theMessage(i)))
    call sha256_hash
    call sha256_digest(hashDigest)

    write(*,"(a)") ""
    write(*,"(2(a,i0),a)") " Test ",i," of ",n,":"
    write(*,"(a)") repeat("=",80)
    write(*,"(a)") " Message: "//trim(theMessage(i))
    write(*,"(a)") " Digest:  "//hashDigest
    write(*,"(a)") " Correct: "//hashCorrect(i)
    if(hashDigest == hashCorrect(i)) then
      write(*,"(a)") " >> SHA-256 Match !"
    else
      write(*,"(a)") " >> SHA-256 Failed :("
    end if
  end do
  write(*,"(a)") ""

end program main
