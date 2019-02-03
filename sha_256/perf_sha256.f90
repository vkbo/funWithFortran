program main

  use mod_sha256

  implicit none

  integer, parameter  :: n = 1000000
  character(len=1024) :: theMessage
  character(len=64)   :: hashDigest, hashCorrect
  integer :: i
  real    :: tStart, tEnd

  open(11,file="lipsum.txt")
  read(11,"(a1024)") theMessage
  close(11)
  hashCorrect = "B6ACA4145A1DB0A151E1569F3B197E17ABD6329807BFFC531AB22053554066A7"

  call cpu_time(tStart)
  do i=1,n
    call sha256_init(trim(theMessage))
    call sha256_hash
    call sha256_digest(hashDigest)
  end do
  call cpu_time(tEnd)

  if(hashDigest == hashCorrect) then
    write(*,"(a)") "SHA-256 Match !"
  else
    write(*,"(a)") "SHA-256 Failed :("
  end if
  write(*,"(a,i0,a,i0,a,f10.6,a)") "Digested ",n," messages of ",len_trim(theMessage)," bytes in ",tEnd-tStart," seconds."

  theMessage  = "42"
  hashCorrect = "73475CB40A568E8DA8A045CED110137E159F890AC4DA883B6B17DC651B3A8049"

  call cpu_time(tStart)
  do i=1,n
    call sha256_init(trim(theMessage))
    call sha256_hash
    call sha256_digest(hashDigest)
  end do
  call cpu_time(tEnd)

  if(hashDigest == hashCorrect) then
    write(*,"(a)") "SHA-256 Match !"
  else
    write(*,"(a)") "SHA-256 Failed :("
  end if
  write(*,"(a,i0,a,i0,a,f10.6,a)") "Digested ",n," messages of ",len_trim(theMessage)," bytes in ",tEnd-tStart," seconds."

end program main
