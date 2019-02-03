program main

  use mod_sha256

  implicit none

  character(len=64) :: hashDigest

  call sha256_init("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
  call sha256_digest(hashDigest)

  write(*,"(a)") "Digest: "//hashDigest

end program main
