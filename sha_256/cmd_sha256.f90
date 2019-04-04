! Extracted from module string_tools in SixTrack also written by V.K. Berglyd Olsen
! https://github.com/SixTrack/SixTrack
module string_tools
contains
function chr_toLower(theString) result(retString)
  character(len=*), intent(in)  :: theString
  character(len=:), allocatable :: retString
  character          :: ch
  integer, parameter :: ulOffset = ichar("A") - ichar("a")
  integer            :: i
  allocate(character(len(theString)) :: retString)
  do i = 1,len(theString)
    ch = theString(i:i)
    if(ch >= "A" .and. ch <= "Z") ch = char(ichar(ch)-ulOffset)
    retString(i:i) = ch
  end do
end function chr_toLower
end module string_tools

program main

  use mod_sha256
  use string_tools

  implicit none

  integer            :: nCmd
  character(len=4096):: inArg
  character(len=64)  :: hashDigest

  inArg  = " "
  nCmd   = command_argument_count()
  if(nCmd == 1) then
    if(nCmd > 1) then
      write(*,"(a)") "Warning: Received more than one input argument. Ignoring the rest."
    end if
    call get_command_argument(1, inArg)
    call sha256_init(trim(inArg))
    call sha256_hash
    call sha256_digest(hashDigest)
    write(*,"(a)") chr_toLower(hashDigest)
  else
    write(*,"(a)") "Error: Provide the string to be hashed as a single argument."
  end if

end program main
