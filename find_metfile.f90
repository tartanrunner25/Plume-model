!-------------------------------------------------------------------------------------------!
!   This subroutine looks for files that match the met input prefix that we have specified. !
!   When the met input files are found, this is added to our integer which is needed to     !
!   determine how much space we need to allocate for our character array in the             !
!   list_metinput.f90 subroutine. If no met input file is found, or if there is an error    !
!   listing the files, the script will abort.                                               !
!   Written by DVM 1/14/2019                                                                !
!-------------------------------------------------------------------------------------------!


subroutine find_metfile(file_num,met_prefix)

  use iso_fortran_env
  implicit none
  character(len=*), parameter :: ls_file = 'my_ls.tmp'
  integer :: u, ios, x,  allocation_status, file_num
  character(len=60) :: filename, met_prefix, com1, com2, com
  character(len=32), dimension(:), allocatable   :: filenames

  ios=0
  x = 0


  !Find files that have our matching suffix using the system command. Then store
  !the results in a temporary text file, which will be read in by the next
  !subroutine to determine which met files we need to loop over. The "trim" command
  !concatenates the trailing blanks creates when allocating the character strings
  !above, else the command will be cropped.

  call system(trim('ls -1 '//met_prefix) // trim('* > '//ls_file))

  open(newunit=u, file=ls_file, iostat=ios, status="old", action="read")

  do
      read(u, *, iostat=ios) filename
      if (is_iostat_end(ios)) exit
      if (ios /= 0) STOP "Unexpected error while reading listing file"
      if (index(filename, "wrfout_d03") > 0) then
          !print*, filename
          x = x+1
      end if
  end do

  close(u)

  file_num = x

  if (file_num ==0) STOP "No met input files found... STOP!"

  !call system('rm '//ls_file)

end subroutine find_metfile
