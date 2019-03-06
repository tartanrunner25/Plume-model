!-------------------------------------------------------------------------------------------!
!   Simple subroutine reads in text file input as specified by the Freitas model namelist   !
!   file. This script reads in the number of lines, which is used to allocate our fire      !
!   input sizes later on in our main program.                                               !
!   Written by DVM on 1/4/2019                                                              !
!-------------------------------------------------------------------------------------------!

subroutine read_fsize(input_file,nlines)

  character (len=80) :: input_file
  integer            :: nlines

  nlines = 0

  OPEN (1, file = input_file,status='OLD')

  DO
      READ (1,*, END=1)
      nlines = nlines + 1
  END DO

  1 CLOSE (1)

end subroutine read_fsize
