!-------------------------------------------------------------------------------------------!
!   This subroutine opens the text file we generated using the linux ls system commands and !
!   takes the file names that were stored in this file and saves it to our allocated        !
!   unique_met file list. This is used to loop through each met input files                 !
!   files       Written by DVM 1/15/2019                                                    !
!-------------------------------------------------------------------------------------------!

subroutine list_metfile(file_num,unique_met)

  character(len=60), dimension(file_num) :: unique_met
  integer :: i,file_num

  open(unit = 4, file = 'my_ls.tmp', status = 'old', action = 'read')
  read(4,*) unique_met

  call system('rm my_ls.tmp')

end subroutine list_metfile
