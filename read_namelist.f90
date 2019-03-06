!-----------------------------------------------------------------------------------!
!   This subroutine opens the 'plume_namelist' file, which is used to set runtime   !
!   options for our Freitas model plume rise simulations                            !
!                                                     Written by DVM 1/15/2019      !
!-----------------------------------------------------------------------------------!

subroutine read_namelist(input_file,met_type,met_prefix,overwriteTF)

  character(len=80), dimension(4) :: namelist_opt
  character(len=80)               :: input_file, met_type, met_prefix,overwriteTF



  open(unit = 5, file = 'plume_namelist', status = 'old', action = 'read')
  read(5,*) namelist_opt

  input_file = namelist_opt(1)
  met_type = namelist_opt(2)
  met_prefix = namelist_opt(3)
  overwriteTF = namelist_opt(4)




end subroutine read_namelist
