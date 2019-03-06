!-------------------------------------------------------------------------------------------!
!   This subroutine reads in text file input as specified by the Freitas model namelist     !
!   file. This text file should contain information such as the fire location on the WRF    !
!   i j grid, heat flux, and the fire area.                                                 !
!   Written by DVM on 10/4/2018                                                             !
!-------------------------------------------------------------------------------------------!


subroutine read_finput(input_file,nlines,dates,lats,lons,i_points,j_points,&
                       fire_fluxes,fire_areas,met_files,model_times)


  integer                              :: nlines, i
  character (len=80)                   :: input_file
  character(len=60), dimension(nlines) :: met_files
  integer, dimension(nlines)           :: dates, i_points, j_points, model_times
  real, dimension(nlines)              :: fire_fluxes, fire_areas, lats, lons

  type :: fire_record
      integer :: date
      real :: lat
      real :: lon
      real :: fire_flux
      real :: fire_area
      integer :: i_point
      integer :: j_point
      integer :: model_time
      character(len=32) :: met_file
  end type fire_record
  type(fire_record), dimension(nlines) :: fire_reports


! Open up our 'text' input file and read the file line by line. For each line, assign
! the line values to our type and its components (each variable in our line). Assign
! each component per iteration to our output variables that will feed back to the
! run_plume_model.f90 code.

  OPEN (2, file = input_file,status='OLD',access ='sequential',form='formatted')
  do i = 1, nlines
      read(2,*) fire_reports(i)
      dates(i) = fire_reports(i) % date
      lats(i) = fire_reports(i) % lat
      lons(i) = fire_reports(i) % lon
      i_points(i) = fire_reports(i) % i_point
      j_points(i) = fire_reports(i) % j_point
      model_times(i) = fire_reports(i) % model_time
      fire_fluxes(i) = fire_reports(i) % fire_flux
      fire_areas(i) = fire_reports(i) % fire_area
      met_files(i) = fire_reports(i) % met_file
  end do


end subroutine read_finput
