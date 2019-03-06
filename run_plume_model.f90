!--------------------------------------------------------------------------------------------!
!   This is the main program that runs all components needed to succesfully simulate fire    !
!   plume rises using the Freitas model. This script is responsible for:                     !
!              (1) Reading in namelist file to determine model settings                      !
!              (2) Reading in fire inputs                                                    !
!              (3) Determining unique met files to limit the number of times met input       !
!                  are opened and read in (which can be large). There are then used to       !
!                  to create a loop which will run through unique met files                  !
!              (4) Reads in environmental conditions from met inputs which are necessary     !
!                  for running the Freitas plume rise model                                  !
!              (5) Running the plume_alone_module.f90 module, which contains the plume rise  !
!                  model code and the necessary subroutines.                                 !
!                                                                                            !
!   Model code developed by S. Freitas, 2007, updated in 2009, and modified by by DVM on     !
!   1/7/2017 to include WRF model compatibility. Code restructured on 1/14/2019 to include   !
!   more runtime options and code efficiency. Alpha version completed on 1/27/2019 DVM       !
!   Freitas model v2.0 BETA                                                                  !
!--------------------------------------------------------------------------------------------!

program pr

use smk_plumerise, only : smk_pr_driver
use netcdf_func

implicit none
character (len=80)                           :: input_file, ncdf_file,met_type
character(len=80)                            :: fmt,overwriteTF
character (len=60)                           :: met_prefix
character(len=60), dimension(:), allocatable :: met_files, unique_met
character (len=5)                            :: x1
character sname*31,gname*31,uname*31,vname*31
integer, dimension(:), allocatable           :: dates, i_points, j_points, model_times
integer, dimension(:), allocatable           :: YYYYMMDDHH
real, dimension(:), allocatable              :: fire_fluxes,fire_areas,lats,lons
real, dimension(:), allocatable              :: flat,flon,heat_fluxW,burnt_area,ptops
integer                                      :: nlines,i,j,z,k,t,file_num,x,f,boolean
integer                                      :: sdims(4),udims(4),vdims(4),gdims(4),sd,ud,gd
integer                                      :: fire_num,ix
real, allocatable, dimension(:,:,:,:)        :: pot,qvapor,press,densi,geoph,uwind,vwind
real, allocatable, dimension(:)              :: col_theta,col_qvapor,col_densi,exner_func
real, allocatable, dimension(:)              :: mid_hgt,col_geoph,col_u,col_v
real                                         :: ptop

!Read in namelist file to specify our input text file name and met file type and
!name. Options selected here will be used to select options such as the name of the
!fire text input file, model output type (WRF or other data processed via met_em),
!and the prefix of the met input files.

call read_namelist(input_file,met_type,met_prefix,overwriteTF)


!Name of our input file? Eventually this will be specified in a text namelist
!file that will be read in.

!input_file = 'freitas_input_test.csv'
!met_type = 'WRF'

!Determine number of fires in our fire input file so we can properly allocate
!fire input arrays. Once our arrays have been properly allocated, we can read
!through the file again and save the fire information to our arrays, which
!we allocated for.

write(6,*) ' '
write(6,*) ' '
write(6,*) "Lets determine the size of our text file"


call read_fsize(input_file,nlines)


! Allocate our 1-D arrays based on the number of lines that were determined

allocate(dates(nlines))
allocate(lats(nlines))
allocate(lons(nlines))
allocate(i_points(nlines))
allocate(j_points(nlines))
allocate(model_times(nlines))
allocate(fire_fluxes(nlines))
allocate(fire_areas(nlines))
allocate(met_files(nlines))
allocate(YYYYMMDDHH(nlines))
allocate(flat(nlines))
allocate(flon(nlines))
allocate(heat_fluxW(nlines))
allocate(burnt_area(nlines))
allocate(ptops(nlines))


! Lets read in our fire properties and assign them to our array allocated
! above.

write(6,*) ' '
write(6,*) ' '
write(6,*) "Read in text file that contains fire data..."



call read_finput(input_file,nlines,dates,lats,lons,i_points,j_points,&
                 fire_fluxes,fire_areas,met_files,model_times)


! In an effort to limit the number of times we open our meteorological output files,
! lets determine what met files are in our met input directory, and then loop through
! these files later on. While loop through, we will open the file, and then determine
! which fires fall in the time that corresponds to the met file we've read in.

write(6,*) ' '
write(6,*) ' '
write(6,*) "Determine what met files we need to read in"

call find_metfile(file_num,met_prefix)

allocate(unique_met(file_num))

call list_metfile(file_num,unique_met)

write(6,*) ' '
write(6,*) ' '
write(6,*) '|||||||||||||||||||||||||||||||||||||||||||||'
write(6,*) '|||  Found the following met files....!   |||'
write(6,*) '|||||||||||||||||||||||||||||||||||||||||||||'
write(6,*) (unique_met(x), x=1,file_num)
write(6,*) ' '
write(6,*) ' '



!Loop through our met input files and read them in

ix = 1

do x = 1, file_num


  ncdf_file = unique_met(x)

  !Determine if we need to even continue with our loop. If we have fires that
  !do not have met files that match our input, cycle (skip) to the next iteration.

  boolean = 0

  do f = 1, nlines
     if (met_files(f) == unique_met(x)) then
         boolean = 1
     endif
  end do

  if (boolean == 0) then
        write(6,*) 'No fires fall within met input file:'
        write(6,*) unique_met(x)
        write(6,*) ' '
        write(6,*) ' '
      cycle
  end if

  write(6,*) 'Found matching fire data for met input file:'
  write(6,*) unique_met(x)
  write(6,*) ' '


  !Determine which netcdf driver to use depending on the model type
  !specified in the namelist file. Right now the code onlu supports WRF data
  !but added functionality for other met input types will be added later on.
  !Probably smart to have a consistent output option, despite when using
  !met inputs. For now, outputs should include theta [K], water vapor [kg/kg]
  !pressure, density, height [m], and u- and v-winds. If not, a conversion
  !subroutine call is needed.

  if (met_type == 'WRF') then

      !Initialize data pull with random scalar variable, then return our
      !met environmental conditions. Four different calls are needed since
      !WRF u and v-winds, scalars, and geopotentials are on slightly different
      !grids.

      sname = "T"                              !Temperature (mass point)
      gname = "PHB"                            !geopot. (vertical staggered grid)
      uname = "U"                              !U-wind (u staggered grid)
      vname = "V"                              !V-wind (v staggered grid)

      call get_ncdf_dims(ncdf_file,sname,sdims)
      call get_ncdf_dims(ncdf_file,gname,gdims)
      call get_ncdf_dims(ncdf_file,uname,udims)
      call get_ncdf_dims(ncdf_file,vname,vdims)


      write(6,*) "Dimensions of WRF scalar quantities: ",sdims
      write(6,*) "Dimensions of WRF u-wind vectors: ",udims
      write(6,*) "Dimensions of WRF v-wind vectors: ",vdims
      write(6,*) "Dimensions of WRF Geopotential heights: ",gdims
      write(6,*) " "

      allocate (pot(sdims(1),sdims(2),sdims(3),sdims(4)))    ! potential temp [K]
      allocate (qvapor(sdims(1),sdims(2),sdims(3),sdims(4))) ! water vapor [kg/kg]
      allocate (press(sdims(1),sdims(2),sdims(3),sdims(4)))  ! pressure [Pa]
      allocate (densi(sdims(1),sdims(2),sdims(3),sdims(4)))  ! density [kg/m3]
      allocate (geoph(gdims(1),gdims(2),gdims(3),gdims(4)))  ! cartessian height [m] dyn.
      allocate (uwind(udims(1),udims(2),udims(3),udims(4)))  ! u-wind [m/s]
      allocate (vwind(vdims(1),vdims(2),vdims(3),vdims(4)))  ! v-wind [m/s]


      call readin_wrf(ncdf_file,pot,qvapor,press,densi,geoph,uwind,vwind,sdims,udims,vdims,gdims)


      !Call the plume rise model and provide it with fire properties, location
      !and meteorological environment conditions

      sd = sdims(3)
      ud = udims(3)
      gd = gdims(3)


      write(6,*) ' '
      write(6,*) ' '
      write(6,*) '|||||||||||||||||||||||||||||||||||||||||||||'
      write(6,*) 'Succesfully read in WRF data for:'
      write(6,*) unique_met(x)
      write(6,*) '|||||||||||||||||||||||||||||||||||||||||||||'
      write(6,*) ' '
      write(6,*) ' '


  endif

  if (met_type == 'MET_EM') then
         write(6,*) "This data type is not supported yet. Functionality with "
         write(6,*) "met_em files will be added in the Freitas model v2.0 release"
         write(6,*) "Killing our code for now..."
         stop
  endif


  !Loop through each fire that was specified in our input text file. If theres a fire
  !that has a matching input file name (the filename is determined in the pre-processor),
  !then use the met input data and fire location to determine the column we want to
  !pull within out met fields. This column data is process, and fed into the Freitas
  !plume rise model which is called as a last step.

  do f = 1, nlines


     !If the fire we are working on has a metfile name that matches in the met input
     !file that we've read in, we have a hit! Our fire falls within our met file
     !time slice. Lets take the corresponding i j point of our fire, and pull the
     !column data for that point so that it can be fed into the plume rise model.

     if (met_files(f) == unique_met(x)) then

         !Fire locations in time and space
         i = i_points(f)
         j = j_points(f)
         t = model_times(f)

         allocate (col_theta(sdims(3)))
         allocate (col_qvapor(sdims(3)))
         allocate (col_densi(sdims(3)))
         allocate (exner_func(sdims(3)))
         allocate (mid_hgt(sdims(3)))
         allocate (col_geoph(gdims(3)))
         allocate (col_u(udims(3)))
         allocate (col_v(vdims(3)))


         if (met_type == 'WRF') then

             !Format WRF data as a column so it can be ingested within the plume
             !rise model

             call format_wrf(pot,qvapor,press,densi,geoph,uwind,vwind,sdims,&
                             udims,vdims,gdims,i,j,t,col_theta,col_qvapor,&
                             col_densi,col_geoph,exner_func,mid_hgt,col_u,col_v)
         end if


         call smk_pr_driver(dates(f),fire_fluxes(f),fire_areas(f),lats(f),lons(f),&
                            col_theta,col_qvapor,col_densi,col_geoph,exner_func,&
                            mid_hgt,col_u,col_v,sd,ud,gd,ptop)


         !Save fire info so it can be written to a text file later on...

         YYYYMMDDHH(ix) = dates(f)
         flat(ix) = lats(f)
         flon(ix) = lons(f)
         heat_fluxW(ix) = fire_fluxes(f)
         burnt_area(ix) = fire_areas(f)
         ptops(ix) = ptop


         !Do we want to save our individual plume data text file? If overwriteTF
         !is set to FALSE, save the full ouput text file to its own unique name,
         !otherwise it will just be overwritten by the next iteration.

         if (overwriteTF == 'FALSE') then

             fmt = '(I5.5)'
             write (x1,fmt) ix              ! converting integer to string using a 'internal file'

             !call system('mv plumegen.dat plumegen_'//x1//'.dat')
             call system('mv final_plume.dat final_plume_'//x1//'.dat')

         end if


         ix = ix + 1


         deallocate(col_theta)
         deallocate(col_qvapor)
         deallocate(col_densi)
         deallocate(exner_func)
         deallocate(mid_hgt)
         deallocate(col_geoph)
         deallocate(col_u)
         deallocate(col_v)

     endif

  end do


  !We need to allocate and then reallocate for each loop since the size of time
  !dimensions of each WRF input file could change

  deallocate(pot)
  deallocate(qvapor)
  deallocate(press)
  deallocate(densi)
  deallocate(geoph)
  deallocate(uwind)
  deallocate(vwind)


end do

!Save all of our plume rise data to our text output file

call write_plume(YYYYMMDDHH,flat,flon,heat_fluxW,burnt_area,ptops,nlines)


!End

end program pr
