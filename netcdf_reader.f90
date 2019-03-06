!-----------------------------------------------------------------------------------!
!  This module contains the subroutines that are responsible for reading in netcdf  !
!  formatted data into fortran. The first subroutine reads in the dimensions of the !
!  specified variables, which is needed for allocation, thus these are sent back    !
!  to the main program. The second routine opens the specified netcdf file, but     !
!  also reads in the data for the variable that is specified. The third routine     !
!  reads tells the get_ncdf_data routine which variables to pull. These script have !
!  only been tested on WRF-formatted netcdf files, so more subroutines may need     !
!  to be written for other met data file formats.                                   !
!                                                                                   !
!  Subroutines written by Adam Kochanski & modified by Mallia on 1/17/2019          !
!-----------------------------------------------------------------------------------!


module netcdf_func
 contains


   subroutine get_ncdf_dims(input_file,vname,dims)

     implicit none
     INCLUDE 'netcdf.inc'

     character vname*31
     character cval*50
     integer             :: jdim, varid
     parameter (jdim=4)
     integer ishape(jdim)
     integer idim(4)
     integer ncid, status
     character (len=80)  :: input_file, output_file
     character (len=10)  :: option
     logical :: debug=.TRUE.
     integer :: dims(4)
     integer :: i,j,k,ii, itype, idm, ndims, nvars, natt, ngatts
     integer :: iweg, isng, ibtg, nunlimdimid
     character (len=31),allocatable, dimension(:)  :: dname
     integer,           allocatable, dimension(:)  :: dval
     real dx, dy

     ! Open netcdf file and assign and is (ncid)

     ! write(6,*) "Running the get_ncdf_dims"

     status = nf_open(input_file, 0, ncid)


     ! Get basic information about the file 'ncid', number of dimenssions (ndims),
     ! number of variables (nvars), number of global agttributes (ngatts) ,
     ! and ID of unlimited dimenssion (nunlimimid) (time for example)

     status = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)


     ! Allocate arrays for dimenssion names and dimenssion values

     allocate (dval(ndims))
     allocate(dname(ndims))


     ! get the dimenssion details

     do i = 1, ndims
        status = nf_inq_dim(ncid, i, dname(i), dval(i))
     end do


     ! Get information about all variables and select the requested one by assigning varid to it
     ! if the variable is not found stop and write an error

     do i = 1, nvars
        status = nf_inq_var(ncid,i,cval,itype,idm,ishape,natt)

        if (cval==vname) varid=i
     end do

     if (varid==0) then
         write (6,*) "variable ",vname," not foundC "
         STOP "error"
     else     ! get info about the selected variable of id=varid and display it
        status = nf_inq_var(ncid,varid,cval,itype,idm,ishape,natt)
     end if

     ! END IF  C  (varid==0)

     ! assign dimenssion matrix
     do ii = 1,idm
        dims(ii)  = dval(ishape(ii))
     enddo

     deallocate(dname)
     deallocate(dval)

     status=nf_close(ncid)

   end subroutine get_ncdf_dims


   subroutine get_ncdf_data(input_file,vname,data_in,dims_in)

     implicit none
     INCLUDE 'netcdf.inc'
     character vname*31
     character cval*50
     integer  :: jdim, varid
     parameter (jdim=4)
     integer ishape(jdim)
     integer ncid, status
     character (len=80)  :: input_file
     logical :: debug
     integer :: dims_in(jdim), dims_out(jdim-1),nunlimdimid
     integer :: i,j,k,t,ii, itype, idm, ndims, nvars, natt, ngatts
     integer               :: iweg, isng, ibtg
     real, dimension(dims_in(1),dims_in(2),dims_in(3),dims_in(4)) :: data_in
     character (len=31),allocatable, dimension(:)   :: dname
     integer,             allocatable, dimension(:) :: dval
     real dx, dy

     debug=.FALSE.
     varid=0

     ! Get the dimenssion of the variable vname in netcdf file input_file

     IF (debug) write(6,*) "passed dimenssions are:",dims_in


     ! Open netcdf file and assign and is (ncid)

     status = nf_open(input_file, 0, ncid)

     IF (debug) write(6,*) "ncid=",ncid


     ! Get basic information about the file 'ncid', number of dimenssions (ndims),
     ! number of variables (nvars), number of global agttributes (ngatts) ,
     ! and ID of unlimited dimenssion (nunlimimid) (time for example)

     status = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)

     IF (debug) THEN
        write(6,*) "ndims=",ndims," nvars=",nvars
        write(6,*) "ngatts=",ngatts," nunlimdimid=",nunlimdimid
     ENDIF


     ! Get information about all variables and select the requested one by assigning varid to it
     ! if the variable is not found stop and write an error

     do i = 1, nvars
        status = nf_inq_var(ncid, i, cval, itype, idm, ishape, natt)

        IF (debug) THEN
           write(6,*) "ishape=",ishape
           write(6,*) "i=",i," cval=",cval," itype=",itype," idm=",idm," natt=",natt
        END IF

       ! END IF  debug
       if (cval==vname) varid=i
     end do

     if (varid==0) then
        write (6,*) "variable ",vname," not foundC "
        STOP "error"
     else


     ! Get info about the selected variable of id=varid and display it

     status = nf_inq_var(ncid, varid, cval, itype, idm, ishape, natt)


     ! END IF debug

     end if

     ! END IF  (varid==0)


     ! Get basic information about the domain setup

     status = nf_get_att_real (ncid, nf_global, 'DX', dx)
     status = nf_get_att_real (ncid, nf_global, 'DY', dy)
     status = nf_get_att_int (ncid, nf_global,'WEST-EAST_GRID_DIMENSION', iweg)
     status = nf_get_att_int (ncid, nf_global,'SOUTH-NORTH_GRID_DIMENSION', isng)
     status = nf_get_att_int (ncid, nf_global,'BOTTOM-TOP_GRID_DIMENSION', ibtg)

     IF (debug) THEN
         write(6,*) "BASICS from input file:"
         write(6,*) "        DX= ", dx
         write(6,*) "        DY= ", dy
         write(6,*) "         X= ", iweg
         write(6,*) "         Y= ", isng
         write(6,*) "         Z= ", ibtg
     ENDIF


     ! Get data values

     IF (debug) THEN
         write(6,*) "here goes the data:"
         write(6,*) "shape in is",dims_in
         write(6,*) "shape of the subset is",dims_out,"ncdf shape is:",dims_in
     END IF

     ! END IF  debug

     status = nf_get_var_real(ncid, varid, data_in)

     status=nf_close(ncid)


   end subroutine get_ncdf_data


   subroutine readin_wrf(input_file,pot,qvapor,press,densi,geoph,uwind,vwind,&
                           sdims,udims,vdims,gdims)

     implicit none
     INCLUDE 'netcdf.inc'
     integer                           :: sdims(4),udims(4),vdims(4),gdims(4)
     real, dimension(sdims(1),sdims(2),sdims(3),sdims(4)) :: pot,qvapor,press,densi,pert_press
     real, dimension(gdims(1),gdims(2),gdims(3),gdims(4)) :: geoph,pert_geoph
     real, dimension(udims(1),udims(2),udims(3),udims(4)) :: uwind
     real, dimension(vdims(1),vdims(2),vdims(3),vdims(4)) :: vwind
     character (len=80)  :: input_file
     character (len=31)  :: vname1,vname2,vname3,vname4,vname5,vname6,vname7,vname8,vname9


     !Lets read in the necessary variables from our WRF file

     vname1 ="T"
     vname2 ="QVAPOR"
     vname3 ="P"
     vname4 ="PB"
     vname5 ="ALT"
     vname6 ="PH"
     vname7 ="PHB"
     vname8 ="U"
     vname9 ="V"

     write(6,*) "Grabbing Temperature"
     write(6,*) sdims
     call get_ncdf_data(input_file,vname1,pot,sdims)
     write(6,*) "Grabbing Water Vapor"
     write(6,*) sdims
     call get_ncdf_data(input_file,vname2,qvapor,sdims)
     write(6,*) "Grabbing Pressure"
     write(6,*) sdims
     call get_ncdf_data(input_file,vname3,press,sdims)
     write(6,*) "Grabbing Perturbation Pressure"
     write(6,*) sdims
     call get_ncdf_data(input_file,vname4,pert_press,sdims)
     write(6,*) "Grabbing Density"
     write(6,*) sdims
     call get_ncdf_data(input_file,vname5,densi,sdims)
     write(6,*) "Grabbing Geopotential Height"
     write(6,*) gdims
     call get_ncdf_data(input_file,vname6,geoph,gdims)
     write(6,*) "Grabbing Perturbation Geopotential Height"
     write(6,*) gdims
     call get_ncdf_data(input_file,vname7,pert_geoph,gdims)
     write(6,*) "Grabbing u-winds"
     write(6,*) udims
     call get_ncdf_data(input_file,vname8,uwind,udims)
     write(6,*) "Grabbing v-winds"
     write(6,*) vdims
     call get_ncdf_data(input_file,vname9,vwind,vdims)


     !Add our perturbation and the corresponding scalar quantities together.

     geoph = pert_geoph + geoph
     press = pert_press + press


   end subroutine readin_wrf


 end module netcdf_func
