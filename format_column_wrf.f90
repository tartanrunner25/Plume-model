!------------------------------------------------------------------------------!
!   WRF meteorological data is passed into this subroutine. Using the provided !
!   i j and t locations of the fires, this script grabs the column data for    !
!   the corresponding point. The data is then manipulated to so that it can be !
!   read in and used by the Freitas plume alone module, which is the following !
!   step.                      DVM 1/24/2018                                   !
!------------------------------------------------------------------------------!


subroutine format_wrf(pot,qvapor,press,densi,geoph,uwind,vwind,sdims,udims,&
                      vdims,gdims,i,j,t,col_theta,col_qvapor,col_densi,&
                      col_geoph,exner_func,mid_hgt,col_u,col_v)

  implicit none
  integer   :: sdims(4),udims(4),vdims(4),gdims(4),k,i,j,t
  real, dimension(sdims(1),sdims(2),sdims(3),sdims(4)) :: pot,qvapor,press,densi
  real, dimension(gdims(1),gdims(2),gdims(3),gdims(4)) :: geoph
  real, dimension(udims(1),udims(2),udims(3),udims(4)) :: uwind
  real, dimension(vdims(1),vdims(2),vdims(3),vdims(4)) :: vwind
  real, dimension(sdims(3)) :: col_theta,col_qvapor,col_densi,col_press
  real, dimension(sdims(3)) :: mid_hgt, exner_func
  real, dimension(gdims(3)) :: col_geoph
  real, dimension(udims(3)) :: col_u
  real, dimension(vdims(3)) :: col_v
  real cp, ro


  !Constants

  cp = 1004.
  ro = 287.

  !Subset our WRF data based on provided i and j points which represents the fire
  !location, will t represents the fire location in time.

  col_theta = pot(i,j,1:sdims(3),t)
  col_qvapor = qvapor(i,j,1:sdims(3),t)
  col_press = press(i,j,1:sdims(3),t)
  col_densi = densi(i,j,1:sdims(3),t)
  col_geoph = geoph(i,j,1:gdims(3),t)
  col_u = uwind(i,j,1:udims(3),t)
  col_v = vwind(i,j,1:vdims(3),t)



  !Lets do some essential unit conversions...

  col_theta = col_theta + 300                       !Perturbation theta to theta [K]
  col_densi = 1/col_densi                           !Convert alpha to density [m3/kg -> kg/m3]
  exner_func = cp * ((col_press/100000)**(ro/cp))   !Convert pressure to Exner function
  col_geoph = col_geoph/9.81                        !Convert geopotential to geopotential height
                                                    !by dividing gravity const m2/s2 / m/s2 = [m]
  col_geoph = col_geoph - col_geoph(1)              !Convert height from ASLm to AGLm


  !Next we need to take our WRF u- and v-winds, which are on staggered grids,
  !and interpolate them to WRF's gridcell mass point centered)

  col_u = (uwind(i,j,1:udims(3),t)+uwind(i+1,j,1:udims(3),t))/2
  col_v = (vwind(i,j,1:vdims(3),t)+vwind(i,j+1,1:vdims(3),t))/2


  !Finally we need to compute the mid-gridcell height to get the "theta-points"
  !or cell mid-plane as required by the Freitas model, we need to take the
  !average of the two vertical levels to get the height at theta location.

  do k = 1,sdims(3)
     mid_hgt(k) = (col_geoph(k) + col_geoph(k+1))/2
  end do


end subroutine format_wrf
