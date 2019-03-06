!-----------------------------------------------------------------------------------------!
!   This subroutine takes the plume rises which we calculated with the Freitas model and  !
!   outputs the result to a text file, in addition to the fire's lat, lon, and fire       !
!   properties                     Written by DVM on 1/28/2018                            !
!-----------------------------------------------------------------------------------------!

subroutine write_plume(YYYYMMDDHH,flat,flon,heat_fluxW,burnt_area,ptops,nlines)

   real, dimension(nlines)    :: flat,flon,heat_fluxW,burnt_area,ptops
   integer, dimension(nlines) :: YYYYMMDDHH
   integer :: ix,nlines



   do ix = 1, nlines

      if(ix == 1) then

         open(12, file = 'plume_output.dat')
         write (12, 149)
         149 FORMAT('date       lat       lon       heatflux       burnedA       plume_top')

         write (12, 150) YYYYMMDDHH(ix), flat(ix), flon(ix), heat_fluxW(ix), burnt_area(ix), ptops(ix)/1000
         150      format (I12,11F10.1,11F10.1,11F10.1,11F10.1,11F10.1)

      else

         write (12, 150) YYYYMMDDHH(ix), flat(ix), flon(ix), heat_fluxW(ix), burnt_area(ix), ptops(ix)/1000

      end if
  end do


end subroutine write_plume
