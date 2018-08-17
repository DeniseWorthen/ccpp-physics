!> \file GFS_zhao_carr_pwat.f90
!! This file contains the subroutines that calculates physics/diagnotics variables
!! after calling microphysics scheme:
!! - pwat: column integrated precipitable water

      module GFS_zhao_carr_pwat
      contains

!> \defgroup GFS_zhao_carr_pwat GFS Zhao-Carr PWAT Calculation
!! @{
!! \section arg_table_GFS_zhao_carr_pwat_init Argument Table
!!
      subroutine GFS_zhao_carr_pwat_init
      end subroutine GFS_zhao_carr_pwat_init


!! \section arg_table_GFS_zhao_carr_pwat_run Argument Table
!! | local_name     | standard_name                                                           | long_name                                                               | units       | rank |  type     |   kind    | intent | optional |
!! |----------------|-------------------------------------------------------------------------|-------------------------------------------------------------------------|-------------|------|-----------|-----------|--------|----------|
!! | im             | horizontal_loop_extent                                                  | horizontal loop extent                                                  | count       |    0 | integer   |           | in     | F        |
!! | ix             | horizontal_dimension                                                    | horizontal dimension                                                    | count       |    0 | integer   |           | in     | F        |
!! | levs           | vertical_dimension                                                      | vertical layer dimension                                                | count       |    0 | integer   |           | in     | F        |
!! | del            | air_pressure_difference_between_midlayers                               | air pressure difference between midlayers                               | Pa          |    2 | real      | kind_phys | in     | F        |
!! | ntcw           | index_for_liquid_cloud_condensate                                       | cloud condensate index in tracer array(3)                               | index       |    0 | integer   |           | in     | F        |
!! | ncld           | number_of_hydrometeors                                                  | number of hydrometeors(1 for Z-C)                                       | count       |    0 | integer   |           | in     | F        |
!! | nncl           | number_of_tracers_for_cloud_condensate                                  | number of tracers for cloud condensate                                  | count       |    0 | integer   |           | in     | F        |
!! | cwm            | cloud_condensed_water_mixing_ratio_updated_by_physics                   | moist cloud condensed water mixing ratio                                | kg kg-1     |    2 | real      | kind_phys | in     | F        |
!! | q              | water_vapor_specific_humidity_updated_by_physics                        | water vapor specific humidity                                           | kg kg-1     |    2 | real      | kind_phys | in     | F        |
!! | pwat           | column_precipitable_water                                               | column integrated precipitable water                                    | kg m-2      |    1 | real      | kind_phys | out    | F        |
!! | errmsg         | ccpp_error_message                                                      | error message for error handling in CCPP                                | none        |    0 | character | len=*     | out    | F        |
!! | errflg         | ccpp_error_flag                                                         | error flag for error handling in CCPP                                   | flag        |    0 | integer   |           | out    | F        |
!!
      subroutine GFS_zhao_carr_pwat_run(im,ix,levs,del,          &
                 ntcw,ncld,nncl,cwm,q,              & ! input
                 pwat,errmsg,errflg)              ! output

      use machine,               only: kind_phys
      use physcons,              only:  con_g

      implicit none

      ! Interface variables
      integer,              intent(in) :: im, ix, levs, ntcw, ncld, nncl
      real(kind=kind_phys), dimension(ix,levs), intent(in)    :: del
      real(kind=kind_phys), dimension(ix,levs), intent(in)    :: cwm
      real(kind=kind_phys), dimension(ix,levs), intent(in)    :: q
      real(kind=kind_phys), dimension(im),      intent(out)   :: pwat

      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errflg

      ! Local variables
      integer              :: ic,i,k
     ! real(kind=kind_phys) :: tem
      real(kind=kind_phys), dimension(im) :: work1

!     CONSTANT PARAMETERS
      real(kind=kind_phys), parameter :: one     = 1.0d0, onebg   = one/con_g

      ! Initialize CCPP error handling variables
      errmsg = ''
      errflg = 0

!  --- ...  calculate column precipitable water "pwat"
      do i = 1, im
        pwat(i) = 0.0
      enddo

      !write(0,*) 'zhao_carr_pwat: ncld, ntcw, nncl = ', ncld, ntcw, nncl
      !write(0,*) 'zhao_carr_pwat: max/min cwm = ', maxval(cwm), minval(cwm)
      do k = 1, levs
           do i = 1, im
              work1(i) = 0.0
           enddo
           if (ncld > 0) then
             !do ic = ntcw, ntcw+nncl-1
               do i =1, im
                  !work1(i) = work1(i) +  Stateout%gq0(i,k,ic)
                  work1(i) = work1(i) + cwm(i,k)
               enddo
             !enddo
           endif
           do i = 1,im
           pwat(i) = pwat(i) + del(i,k)*(q(i,k)+work1(i))
           enddo
      enddo

      do i=1,im
         pwat(i) = pwat(i) * onebg
      enddo

      end subroutine GFS_zhao_carr_pwat_run

!! \section arg_table_GFS_zhao_carr_pwat_finalize Argument Table
!!
      subroutine GFS_zhao_carr_pwat_finalize
      end subroutine GFS_zhao_carr_pwat_finalize
!! @}
      end module GFS_zhao_carr_pwat