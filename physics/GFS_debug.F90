!> \file GFS_debug.F90


!!
!! This is the place to switch between different debug outputs.
!! - The default behavior for Intel (or any compiler other than GNU)
!!   is to print mininmum, maximum and 32-bit Adler checksum for arrays.
!! - The default behavior for GNU is to mininmum, maximum and
!!   mean value of arrays, because calculating the checksum leads
!!   to segmentation faults with gfortran (bug in malloc?).
!! - If none of the #define preprocessor statements is used,
!!   arrays are printed in full (this is often unpractical).
!! - All output to stdout/stderr from these routines are prefixed
!!   with 'XXX: ' so that they can be easily removed from the log files
!!   using "grep -ve 'XXX: ' ..." if needed.
!! - Only one #define statement can be active at any time
!!
!! Available options for debug output:
!!
!!   #define PRINT_SUM: print mininmum, maximum and mean value of arrays
!!
!!   #define PRINT_CHKSUM: mininmum, maximum and 32-bit Adler checksum for arrays
!!

!#ifdef __GFORTRAN__
!#define PRINT_SUM
!#else
!#define PRINT_CHKSUM
!#endif

!!
!!
!!

    module print_var_chksum

      use machine, only: kind_phys

      implicit none

      private

      public chksum_int, chksum_real, print_var

      interface print_var
        module procedure print_logic_0d
        module procedure print_logic_1d
        module procedure print_int_0d
        module procedure print_int_1d
        module procedure print_int_2d
        module procedure print_real_0d
        module procedure print_real_1d
        module procedure print_real_2d
        module procedure print_real_3d
        module procedure print_real_4d
        module procedure print_2real_1d
      end interface

    contains

      subroutine print_logic_0d(mpirank, omprank, blkno, lat_d, lon_d, name, var)

          integer, intent(in) :: mpirank, omprank, blkno
          real(kind_phys), intent(in) :: lat_d(:), lon_d(:)
          character(len=*), intent(in) :: name
          logical, intent(in) :: var

          write(0,'(2a,3i6,1x,l)') 'XXX: ', trim(name), mpirank, omprank, blkno, var

      end subroutine print_logic_0d

      subroutine print_logic_1d(mpirank, omprank, blkno, lat_d, lon_d, name, var)

          integer, intent(in) :: mpirank, omprank, blkno
          real(kind_phys), intent(in) :: lat_d(:), lon_d(:)
          character(len=*), intent(in) :: name
          logical, intent(in) :: var(:)

          integer :: i

#ifdef PRINT_SUM
          write(0,'(2a,3i6,2i8)') 'XXX: ', trim(name), mpirank, omprank, blkno, size(var), count(var)
#elif defined(PRINT_CHKSUM)
          write(0,'(2a,3i6,2i8)') 'XXX: ', trim(name), mpirank, omprank, blkno, size(var), count(var)
#else
          do i=lbound(var,1),ubound(var,1)
              write(0,'(2a,3i6,i6,2e16.7,1x,l)') 'XXX: ', trim(name), mpirank, omprank, blkno, i, lat_d(i), lon_d(i), var(i)
          end do
#endif

      end subroutine print_logic_1d

      subroutine print_int_0d(mpirank, omprank, blkno, lat_d, lon_d, name, var)

          integer, intent(in) :: mpirank, omprank, blkno
          real(kind_phys), intent(in) :: lat_d(:), lon_d(:)
          character(len=*), intent(in) :: name
          integer, intent(in) :: var

          write(0,'(2a,3i6,i15)') 'XXX: ', trim(name), mpirank, omprank, blkno, var

      end subroutine print_int_0d

      subroutine print_int_1d(mpirank, omprank, blkno, lat_d, lon_d, name, var)

          integer, intent(in) :: mpirank, omprank, blkno
          real(kind_phys), intent(in) :: lat_d(:), lon_d(:)
          character(len=*), intent(in) :: name
          integer, intent(in) :: var(:)

          integer :: i

#ifdef PRINT_SUM
          write(0,'(2a,3i6,3i15)') 'XXX: ', trim(name), mpirank, omprank, blkno, sum(var), minval(var), maxval(var)
#elif defined(PRINT_CHKSUM)
          write(0,'(2a,3i6,i20,2i15)') 'XXX: ', trim(name), mpirank, omprank, blkno, chksum_int(size(var),var), minval(var), maxval(var)
#else
          do i=lbound(var,1),ubound(var,1)
              write(0,'(2a,3i6,i6,2e16.7,i15)') 'XXX: ', trim(name), mpirank, omprank, blkno, i, lat_d(i), lon_d(i), var(i)
          end do
#endif

      end subroutine print_int_1d

      subroutine print_int_2d(mpirank, omprank, blkno, lat_d, lon_d, name, var)

          integer, intent(in) :: mpirank, omprank, blkno
          real(kind_phys), intent(in) :: lat_d(:), lon_d(:)
          character(len=*), intent(in) :: name
          integer, intent(in) :: var(:,:)

          integer :: i, k

#ifdef PRINT_SUM
          write(0,'(2a,3i6,3i15)') 'XXX: ', trim(name), mpirank, omprank, blkno, sum(var), minval(var), maxval(var)
#elif defined(PRINT_CHKSUM)
          write(0,'(2a,3i6,i20,2i15)') 'XXX: ', trim(name), mpirank, omprank, blkno, chksum_int(size(var),var), minval(var), maxval(var)
#else
          do i=lbound(var,1),ubound(var,1)
              do k=lbound(var,2),ubound(var,2)
                  write(0,'(2a,3i6,2i6,2e16.7,i15)') 'XXX: ', trim(name), mpirank, omprank, blkno, i, k, lat_d(i), lon_d(i), var(i,k)
              end do
          end do
#endif

      end subroutine print_int_2d

      subroutine print_real_0d(mpirank, omprank, blkno, lat_d, lon_d, name, var)

          integer, intent(in) :: mpirank, omprank, blkno
          real(kind_phys), intent(in) :: lat_d(:), lon_d(:)
          character(len=*), intent(in) :: name
          real(kind_phys), intent(in) :: var

          write(0,'(2a,3i6,e35.25)') 'XXX: ', trim(name), mpirank, omprank, blkno, var

      end subroutine print_real_0d

      subroutine print_real_1d(mpirank, omprank, blkno, lat_d, lon_d, name, var)

          integer, intent(in) :: mpirank, omprank, blkno
          real(kind_phys), intent(in) :: lat_d(:), lon_d(:)
          character(len=*), intent(in) :: name
          real(kind_phys), intent(in) :: var(:)

          integer :: i

#ifdef PRINT_SUM
          write(0,'(2a,3i6,3e35.25)') 'XXX: ', trim(name), mpirank, omprank, blkno, sum(var), minval(var), maxval(var)
#elif defined(PRINT_CHKSUM)
          write(0,'(2a,3i6,i20,2e35.25)') 'XXX: ', trim(name), mpirank, omprank, blkno, chksum_real(size(var),var), minval(var), maxval(var)
#else
          do i=lbound(var,1),ubound(var,1)
!             if (abs(lat_d(i)) <= 3.0 .and. abs(lon_d(i)) >= 120.0 .and. abs(lon_d(i)) <= 123.0) then
!             if (abs(var(i)) <= 3.0) then
                write(0,'(2a,3i6,i6,2e16.7,e35.25)') 'YYY: ', trim(name), mpirank, omprank, blkno, i, lat_d(i), lon_d(i), var(i)
!             end if
          end do
#endif

      end subroutine print_real_1d


      subroutine print_2real_1d(mpirank, omprank, blkno, lat_d, lon_d, name, var1, var2)

          integer, intent(in) :: mpirank, omprank, blkno
          real(kind_phys), intent(in) :: lat_d(:), lon_d(:)
          character(len=*), intent(in) :: name
          real(kind_phys), intent(in) :: var1(:),var2(:)

          integer :: i

!#ifdef PRINT_SUM
!          write(0,'(2a,3i6,3e35.25)') 'XXX: ', trim(name), mpirank, omprank, blkno, sum(var), minval(var), maxval(var)
!#elif defined(PRINT_CHKSUM)
!          write(0,'(2a,3i6,i20,2e35.25)') 'XXX: ', trim(name), mpirank, omprank, blkno, chksum_real(size(var),var), minval(var), maxval(var)
!#else
          do i=lbound(var1,1),ubound(var1,1)
!             if (abs(lat_d(i)) <= 3.0 .and. abs(lon_d(i)) >= 120.0 .and. abs(lon_d(i)) <= 123.0) then
!             if (abs(var(i)) <= 3.0) then
                write(0,'(2a,3i6,i6,2e16.7,2e35.25)') 'YYY: ', trim(name), mpirank, omprank, blkno, i, lat_d(i), lon_d(i), var1(i), var2(i)
!             end if
          end do
!#endif

      end subroutine print_2real_1d

      subroutine print_real_2d(mpirank, omprank, blkno, lat_d, lon_d, name, var)

          integer, intent(in) :: mpirank, omprank, blkno
          real(kind_phys), intent(in) :: lat_d(:), lon_d(:)
          character(len=*), intent(in) :: name
          real(kind_phys), intent(in) :: var(:,:)

          integer :: k, i

#ifdef PRINT_SUM
          write(0,'(2a,3i6,3e35.25)') 'XXX: ', trim(name), mpirank, omprank, blkno, sum(var), minval(var), maxval(var)
#elif defined(PRINT_CHKSUM)
          write(0,'(2a,3i6,i20,2e35.25)') 'XXX: ', trim(name), mpirank, omprank, blkno, chksum_real(size(var),reshape(var,(/size(var)/))), minval(var), maxval(var)
#else
          do i=lbound(var,1),ubound(var,1)
              do k=lbound(var,2),ubound(var,2)
                  write(0,'(2a,3i6,2i6,2e16.7,e35.25)') 'XXX: ', trim(name), mpirank, omprank, blkno, i, k, lat_d(i), lon_d(i), var(i,k)
              end do
          end do
#endif

      end subroutine print_real_2d

      subroutine print_real_3d(mpirank, omprank, blkno, lat_d, lon_d, name, var)

          integer, intent(in) :: mpirank, omprank, blkno
          real(kind_phys), intent(in) :: lat_d(:), lon_d(:)
          character(len=*), intent(in) :: name
          real(kind_phys), intent(in) :: var(:,:,:)

          integer :: k, i, l

#ifdef PRINT_SUM
          write(0,'(2a,3i6,3e35.25)') 'XXX: ', trim(name), mpirank, omprank, blkno, sum(var), minval(var), maxval(var)
#elif defined(PRINT_CHKSUM)
          write(0,'(2a,3i6,i20,2e35.25)') 'XXX: ', trim(name), mpirank, omprank, blkno, chksum_real(size(var),reshape(var,(/size(var)/))), minval(var), maxval(var)
#else
          do i=lbound(var,1),ubound(var,1)
              do k=lbound(var,2),ubound(var,2)
                  do l=lbound(var,3),ubound(var,3)
                      write(0,'(2a,3i6,3i6,2e16.7,e35.25)') 'XXX: ', trim(name), mpirank, omprank, blkno, i, k, l, lat_d(i), lon_d(i), var(i,k,l)
                  end do
              end do
          end do
#endif

      end subroutine print_real_3d

      subroutine print_real_4d(mpirank, omprank, blkno, lat_d, lon_d, name, var)

          integer, intent(in) :: mpirank, omprank, blkno
          real(kind_phys), intent(in) :: lat_d(:), lon_d(:)
          character(len=*), intent(in) :: name
          real(kind_phys), intent(in) :: var(:,:,:,:)

          integer :: k, i, l, m

#ifdef PRINT_SUM
          write(0,'(2a,3i6,3e35.25)') 'XXX: ', trim(name), mpirank, omprank, blkno, sum(var), minval(var), maxval(var)
#elif defined(PRINT_CHKSUM)
          write(0,'(2a,3i6,i20,2e35.25)') 'XXX: ', trim(name), mpirank, omprank, blkno, chksum_real(size(var),reshape(var,(/size(var)/))), minval(var), maxval(var)
#else
          do i=lbound(var,1),ubound(var,1)
              do k=lbound(var,2),ubound(var,2)
                  do l=lbound(var,3),ubound(var,3)
                      do m=lbound(var,4),ubound(var,4)
                          write(0,'(2a,3i6,4i6,2e16.7,e35.25)') 'XXX: ', trim(name), mpirank, omprank, blkno, i, k, l, m, lat_d(i), lon_d(i), var(i,k,l,m)
                      end do
                  end do
              end do
          end do
#endif

      end subroutine print_real_4d

      function chksum_int(N, var) result(hash)

          integer, intent(in) :: N
          integer, dimension(1:N), intent(in) :: var
          integer*8, dimension(1:N) :: int_var
          integer*8 :: a, b, i, hash
          integer*8, parameter :: mod_adler=65521

          a=1
          b=0
          i=1
          hash = 0
          int_var = TRANSFER(var, a, N)

          do i= 1, N
              a = MOD(a + int_var(i), mod_adler)
              b = MOD(b+a, mod_adler)
          end do

          hash = ior(b * 65536, a)

      end function chksum_int

      function chksum_real(N, var) result(hash)

          integer, intent(in) :: N
          real(kind_phys), dimension(1:N), intent(in) :: var
          integer*8, dimension(1:N) :: int_var
          integer*8 :: a, b, i, hash
          integer*8, parameter :: mod_adler=65521

          a=1
          b=0
          i=1
          hash = 0
          int_var = TRANSFER(var, a, N)

          do i= 1, N
              a = MOD(a + int_var(i), mod_adler)
              b = MOD(b+a, mod_adler)
          end do

          hash = ior(b * 65536, a)

      end function chksum_real

    end module print_var_chksum

    module GFS_diagtoscreen

      use print_var_chksum, only: print_var

      implicit none

      private

      public GFS_diagtoscreen_init, GFS_diagtoscreen_timestep_init, GFS_diagtoscreen_run

      contains

!> \section arg_table_GFS_diagtoscreen_init Argument Table
!! \htmlinclude GFS_diagtoscreen_init.html
!!
      subroutine GFS_diagtoscreen_init (Model, Data, Interstitial, errmsg, errflg)

         use GFS_typedefs,  only: GFS_control_type, GFS_data_type
         use CCPP_typedefs, only: GFS_interstitial_type

         implicit none

         !--- interface variables
         type(GFS_control_type),      intent(in)  :: Model
         type(GFS_data_type),         intent(in)  :: Data(:)
         type(GFS_interstitial_type), intent(in)  :: Interstitial(:)
         character(len=*),            intent(out) :: errmsg
         integer,                     intent(out) :: errflg

         !--- local variables
         integer :: i

         ! Initialize CCPP error handling variables
         errmsg = ''
         errflg = 0

         do i=1,size(Data)
           call GFS_diagtoscreen_run (Model, Data(i)%Statein, Data(i)%Stateout, Data(i)%Sfcprop,    &
                                      Data(i)%Coupling, Data(i)%Grid, Data(i)%Tbd, Data(i)%Cldprop, &
                                      Data(i)%Radtend, Data(i)%Intdiag, Interstitial(1),            &
                                      size(Interstitial), i, errmsg, errflg)
         end do

      end subroutine GFS_diagtoscreen_init

!> \section arg_table_GFS_diagtoscreen_timestep_init Argument Table
!! \htmlinclude GFS_diagtoscreen_timestep_init.html
!!
      subroutine GFS_diagtoscreen_timestep_init (Model, Data, Interstitial, errmsg, errflg)

         use GFS_typedefs,  only: GFS_control_type, GFS_data_type
         use CCPP_typedefs, only: GFS_interstitial_type

         implicit none

         !--- interface variables
         type(GFS_control_type),      intent(in)  :: Model
         type(GFS_data_type),         intent(in)  :: Data(:)
         type(GFS_interstitial_type), intent(in)  :: Interstitial(:)
         character(len=*),            intent(out) :: errmsg
         integer,                     intent(out) :: errflg

         !--- local variables
         integer :: i

         ! Initialize CCPP error handling variables
         errmsg = ''
         errflg = 0

         do i=1,size(Data)
           call GFS_diagtoscreen_run (Model, Data(i)%Statein, Data(i)%Stateout, Data(i)%Sfcprop,    &
                                      Data(i)%Coupling, Data(i)%Grid, Data(i)%Tbd, Data(i)%Cldprop, &
                                      Data(i)%Radtend, Data(i)%Intdiag, Interstitial(1),            &
                                      size(Interstitial), i, errmsg, errflg)
         end do

      end subroutine GFS_diagtoscreen_timestep_init

!> \section arg_table_GFS_diagtoscreen_run Argument Table
!! \htmlinclude GFS_diagtoscreen_run.html
!!
      subroutine GFS_diagtoscreen_run (Model, Statein, Stateout, Sfcprop, Coupling,     &
                                       Grid, Tbd, Cldprop, Radtend, Diag, Interstitial, &
                                       nthreads, blkno, errmsg, errflg)

#ifdef MPI
         use mpi
#endif
#ifdef _OPENMP
         use omp_lib
#endif
         use GFS_typedefs,          only: GFS_control_type, GFS_statein_type,  &
                                          GFS_stateout_type, GFS_sfcprop_type, &
                                          GFS_coupling_type, GFS_grid_type,    &
                                          GFS_tbd_type, GFS_cldprop_type,      &
                                          GFS_radtend_type, GFS_diag_type
         use CCPP_typedefs,         only: GFS_interstitial_type

         implicit none

         !--- interface variables
         type(GFS_control_type),   intent(in   ) :: Model
         type(GFS_statein_type),   intent(in   ) :: Statein
         type(GFS_stateout_type),  intent(in   ) :: Stateout
         type(GFS_sfcprop_type),   intent(in   ) :: Sfcprop
         type(GFS_coupling_type),  intent(in   ) :: Coupling
         type(GFS_grid_type),      intent(in   ) :: Grid
         type(GFS_tbd_type),       intent(in   ) :: Tbd
         type(GFS_cldprop_type),   intent(in   ) :: Cldprop
         type(GFS_radtend_type),   intent(in   ) :: Radtend
         type(GFS_diag_type),      intent(in   ) :: Diag
         type(GFS_interstitial_type), intent(in) :: Interstitial
         integer,                  intent(in   ) :: nthreads
         integer,                  intent(in   ) :: blkno
         character(len=*),           intent(out) :: errmsg
         integer,                    intent(out) :: errflg

         !--- local variables
         integer :: impi, iomp, ierr, n, idtend, iprocess, itracer
         integer :: mpirank, mpisize, mpicomm
         integer :: omprank, ompsize

         ! Initialize CCPP error handling variables
         errmsg = ''
         errflg = 0

#ifdef MPI
         mpicomm = Model%communicator
         mpirank = Model%me
         mpisize = Model%ntasks
#else
         mpirank = 0
         mpisize = 1
         mpicomm = 0
#endif
#ifdef _OPENMP
         omprank = OMP_GET_THREAD_NUM()
         ompsize = nthreads
#else
         omprank = 0
         ompsize = 1
#endif

#ifdef _OPENMP
!$OMP BARRIER
#endif
#ifdef MPI
!         call MPI_BARRIER(mpicomm,ierr)
#endif

         do impi=0,mpisize-1
             do iomp=0,ompsize-1
                 if (mpirank==impi .and. omprank==iomp) then
                     !call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Model%kdt'         , Model%kdt)
                     !
                     ! Grid
                     !call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Grid%xlon  ', Grid%xlon_d  )
                    if (Model%kdt .eq. 2) then
                       call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Grid%xlat,lon  ', Grid%xlat_d, Grid%xlon_d  )
                    end if
                 end if
#ifdef _OPENMP
!$OMP BARRIER
#endif
             end do
#ifdef MPI
!             call MPI_BARRIER(mpicomm,ierr)
#endif
         end do

#ifdef _OPENMP
!$OMP BARRIER
#endif
#ifdef MPI
!         call MPI_BARRIER(mpicomm,ierr)
#endif

      end subroutine GFS_diagtoscreen_run

    end module GFS_diagtoscreen


    module GFS_interstitialtoscreen

      use print_var_chksum, only: print_var

      implicit none

      private

      public GFS_interstitialtoscreen_init, GFS_interstitialtoscreen_timestep_init, GFS_interstitialtoscreen_run

      contains

!> \section arg_table_GFS_interstitialtoscreen_init Argument Table
!! \htmlinclude GFS_interstitialtoscreen_init.html
!!
      subroutine GFS_interstitialtoscreen_init (Model, Data, Interstitial, errmsg, errflg)

         use GFS_typedefs,  only: GFS_control_type, GFS_data_type
         use CCPP_typedefs, only: GFS_interstitial_type

         implicit none

         !--- interface variables
         type(GFS_control_type),      intent(in)  :: Model
         type(GFS_data_type),         intent(in)  :: Data(:)
         type(GFS_interstitial_type), intent(in)  :: Interstitial(:)
         character(len=*),            intent(out) :: errmsg
         integer,                     intent(out) :: errflg

         !--- local variables
         integer :: i

         ! Initialize CCPP error handling variables
         errmsg = ''
         errflg = 0


         do i=1,size(Interstitial)
           call GFS_interstitialtoscreen_run (Model, Data(1)%Statein, Data(1)%Stateout, Data(1)%Sfcprop,    &
                                              Data(1)%Coupling, Data(1)%Grid, Data(1)%Tbd, Data(1)%Cldprop, &
                                              Data(1)%Radtend, Data(1)%Intdiag, Interstitial(i),            &
                                              size(Interstitial), -999, errmsg, errflg)
         end do

      end subroutine GFS_interstitialtoscreen_init

!> \section arg_table_GFS_interstitialtoscreen_timestep_init Argument Table
!! \htmlinclude GFS_interstitialtoscreen_timestep_init.html
!!
      subroutine GFS_interstitialtoscreen_timestep_init (Model, Data, Interstitial, errmsg, errflg)

         use GFS_typedefs,  only: GFS_control_type, GFS_data_type
         use CCPP_typedefs, only: GFS_interstitial_type

         implicit none

         !--- interface variables
         type(GFS_control_type),      intent(in)  :: Model
         type(GFS_data_type),         intent(in)  :: Data(:)
         type(GFS_interstitial_type), intent(in)  :: Interstitial(:)
         character(len=*),            intent(out) :: errmsg
         integer,                     intent(out) :: errflg

         !--- local variables
         integer :: i

         ! Initialize CCPP error handling variables
         errmsg = ''
         errflg = 0


         do i=1,size(Interstitial)
           call GFS_interstitialtoscreen_run (Model, Data(1)%Statein, Data(1)%Stateout, Data(1)%Sfcprop,    &
                                              Data(1)%Coupling, Data(1)%Grid, Data(1)%Tbd, Data(1)%Cldprop, &
                                              Data(1)%Radtend, Data(1)%Intdiag, Interstitial(i),            &
                                              size(Interstitial), -999, errmsg, errflg)
         end do

      end subroutine GFS_interstitialtoscreen_timestep_init

!> \section arg_table_GFS_interstitialtoscreen_run Argument Table
!! \htmlinclude GFS_interstitialtoscreen_run.html
!!
      subroutine GFS_interstitialtoscreen_run (Model, Statein, Stateout, Sfcprop, Coupling, &
                                           Grid, Tbd, Cldprop, Radtend, Diag, Interstitial, &
                                           nthreads, blkno, errmsg, errflg)

#ifdef MPI
         use mpi
#endif
#ifdef _OPENMP
         use omp_lib
#endif
         use machine,               only: kind_phys
         use GFS_typedefs,          only: GFS_control_type, GFS_statein_type,  &
                                          GFS_stateout_type, GFS_sfcprop_type, &
                                          GFS_coupling_type, GFS_grid_type,    &
                                          GFS_tbd_type, GFS_cldprop_type,      &
                                          GFS_radtend_type, GFS_diag_type
         use CCPP_typedefs,         only: GFS_interstitial_type

         implicit none

         !--- interface variables
         type(GFS_control_type),   intent(in   ) :: Model
         type(GFS_statein_type),   intent(in   ) :: Statein
         type(GFS_stateout_type),  intent(in   ) :: Stateout
         type(GFS_sfcprop_type),   intent(in   ) :: Sfcprop
         type(GFS_coupling_type),  intent(in   ) :: Coupling
         type(GFS_grid_type),      intent(in   ) :: Grid
         type(GFS_tbd_type),       intent(in   ) :: Tbd
         type(GFS_cldprop_type),   intent(in   ) :: Cldprop
         type(GFS_radtend_type),   intent(in   ) :: Radtend
         type(GFS_diag_type),      intent(in   ) :: Diag
         type(GFS_interstitial_type), intent(in) :: Interstitial
         integer,                  intent(in   ) :: nthreads
         integer,                  intent(in   ) :: blkno
         character(len=*),         intent(  out) :: errmsg
         integer,                  intent(  out) :: errflg

         !--- local variables
         integer :: impi, iomp, ierr
         integer :: mpirank, mpisize, mpicomm
         integer :: omprank, ompsize
         integer :: istart, iend, kstart, kend

         ! Initialize CCPP error handling variables
         errmsg = ''
         errflg = 0

#ifdef MPI
         mpicomm = Model%communicator
         mpirank = Model%me
         call MPI_COMM_SIZE(mpicomm, mpisize, ierr)
#else
         mpirank = 0
         mpisize = 1
         mpicomm = 0
#endif
#ifdef _OPENMP
         omprank = OMP_GET_THREAD_NUM()
         ompsize = nthreads
#else
         omprank = 0
         ompsize = 1
#endif

#ifdef _OPENMP
!$OMP BARRIER
#endif
#ifdef MPI
!         call MPI_BARRIER(mpicomm,ierr)
#endif

         do impi=0,mpisize-1
             do iomp=0,ompsize-1
                 if (mpirank==impi .and. omprank==iomp) then
                     ! Print static variables
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ipr                 ', Interstitial%ipr                     )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%itc                 ', Interstitial%itc                     )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%latidxprnt          ', Interstitial%latidxprnt              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%levi                ', Interstitial%levi                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%lmk                 ', Interstitial%lmk                     )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%lmp                 ', Interstitial%lmp                     )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%nbdlw               ', Interstitial%nbdlw                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%nbdsw               ', Interstitial%nbdsw                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%nf_aelw             ', Interstitial%nf_aelw                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%nf_aesw             ', Interstitial%nf_aesw                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%nsamftrac           ', Interstitial%nsamftrac               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%nscav               ', Interstitial%nscav                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%nspc1               ', Interstitial%nspc1                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ntiwx               ', Interstitial%ntiwx                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%nvdiff              ', Interstitial%nvdiff                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%phys_hydrostatic    ', Interstitial%phys_hydrostatic        )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%skip_macro          ', Interstitial%skip_macro              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%trans_aero          ', Interstitial%trans_aero              )
                     ! Print all other variables
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%adjsfculw_land      ', Interstitial%adjsfculw_land          )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%adjsfculw_ice       ', Interstitial%adjsfculw_ice           )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%adjsfculw_water     ', Interstitial%adjsfculw_water         )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%adjnirbmd           ', Interstitial%adjnirbmd               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%adjnirbmu           ', Interstitial%adjnirbmu               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%adjnirdfd           ', Interstitial%adjnirdfd               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%adjnirdfu           ', Interstitial%adjnirdfu               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%adjvisbmd           ', Interstitial%adjvisbmd               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%adjvisbmu           ', Interstitial%adjvisbmu               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%adjvisdfu           ', Interstitial%adjvisdfu               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%adjvisdfd           ', Interstitial%adjvisdfd               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%aerodp              ', Interstitial%aerodp                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%alb1d               ', Interstitial%alb1d                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%bexp1d              ', Interstitial%bexp1d                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cd                  ', Interstitial%cd                      )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cd_ice              ', Interstitial%cd_ice                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cd_land             ', Interstitial%cd_land                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cd_water            ', Interstitial%cd_water                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cdq                 ', Interstitial%cdq                     )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cdq_ice             ', Interstitial%cdq_ice                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cdq_land            ', Interstitial%cdq_land                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cdq_water           ', Interstitial%cdq_water               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%chh_ice             ', Interstitial%chh_ice                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%chh_land            ', Interstitial%chh_land                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%chh_water           ', Interstitial%chh_water               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cldf                ', Interstitial%cldf                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cldsa               ', Interstitial%cldsa                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cldtaulw            ', Interstitial%cldtaulw                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cldtausw            ', Interstitial%cldtausw                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cld1d               ', Interstitial%cld1d                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%clw                 ', Interstitial%clw                     )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%clx                 ', Interstitial%clx                     )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%clouds              ', Interstitial%clouds                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cmm_ice             ', Interstitial%cmm_ice                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cmm_land            ', Interstitial%cmm_land                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cmm_water           ', Interstitial%cmm_water               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cnvc                ', Interstitial%cnvc                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cnvw                ', Interstitial%cnvw                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ctei_r              ', Interstitial%ctei_r                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ctei_rml            ', Interstitial%ctei_rml                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cumabs              ', Interstitial%cumabs                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dd_mf               ', Interstitial%dd_mf                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%de_lgth             ', Interstitial%de_lgth                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%del                 ', Interstitial%del                     )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%del_gz              ', Interstitial%del_gz                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%delr                ', Interstitial%delr                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dlength             ', Interstitial%dlength                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dqdt                ', Interstitial%dqdt                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dqsfc1              ', Interstitial%dqsfc1                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%drain               ', Interstitial%drain                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dtdt                ', Interstitial%dtdt                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dtsfc1              ', Interstitial%dtsfc1                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dtzm                ', Interstitial%dtzm                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dt_mf               ', Interstitial%dt_mf                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dudt                ', Interstitial%dudt                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dusfcg              ', Interstitial%dusfcg                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dusfc1              ', Interstitial%dusfc1                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dvdftra             ', Interstitial%dvdftra                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dvdt                ', Interstitial%dvdt                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dvsfcg              ', Interstitial%dvsfcg                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dvsfc1              ', Interstitial%dvsfc1                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dzlyr               ', Interstitial%dzlyr                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%elvmax              ', Interstitial%elvmax                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ep1d                ', Interstitial%ep1d                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ep1d_ice            ', Interstitial%ep1d_ice                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ep1d_land           ', Interstitial%ep1d_land               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ep1d_water          ', Interstitial%ep1d_water              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%evap_ice            ', Interstitial%evap_ice                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%evap_land           ', Interstitial%evap_land               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%evap_water          ', Interstitial%evap_water              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ext_diag_thompson_reset', Interstitial%ext_diag_thompson_reset)
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%faerlw              ', Interstitial%faerlw                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%faersw              ', Interstitial%faersw                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ffhh_ice            ', Interstitial%ffhh_ice                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ffhh_land           ', Interstitial%ffhh_land               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ffhh_water          ', Interstitial%ffhh_water              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fh2                 ', Interstitial%fh2                     )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fh2_ice             ', Interstitial%fh2_ice                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fh2_land            ', Interstitial%fh2_land                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fh2_water           ', Interstitial%fh2_water               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%flag_cice           ', Interstitial%flag_cice               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%flag_guess          ', Interstitial%flag_guess              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%flag_iter           ', Interstitial%flag_iter               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ffmm_ice            ', Interstitial%ffmm_ice                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ffmm_land           ', Interstitial%ffmm_land               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ffmm_water          ', Interstitial%ffmm_water              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fm10                ', Interstitial%fm10                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fm10_ice            ', Interstitial%fm10_ice                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fm10_land           ', Interstitial%fm10_land               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fm10_water          ', Interstitial%fm10_water              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%frain               ', Interstitial%frain                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%frland              ', Interstitial%frland                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fscav               ', Interstitial%fscav                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fswtr               ', Interstitial%fswtr                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%gabsbdlw            ', Interstitial%gabsbdlw                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%gabsbdlw_ice        ', Interstitial%gabsbdlw_ice            )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%gabsbdlw_land       ', Interstitial%gabsbdlw_land           )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%gabsbdlw_water      ', Interstitial%gabsbdlw_water          )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%gamma               ', Interstitial%gamma                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%gamq                ', Interstitial%gamq                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%gamt                ', Interstitial%gamt                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%gasvmr              ', Interstitial%gasvmr                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%gflx                ', Interstitial%gflx                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%gflx_ice            ', Interstitial%gflx_ice                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%gflx_land           ', Interstitial%gflx_land               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%gflx_water          ', Interstitial%gflx_water              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%gwdcu               ', Interstitial%gwdcu                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%gwdcv               ', Interstitial%gwdcv                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%zvfun               ', Interstitial%zvfun                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%hffac               ', Interstitial%hffac                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%hflxq               ', Interstitial%hflxq                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%hflx_ice            ', Interstitial%hflx_ice                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%hflx_land           ', Interstitial%hflx_land               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%hflx_water          ', Interstitial%hflx_water              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%htlwc               ', Interstitial%htlwc                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%htlw0               ', Interstitial%htlw0                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%htswc               ', Interstitial%htswc                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%htsw0               ', Interstitial%htsw0                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dry                 ', Interstitial%dry                     )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%idxday              ', Interstitial%idxday                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%icy                 ', Interstitial%icy                     )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%lake                ', Interstitial%lake                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ocean               ', Interstitial%ocean                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%islmsk              ', Interstitial%islmsk                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%islmsk_cice         ', Interstitial%islmsk_cice             )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%wet                 ', Interstitial%wet                     )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%kb                  ', Interstitial%kb                      )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%kbot                ', Interstitial%kbot                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%kcnv                ', Interstitial%kcnv                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%kd                  ', Interstitial%kd                      )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%kinver              ', Interstitial%kinver                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%kpbl                ', Interstitial%kpbl                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%kt                  ', Interstitial%kt                      )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ktop                ', Interstitial%ktop                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%max_hourly_reset    ', Interstitial%max_hourly_reset        )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%mbota               ', Interstitial%mbota                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%mtopa               ', Interstitial%mtopa                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%nday                ', Interstitial%nday                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%oa4                 ', Interstitial%oa4                     )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%oc                  ', Interstitial%oc                      )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%olyr                ', Interstitial%olyr                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%plvl                ', Interstitial%plvl                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%plyr                ', Interstitial%plyr                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%prcpmp              ', Interstitial%prcpmp                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%prnum               ', Interstitial%prnum                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%qlyr                ', Interstitial%qlyr                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%qss_ice             ', Interstitial%qss_ice                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%qss_land            ', Interstitial%qss_land                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%qss_water           ', Interstitial%qss_water               )
!                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%radar_reset         ', Interstitial%radar_reset             )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%raddt               ', Interstitial%raddt                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%raincd              ', Interstitial%raincd                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%raincs              ', Interstitial%raincs                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%rainmcadj           ', Interstitial%rainmcadj               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%rainp               ', Interstitial%rainp                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%rb                  ', Interstitial%rb                      )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%rb_ice              ', Interstitial%rb_ice                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%rb_land             ', Interstitial%rb_land                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%rb_water            ', Interstitial%rb_water                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%rhc                 ', Interstitial%rhc                     )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%runoff              ', Interstitial%runoff                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%save_q              ', Interstitial%save_q                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%save_t              ', Interstitial%save_t                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%save_tcp            ', Interstitial%save_tcp                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%save_u              ', Interstitial%save_u                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%save_v              ', Interstitial%save_v                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%scmpsw%uvbfc        ', Interstitial%scmpsw%uvbfc            )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%scmpsw%uvbf0        ', Interstitial%scmpsw%uvbf0            )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%scmpsw%nirbm        ', Interstitial%scmpsw%nirbm            )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%scmpsw%nirdf        ', Interstitial%scmpsw%nirdf            )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%scmpsw%visbm        ', Interstitial%scmpsw%visbm            )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%scmpsw%visdf        ', Interstitial%scmpsw%visdf            )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%sfcalb              ', Interstitial%sfcalb                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%sigma               ', Interstitial%sigma                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%sigmaf              ', Interstitial%sigmaf                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%sigmafrac           ', Interstitial%sigmafrac               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%sigmatot            ', Interstitial%sigmatot                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%snowc               ', Interstitial%snowc                   )
!                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%snowd_ice           ', Interstitial%snowd_ice               )
!                    call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%snowd_land          ', Interstitial%snowd_land              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%snohf               ', Interstitial%snohf                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%snowmt              ', Interstitial%snowmt                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%stress              ', Interstitial%stress                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%stress_ice          ', Interstitial%stress_ice              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%stress_land         ', Interstitial%stress_land             )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%stress_water        ', Interstitial%stress_water            )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%theta               ', Interstitial%theta                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tlvl                ', Interstitial%tlvl                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tlyr                ', Interstitial%tlyr                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tprcp_ice           ', Interstitial%tprcp_ice               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tprcp_land          ', Interstitial%tprcp_land              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tprcp_water         ', Interstitial%tprcp_water             )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tseal               ', Interstitial%tseal                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tsfa                ', Interstitial%tsfa                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tsfc_water          ', Interstitial%tsfc_water              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tsfg                ', Interstitial%tsfg                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tsurf_ice           ', Interstitial%tsurf_ice               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tsurf_land          ', Interstitial%tsurf_land              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tsurf_water         ', Interstitial%tsurf_water             )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%uustar_ice          ', Interstitial%uustar_ice              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%uustar_land         ', Interstitial%uustar_land             )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%uustar_water        ', Interstitial%uustar_water            )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%vdftra              ', Interstitial%vdftra                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%vegf1d              ', Interstitial%vegf1d                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%wcbmax              ', Interstitial%wcbmax                  )
!                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%weasd_ice           ', Interstitial%weasd_ice              )
!                    call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%weasd_land          ', Interstitial%weasd_land              )
!                    call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%weasd_water         ', Interstitial%weasd_water             )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%wind                ', Interstitial%wind                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%work1               ', Interstitial%work1                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%work2               ', Interstitial%work2                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%work3               ', Interstitial%work3                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%xcosz               ', Interstitial%xcosz                   )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%xlai1d              ', Interstitial%xlai1d                  )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%xmu                 ', Interstitial%xmu                     )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%z01d                ', Interstitial%z01d                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%zt1d                ', Interstitial%zt1d                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ztmax_ice           ', Interstitial%ztmax_ice               )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ztmax_land          ', Interstitial%ztmax_land              )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ztmax_water         ', Interstitial%ztmax_water             )
                     ! UGWP
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tau_mtb             ', Interstitial%tau_mtb                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tau_ogw             ', Interstitial%tau_ogw                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tau_tofd            ', Interstitial%tau_tofd                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tau_ngw             ', Interstitial%tau_ngw                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tau_oss             ', Interstitial%tau_oss                 )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dudt_mtb            ', Interstitial%dudt_mtb                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dudt_tms            ', Interstitial%dudt_tms                )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%zmtb                ', Interstitial%zmtb                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%zlwb                ', Interstitial%zlwb                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%zogw                ', Interstitial%zogw                    )
                     call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%zngw                ', Interstitial%zngw                    )
                     ! UGWP v1
                     if (Model%do_ugwp_v1) then
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dudt_ngw            ', Interstitial%dudt_ngw                )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dvdt_ngw            ', Interstitial%dvdt_ngw                )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%dtdt_ngw            ', Interstitial%dtdt_ngw                )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%kdis_ngw            ', Interstitial%kdis_ngw                )
                     end if
                     !-- GSD drag suite
                     if (Model%gwd_opt==3 .or. Model%gwd_opt==33 .or. &
                         Model%gwd_opt==2 .or. Model%gwd_opt==22) then
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%varss               ', Interstitial%varss                   )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ocss                ', Interstitial%ocss                    )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%oa4ss               ', Interstitial%oa4ss                   )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%clxss               ', Interstitial%clxss                   )
                     end if
                     ! GFDL and Thompson MP
                     if (Model%imp_physics == Model%imp_physics_gfdl .or. Model%imp_physics == Model%imp_physics_thompson .or. Model%imp_physics == Model%imp_physics_nssl) then
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%graupelmp           ', Interstitial%graupelmp               )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%icemp               ', Interstitial%icemp                   )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%rainmp              ', Interstitial%rainmp                  )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%snowmp              ', Interstitial%snowmp                  )
                     ! Morrison-Gettelman
                     else if (Model%imp_physics == Model%imp_physics_mg) then
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ncgl                ', Interstitial%ncgl                    )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ncpr                ', Interstitial%ncpr                    )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ncps                ', Interstitial%ncps                    )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%qgl                 ', Interstitial%qgl                     )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%qrn                 ', Interstitial%qrn                     )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%qsnw                ', Interstitial%qsnw                    )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%qlcn                ', Interstitial%qlcn                    )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%qicn                ', Interstitial%qicn                    )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%w_upi               ', Interstitial%w_upi                   )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cf_upi              ', Interstitial%cf_upi                  )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cnv_mfd             ', Interstitial%cnv_mfd                 )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cnv_dqldt           ', Interstitial%cnv_dqldt               )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%clcn                ', Interstitial%clcn                    )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cnv_fice            ', Interstitial%cnv_fice                )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cnv_ndrop           ', Interstitial%cnv_ndrop               )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cnv_nice            ', Interstitial%cnv_nice                )
                     end if
                     ! SHOC
                     if (Model%do_shoc) then
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ncgl                ', Interstitial%ncgl                    )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%qrn                 ', Interstitial%qrn                     )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%qsnw                ', Interstitial%qsnw                    )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%qgl                 ', Interstitial%qgl                     )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ncpi                ', Interstitial%ncpi                    )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%ncpl                ', Interstitial%ncpl                    )
                     end if
                     ! Noah MP
                     if (Model%lsm == Model%lsm_noahmp) then
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%t2mmp               ', Interstitial%t2mmp                   )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%q2mp                ', Interstitial%q2mp                    )
                     end if
                     ! RRTMGP
                     if (Model%do_RRTMGP) then
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%aerosolslw          ', Interstitial%aerosolslw              )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%aerosolssw          ', Interstitial%aerosolssw              )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cld_frac            ', Interstitial%cld_frac                )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cld_lwp             ', Interstitial%cld_lwp                 )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cld_reliq           ', Interstitial%cld_reliq               )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cld_iwp             ', Interstitial%cld_iwp                 )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cld_reice           ', Interstitial%cld_reice               )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cld_swp             ', Interstitial%cld_swp                 )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cld_resnow          ', Interstitial%cld_resnow              )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cld_rwp             ', Interstitial%cld_rwp                 )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cld_rerain          ', Interstitial%cld_rerain              )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%precip_frac         ', Interstitial%precip_frac             )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fluxlwUP_allsky     ', Interstitial%fluxlwUP_allsky         )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fluxlwDOWN_allsky   ', Interstitial%fluxlwDOWN_allsky       )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fluxlwUP_clrsky     ', Interstitial%fluxlwUP_clrsky         )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fluxlwDOWN_clrsky   ', Interstitial%fluxlwDOWN_clrsky       )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fluxswUP_allsky     ', Interstitial%fluxswUP_allsky         )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fluxswDOWN_allsky   ', Interstitial%fluxswDOWN_allsky       )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fluxswUP_clrsky     ', Interstitial%fluxswUP_clrsky         )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%fluxswDOWN_clrsky   ', Interstitial%fluxswDOWN_clrsky       )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%relhum              ', Interstitial%relhum                  )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%q_lay               ', Interstitial%q_lay                   )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%qs_lay              ', Interstitial%qs_lay                  )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%deltaZ              ', Interstitial%deltaZ                  )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%p_lay               ', Interstitial%p_lay                   )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%p_lev               ', Interstitial%p_lev                   )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%t_lay               ', Interstitial%t_lay                   )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%t_lev               ', Interstitial%t_lev                   )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%tv_lay              ', Interstitial%tv_lay                  )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%cloud_overlap_param ', Interstitial%cloud_overlap_param     )
                         call print_var(mpirank, omprank, blkno, Grid%xlat_d, Grid%xlon_d, 'Interstitial%precip_overlap_param', Interstitial%precip_overlap_param    )
                     end if
                 end if
#ifdef _OPENMP
!$OMP BARRIER
#endif
             end do
#ifdef MPI
!             call MPI_BARRIER(mpicomm,ierr)
#endif
         end do

#ifdef _OPENMP
!$OMP BARRIER
#endif
#ifdef MPI
!         call MPI_BARRIER(mpicomm,ierr)
#endif

      end subroutine GFS_interstitialtoscreen_run

    end module GFS_interstitialtoscreen

    module GFS_abort

      private

      public GFS_abort_run

      contains

!> \section arg_table_GFS_abort_run Argument Table
!! \htmlinclude GFS_abort_run.html
!!
      subroutine GFS_abort_run (Model, blkno, errmsg, errflg)

         use machine,               only: kind_phys
         use GFS_typedefs,          only: GFS_control_type

         implicit none

         !--- interface variables
         type(GFS_control_type),   intent(in   ) :: Model
         integer,                  intent(in   ) :: blkno
         character(len=*),         intent(  out) :: errmsg
         integer,                  intent(  out) :: errflg

         ! Initialize CCPP error handling variables
         errmsg = ''
         errflg = 0

         if (Model%kdt==1 .and. blkno==size(Model%blksz)) then
             if (Model%me==Model%master) write(0,*) "GFS_abort_run: ABORTING MODEL"
             call sleep(10)
             stop
         end if

      end subroutine GFS_abort_run

    end module GFS_abort

    module GFS_checkland

      private

      public  GFS_checkland_run

      contains

!> \section arg_table_GFS_checkland_run Argument Table
!! \htmlinclude GFS_checkland_run.html
!!
      subroutine GFS_checkland_run (me, master, blkno, im, kdt, iter, flag_iter, flag_guess, &
              flag_init, flag_restart, frac_grid, isot, ivegsrc, stype,scolor, vtype, slope,        &
              dry, icy, wet, lake, ocean, oceanfrac, landfrac, lakefrac, slmsk, islmsk,      &
              zorl, zorlw, zorll, zorli, fice, errmsg, errflg )

         use machine, only: kind_phys

         implicit none

         ! Interface variables
         integer,          intent(in   ) :: me
         integer,          intent(in   ) :: master
         integer,          intent(in   ) :: blkno
         integer,          intent(in   ) :: im
         integer,          intent(in   ) :: kdt
         integer,          intent(in   ) :: iter
         logical,          intent(in   ) :: flag_iter(:)
         logical,          intent(in   ) :: flag_guess(:)
         logical,          intent(in   ) :: flag_init
         logical,          intent(in   ) :: flag_restart
         logical,          intent(in   ) :: frac_grid
         integer,          intent(in   ) :: isot
         integer,          intent(in   ) :: ivegsrc
         integer,          intent(in   ) :: stype(:)
         integer,          intent(in   ) :: scolor(:)

         integer,          intent(in   ) :: vtype(:)
         integer,          intent(in   ) :: slope(:)
         logical,          intent(in   ) :: dry(:)
         logical,          intent(in   ) :: icy(:)
         logical,          intent(in   ) :: wet(:)
         logical,          intent(in   ) :: lake(:)
         logical,          intent(in   ) :: ocean(:)
         real(kind_phys),  intent(in   ) :: oceanfrac(:)
         real(kind_phys),  intent(in   ) :: landfrac(:)
         real(kind_phys),  intent(in   ) :: lakefrac(:)
         real(kind_phys),  intent(in   ) :: slmsk(:)
         integer,          intent(in   ) :: islmsk(:)
         real(kind_phys),  intent(in   ) :: zorl(:)
         real(kind_phys),  intent(in   ) :: zorlw(:)
         real(kind_phys),  intent(in   ) :: zorll(:)
         real(kind_phys),  intent(in   ) :: zorli(:)
         real(kind_phys),  intent(in   ) :: fice(:)
         character(len=*), intent(  out) :: errmsg
         integer,          intent(  out) :: errflg

         ! Local variables
         integer :: i

         errflg = 0
         errmsg = ''

         write(0,'(a,i5)')   'YYY: me           :', me
         write(0,'(a,i5)')   'YYY: master       :', master
         write(0,'(a,i5)')   'YYY: blkno        :', blkno
         write(0,'(a,i5)')   'YYY: im           :', im
         write(0,'(a,i5)')   'YYY: kdt          :', kdt
         write(0,'(a,i5)')   'YYY: iter         :', iter
         write(0,'(a,1x,l)') 'YYY: flag_init    :', flag_init
         write(0,'(a,1x,l)') 'YYY: flag_restart :', flag_restart
         write(0,'(a,1x,l)') 'YYY: frac_grid    :', frac_grid
         write(0,'(a,i5)')   'YYY: isot         :', isot
         write(0,'(a,i5)')   'YYY: ivegsrc      :', ivegsrc

         do i=1,im
           !if (fice(i)>0.999) then
           !if (vegtype(i)==15) then
             write(0,'(a,2i5,1x,1x,l)') 'YYY: i, blk, flag_iter(i)  :', i, blkno, flag_iter(i)
             write(0,'(a,2i5,1x,1x,l)') 'YYY: i, blk, flag_guess(i) :', i, blkno, flag_guess(i)
             write(0,'(a,2i5,1x,e16.7)')'YYY: i, blk, stype(i)      :', i, blkno, stype(i)

             write(0,'(a,2i5,1x,e16.7)')'YYY: i, blk, scolor(i)      :', i, blkno, scolor(i)
             write(0,'(a,2i5,1x,e16.7)')'YYY: i, blk, vtype(i)      :', i, blkno, vtype(i)
             write(0,'(a,2i5,1x,e16.7)')'YYY: i, blk, slope(i)      :', i, blkno, slope(i)
             write(0,'(a,2i5,1x,1x,l)') 'YYY: i, blk, dry(i)        :', i, blkno, dry(i)
             write(0,'(a,2i5,1x,1x,l)') 'YYY: i, blk, icy(i)        :', i, blkno, icy(i)
             write(0,'(a,2i5,1x,1x,l)') 'YYY: i, blk, wet(i)        :', i, blkno, wet(i)
             write(0,'(a,2i5,1x,1x,l)') 'YYY: i, blk, lake(i)       :', i, blkno, lake(i)
             write(0,'(a,2i5,1x,1x,l)') 'YYY: i, blk, ocean(i)      :', i, blkno, ocean(i)
             write(0,'(a,2i5,1x,e16.7)')'YYY: i, blk, oceanfrac(i)  :', i, blkno, oceanfrac(i)
             write(0,'(a,2i5,1x,e16.7)')'YYY: i, blk, landfrac(i)   :', i, blkno, landfrac(i)
             write(0,'(a,2i5,1x,e16.7)')'YYY: i, blk, lakefrac(i)   :', i, blkno, lakefrac(i)
             write(0,'(a,2i5,1x,e16.7)')'YYY: i, blk, fice(i)       :', i, blkno, fice(i)
             write(0,'(a,2i5,1x,e16.7)')'YYY: i, blk, slmsk(i)      :', i, blkno, slmsk(i)
             write(0,'(a,2i5,1x,i5)')   'YYY: i, blk, islmsk(i)     :', i, blkno, islmsk(i)
             write(0,'(a,2i5,1x,e16.7)')'YYY: i, blk, zorl(i)       :', i, blkno, zorl(i)
             write(0,'(a,2i5,1x,e16.7)')'YYY: i, blk, zorlw(i)      :', i, blkno, zorlw(i)
             write(0,'(a,2i5,1x,e16.7)')'YYY: i, blk, zorli(i)      :', i, blkno, zorli(i)
             write(0,'(a,2i5,1x,e16.7)')'YYY: i, blk, zorll(i)      :', i, blkno, zorll(i)
           !end if
         end do

      end subroutine GFS_checkland_run
    end module GFS_checkland

    module GFS_checktracers

      private

      public GFS_checktracers_init, GFS_checktracers_timestep_init, GFS_checktracers_run

      contains

!> \section arg_table_GFS_checktracers_init Argument Table
!! \htmlinclude GFS_checktracers_init.html
!!
      subroutine GFS_checktracers_init (me, master, im, levs, ntracer, kdt, qgrs, gq0, errmsg, errflg)

         use machine, only: kind_phys

         implicit none

         ! Interface variables
         integer,          intent(in   ) :: me
         integer,          intent(in   ) :: master
         integer,          intent(in   ) :: im
         integer,          intent(in   ) :: levs
         integer,          intent(in   ) :: ntracer
         integer,          intent(in   ) :: kdt
         real(kind_phys),  intent(in   ) :: qgrs(:,:,:)
         real(kind_phys),  intent(in   ) :: gq0(:,:,:)
         character(len=*), intent(  out) :: errmsg
         integer,          intent(  out) :: errflg

         call GFS_checktracers_timestep_init (me, master, im, levs, ntracer, kdt, qgrs, gq0, errmsg, errflg)

      end subroutine GFS_checktracers_init

!> \section arg_table_GFS_checktracers_timestep_init Argument Table
!! \htmlinclude GFS_checktracers_timestep_init.html
!!
      subroutine GFS_checktracers_timestep_init (me, master, im, levs, ntracer, kdt, qgrs, gq0, errmsg, errflg)

         use machine, only: kind_phys

         implicit none

         ! Interface variables
         integer,          intent(in   ) :: me
         integer,          intent(in   ) :: master
         integer,          intent(in   ) :: im
         integer,          intent(in   ) :: levs
         integer,          intent(in   ) :: ntracer
         integer,          intent(in   ) :: kdt
         real(kind_phys),  intent(in   ) :: qgrs(:,:,:)
         real(kind_phys),  intent(in   ) :: gq0(:,:,:)
         character(len=*), intent(  out) :: errmsg
         integer,          intent(  out) :: errflg

         ! Local variables
         integer :: i, k, n

         errflg = 0
         errmsg = ''

         write(0,'(a,i5)')   'YYY: me           :', me
         write(0,'(a,i5)')   'YYY: master       :', master
         write(0,'(a,i5)')   'YYY: im           :', im
         write(0,'(a,i5)')   'YYY: levs         :', levs
         write(0,'(a,i5)')   'YYY: ntracer      :', ntracer
         write(0,'(a,i5)')   'YYY: kdt          :', kdt

         do n=1,ntracer
           do i=1,im
             do k=1,levs
               if (qgrs(i,k,n)<0 .or. gq0(i,k,n)<0) then
                 write(0,'(a,4i5,1x,2e16.7)') 'YYY: blk, n, i, k, qgrs, gq0 :', -999, n, i, k, qgrs(i,k,n), gq0(i,k,n)
               end if
             end do
           end do
         end do

      end subroutine GFS_checktracers_timestep_init

!> \section arg_table_GFS_checktracers_run Argument Table
!! \htmlinclude GFS_checktracers_run.html
!!
      subroutine GFS_checktracers_run (me, master, blkno, im, levs, ntracer, kdt, qgrs, gq0, errmsg, errflg)

         use machine, only: kind_phys

         implicit none

         ! Interface variables
         integer,          intent(in   ) :: me
         integer,          intent(in   ) :: master
         integer,          intent(in   ) :: blkno
         integer,          intent(in   ) :: im
         integer,          intent(in   ) :: levs
         integer,          intent(in   ) :: ntracer
         integer,          intent(in   ) :: kdt
         real(kind_phys),  intent(in   ) :: qgrs(:,:,:)
         real(kind_phys),  intent(in   ) :: gq0(:,:,:)
         character(len=*), intent(  out) :: errmsg
         integer,          intent(  out) :: errflg

         ! Local variables
         integer :: i, k, n

         errflg = 0
         errmsg = ''

         write(0,'(a,i5)')   'YYY: me           :', me
         write(0,'(a,i5)')   'YYY: master       :', master
         write(0,'(a,i5)')   'YYY: blkno        :', blkno
         write(0,'(a,i5)')   'YYY: im           :', im
         write(0,'(a,i5)')   'YYY: levs         :', levs
         write(0,'(a,i5)')   'YYY: ntracer      :', ntracer
         write(0,'(a,i5)')   'YYY: kdt          :', kdt

         do n=1,ntracer
           do i=1,im
             do k=1,levs
               if (qgrs(i,k,n)<0 .or. gq0(i,k,n)<0) then
                 write(0,'(a,4i5,1x,2e16.7)') 'YYY: blk, n, i, k, qgrs, gq0 :', blkno, n, i, k, qgrs(i,k,n), gq0(i,k,n)
               end if
             end do
           end do
         end do

      end subroutine GFS_checktracers_run

    end module GFS_checktracers
