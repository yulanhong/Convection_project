	module global
	implicit none
	!=== MODIS ========================
	!=== mod03=====================
	integer :: modNrow, modNcol
	!real (kind=4), allocatable :: modlat(:,:), modlon(:,:)
	!real (kind=4), allocatable :: sensorazimuth(:,:),sensorzenith(:,:)
!	integer (kind=1), allocatable :: landsea(:,:)
        integer (kind=2), allocatable :: surf_height(:,:)

        !==== merra 2 =====
         integer, parameter :: merra2_x=576,merra2_y=361,&
                        merra2_z=24

         integer, parameter :: win_size=20, Nray=16

         real (kind=4) :: trop_temp(merra2_x,merra2_y,merra2_z),&
                trop_pres(merra2_x,merra2_y,merra2_z),&
                trop_hgt(merra2_x,merra2_y,merra2_z),&
                surf_pres(merra2_x,merra2_y,merra2_z),&
                surf_temp(merra2_x,merra2_y,merra2_z)


        !=== gpm =========
        integer :: gpmNray
        real (kind=4), allocatable ::  data_ze(:,:,:),precisurf(:,:)
        real (kind=4), allocatable :: gpm_lon(:,:), gpm_lat(:,:)
        integer (kind=1), allocatable :: gpm_hour(:),gpm_minute(:),gpm_sec(:)
        real,allocatable :: gpm_time_utc(:)
        real (kind=4), dimension (176,49) :: range_height

        !===== output =========================

        !integer (kind=1),allocatable :: OT_flag(:,:)
        !real  (kind=4) ,allocatable :: mean_cirrus_bt(:,:),OT_BT(:,:),&
         !       OT_BTD(:,:),OT_BTD_trop(:,:)

	end
