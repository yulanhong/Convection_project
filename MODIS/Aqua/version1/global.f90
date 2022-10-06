	module global
	implicit none
	!=== MODIS ========================
	!=== mod03=====================
	integer :: modNrow, modNcol, modNrow_a,modNcol_a,modNrow_b,modNcol_b
	real (kind=4), allocatable :: modlat(:,:), modlon(:,:),&
                modlat_a(:,:),modlat_b(:,:),&
                modlon_a(:,:),modlon_b(:,:)
!	real (kind=4), allocatable :: solarzenith(:,:),sensorzenith(:,:)
        real (kind=4), allocatable ::rad_1000emis(:,:,:),rad_250(:,:,:),&
                rad_1000emis_a(:,:,:),rad_1000emis_b(:,:,:)
!	integer (kind=1), allocatable :: landsea(:,:)

        !==== merra 2 =====
         integer, parameter :: merra2_x=576,merra2_y=361,&
                        merra2_z=24
         real, parameter :: cirrus_threshold=260.0,storm_threshold=260.0
         real, parameter :: cirrus_btd_threshold=-3,otbtd_threshold=0.0
         real, parameter :: const=-3.2397,coef_1=0.2075,coef_2=0.3516,&
                        coef_3=0.4996
! for average BT6.7-BT11
         real, parameter :: pi=3.1415926

         integer, parameter :: win_size=20, Nray=8

         real :: trop_temp(merra2_x,merra2_y,merra2_z)

        !===== output =========================

        !integer (kind=1),allocatable :: OT_flag(:,:)!,cirrus_radii_num(:,:)
        real (kind=4),allocatable :: &!mean_cirrus_bt(:,:),&
        !        mean_OT_BT(:,:),&!OT_BTD(:,:),
        !        OT_BTD_trop(:,:),OT_area(:,:),&
        !        storm_area(:,:),mean_storm_bt(:,:),&
                BT11(:,:),BT67(:,:)!,OT_proba(:,:)

	end
