	module global
	implicit none
	!=== MODIS ========================
	!=== mod03=====================
	integer :: modNrow, modNcol
	real (kind=4), allocatable :: modlat(:,:), modlon(:,:)
        real (kind=4), allocatable ::rad_1000emis(:,:,:),rad_250(:,:,:)
!	integer (kind=1), allocatable :: landsea(:,:)

        !==== merra 2 =====
         integer, parameter :: merra2_x=576,merra2_y=361,&
            latbin_res=1,lonbin_res=1,&
            merra2_z=24,dimx=360/lonbin_res,dimy=180/latbin_res
! for average BT6.7-BT11
         real, parameter :: pi=3.1415926

         real :: trop_temp(merra2_x,merra2_y,merra2_z)

        !===== output =========================

        !integer (kind=1),allocatable :: OT_flag(:,:)!,cirrus_radii_num(:,:)
        real (kind=4),allocatable :: BT11(:,:),BT67(:,:)!,OT_proba(:,:)

        integer (kind=4), allocatable :: back_obsnumh(:,:,:),&
            back_bt180_numh(:,:,:),back_bt200_numh(:,:,:),&
            back_bt210_numh(:,:,:),back_bt220_numh(:,:,:),&
            back_bt230_numh(:,:,:),back_bt240_numh(:,:,:),&
            back_cdtp_numh(:,:,:),back_cdtp2_numh(:,:,:),&
            back_cdtp5_numh(:,:,:),back_cdtp10_numh(:,:,:)
        
        integer (kind=4) :: send_obsnumh(dimx,dimy),&
            send_bt180_numh(dimx,dimy),send_bt200_numh(dimx,dimy),&
            send_bt210_numh(dimx,dimy),send_bt220_numh(dimx,dimy),&
            send_bt230_numh(dimx,dimy),send_bt240_numh(dimx,dimy),&
            send_cdtp_numh(dimx,dimy),send_cdtp2_numh(dimx,dimy),&
            send_cdtp5_numh(dimx,dimy),send_cdtp10_numh(dimx,dimy)
             
        integer (kind=4) :: total_obsnumh(dimx,dimy),&
            total_bt180_numh(dimx,dimy),total_bt200_numh(dimx,dimy),&
            total_bt210_numh(dimx,dimy),total_bt220_numh(dimx,dimy),&
            total_bt230_numh(dimx,dimy),total_bt240_numh(dimx,dimy),&
            total_cdtp_numh(dimx,dimy),total_cdtp2_numh(dimx,dimy),&
            total_cdtp5_numh(dimx,dimy),total_cdtp10_numh(dimx,dimy)
	end
