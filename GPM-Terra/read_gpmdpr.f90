
        subroutine read_gpmdpr(gpm_fname,yyddhhmm)

        use hdf5
        use global,only : data_ze,gpm_lon,gpm_lat,gpm_hour,&
                gpm_minute,gpm_sec,gpm_time_utc,gpmNray,&
                precisurf

        implicit none

       ! character (len=64), PARAMETER:: gpm_fname="2A.GPM.DPR.&
       ! V8-20180723.20180102-S001822-E015057.021848.V06A.HDF5"
        character (len=12) :: yyddhhmm
        character (len=108) :: gpm_fname
        character (len=6) :: swath_num
        character (len=30) :: gpm_hour_fname,gpm_minute_fname,gpm_sec_fname
        character (len=24), PARAMETER :: dsetname=&
        '/NS/PRE/zFactorMeasured'

        integer(HID_T) :: file_id,dset_id,dataspace
        integer :: error
        integer :: rankr
        
!        real (kind=4), dimension(176,49,7936) ::  data_ze
!        real (kind=4), dimension(49,7936) :: gpm_lon, gpm_lat

        integer(HSIZE_T), dimension(1:3) :: dimr,maxdimsr!=(/176,49,7936/)

        print *,gpm_fname

        swath_num = gpm_fname(93:98)
        gpm_hour_fname=swath_num//yyddhhmm//"_hour.bin"
        gpm_minute_fname=swath_num//yyddhhmm//"_minute.bin"
        gpm_sec_fname=swath_num//yyddhhmm//"_sec.bin"

        !initialzie fortran interface
        call h5open_f(error)
        
        call h5fopen_f(gpm_fname,h5f_acc_rdonly_f,file_id,error)

        call h5dopen_f(file_id,dsetname,dset_id,error)

        CALL h5dget_space_f(dset_id, dataspace, error) ! get dims and dim size
        CALL h5sget_simple_extent_ndims_f(dataspace, rankr, error)
        CALL h5sget_simple_extent_dims_f(dataspace,dimr,maxdimsr,error)

        allocate(data_ze(dimr(1),dimr(2),dimr(3)))
        gpmNray=dimr(3)
        call h5dread_f(dset_id,h5t_native_real,data_ze,dimr,&
                error)
        call h5dclose_f(dset_id,error)

        allocate(gpm_lon(dimr(2),dimr(3)))
        allocate(gpm_lat(dimr(2),dimr(3)))
        allocate(precisurf(dimr(2),dimr(3)))
        allocate(gpm_hour(dimr(3)))
        allocate(gpm_sec(dimr(3)))
        allocate(gpm_minute(dimr(3)))

        call h5dopen_f(file_id,'/NS/Longitude',dset_id,error)
        call h5dread_f(dset_id,h5t_native_real,gpm_lon,dimr(2:3),&
                error)
        call h5dclose_f(dset_id,error)


        call h5dopen_f(file_id,'/NS/Latitude',dset_id,error)
        call h5dread_f(dset_id,h5t_native_real,gpm_lat,dimr(2:3),&
                error)
        call h5dclose_f(dset_id,error)

        call h5dopen_f(file_id,'/NS/SLV/precipRateNearSurface',dset_id,error)
        call h5dread_f(dset_id,h5t_native_real,precisurf,dimr(2:3),&
                error)
        call h5dclose_f(dset_id,error)

        !call h5dopen_f(file_id,'/NS/ScanTime/Minute',dset_id,error)
        !call h5dread_f(dset_id,h5t_native_integer,gpm_minute,dimr(3),&
        !        error)
        !call h5dclose_f(dset_id,error)

        !call h5dopen_f(file_id,'/ScanTime/Second',dset_id,error)
        !call h5dread_f(dset_id,h5t_native_integer,gpm_sec,dimr(3),&
        !        error)
        !call h5dclose_f(dset_id,error)

        call h5fclose_f(file_id,error)
        call h5close_f(error)


        !===== using h5 dump hear

        call system('h5dump -d "/NS/ScanTime/Hour" -b  -o '//&
                gpm_hour_fname//' '// gpm_fname)
        call system('h5dump -d "/NS/ScanTime/Minute" -b  -o '//&
                gpm_minute_fname//' '// gpm_fname)
        call system('h5dump -d "/NS/ScanTime/Second" -b  -o '//&
                gpm_sec_fname//' '// gpm_fname)

        open(1,file=gpm_hour_fname,form="unformatted",&
                access="stream",status="old")
        read(1) gpm_hour
        close(1)        

        open(2,file=gpm_minute_fname,form="unformatted",&
                access="stream",status="old")
        read(2) gpm_minute
        close(2)        

        open(3,file=gpm_sec_fname,form="unformatted",&
                access="stream",status="old")
        read(3) gpm_sec
        close(3)        

        allocate(gpm_time_utc(dimr(3)))
        gpm_time_utc=gpm_hour+gpm_minute/60.0+gpm_sec/3600.0

        !print *, data_ze(159,17,26),precisurf(30,199)
        !print *, gpm_lat(1,1),gpm_lon(1,1),gpm_lat(1,7936),gpm_lon(1,7936)
        !print *, gpm_hour(1),gpm_minute(1),gpm_sec(1)
        call system("rm -rf "//gpm_hour_fname)
        call system("rm -rf "//gpm_minute_fname)
        call system("rm -rf "//gpm_sec_fname)
        
        end
