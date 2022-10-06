        program main_modis_OT_parallel1

	use global

	implicit none

	include 'mpif.h'

	!*********** initialze *****************
	character(len=200) :: mod03dir,merradir,wfname,OT_dir
        character(len=200) :: gpmdir
	character(len=12) :: strday
	character(len=4) :: yrindex
        character(len=3) :: Julday,Julday_as,Julday_bs
	character(len=17) :: cld_date
        character(len=12) :: yyddhhmm,byyddhhmm,ayyddhhmm
	character(len=112) :: myday
	character(len=112), allocatable :: mod03_list(:)

	character(len=200) :: mod03fname,bmod03fname,amod03fname

        character(len=108) :: gpm_fname,gpm_fname_previous,gpm_fname_after,OT_fname
        integer :: gpm_nfname
        character(len=108), allocatable :: gpm_list(:)
        real ::gpm_start_time, gpm_start_time_p
        integer :: gpm_start_hour, gpm_start_mm, gpm_start_sec
        integer :: gpm_year_p,gpm_sn
        character (len=4) :: gpm_year_p1
        character (len=5) :: gpm_sn_p
        character(len=200) :: merra2fname
        integer :: fi

        integer (kind=4) :: status,rd_status
        integer :: values(8)
        integer*2 :: month,day
        character(len=2) :: month_1,day_1
        integer :: year_1,Julday_1,Julday_a,Julday_b
        real :: tphour,search_hour_begin,search_hour_end
        integer :: modhh, modmm, hour_scp
        integer :: bmodhh,bmodmm,amodhh,amodmm
        character (len=2) :: bmodhh_1,bmodmm_1,amodhh_1,amodmm_1

	!=================== modis ================================
	integer :: modlat_scp, modlon_scp, rowi, colj 
        integer :: merralon_scp, merralat_scp
        real :: tpmodlon,tpmodlat,tp_proba

        !================= merra2 =======================
        integer :: mod2merra2_lonscp,mod2merra2_latscp

        !=================== colocated modis-gpm ==================
        integer :: time_min_loc(1),time_max_loc(1),&
                colocate_scp(2)
        real :: gpm_min_lat,gpm_max_lat,gpm_min_lon,gpm_max_lon
        real :: modis_min_lat,modis_max_lat,modis_min_lon,modis_max_lon
	real :: tplon,tplat,cogpm_lon,cogpm_lat,cogpm_preci
        real :: cogpm_ze(176),cogpm_hgt(176),cogpm_hgt_index(176)
        real :: cotime_diff
        real :: maxhgt20, maxhgt15
        integer :: ri

	integer :: colupres,collowres,rowupres,rowlowres
        real, parameter :: planck_c1=1.191042e8, planck_c2=1.4387752e4
       ! real, parameter :: BT_threshold=215,cirrus_threshold=225
       ! integer, parameter :: win_size=7,box_size=8, Nray=16
 
        real :: tpbt11_box(win_size*2+1,win_size*2+1)
               ! tpbt11_ray(win_size+1,Nray),tpbt11_ray_1(win_size+1)

        real :: cirrus_bt(Nray)

        logical :: mod02flag, mod03flag, merraflag, OTextflag
        integer :: OText_size

        !=========== read OT data ====
        real :: tpdata(31)


 	!****** initialize for mpi *************************
	integer :: mpi_err, numnodes, myid, Nodes
	integer, parameter :: my_root=0

	call MPI_INIT( mpi_err )
	call MPI_COMM_SIZE( MPI_COMM_WORLD, numnodes, mpi_err )
   	call MPI_Comm_rank(MPI_COMM_WORLD, myid, mpi_err)
	!************************************************

	IF (myid == my_root) Then

        call date_and_time(values=values)
        print *, values

	read(*,*) cld_date
	print *,cld_date

	yrindex=cld_date(8:11)
        Julday =cld_date(12:14)

	mod03dir="/data/keeling/a/yulanh/satellite/TerraDataArchive/MODIS/MOD03/"//&
        yrindex//"/"//Julday//"/"//cld_date//"*"

	call system("ls "//trim(mod03dir)//" > inputfile"//cld_date,status)
        call system("cat inputfile"//cld_date//" | wc -l > count"//cld_date)

 
        open(100,file="count"//cld_date)
        read(100,*) Nodes
        close(100)
    
        allocate(mod03_list(Nodes))
        open(200,file="inputfile"//cld_date)
        read(200,fmt="(a112)",iostat=status) mod03_list
        close(200)
       
	EndIf ! end root

	call MPI_SCATTER(mod03_list,112,MPI_CHARACTER,myday,&
                112,MPI_CHARACTER,my_root,MPI_COMM_WORLD,mpi_err)

	call MPI_BCAST(cld_date,17,MPI_CHARACTER,&
                my_root,MPI_COMM_WORLD,mpi_err)

	yrindex=cld_date(8:11)
        Julday =cld_date(12:14)

        read(yrindex,'(i4)') year_1
        read(Julday, '(i3)') Julday_1

        call Julday2month(year_1,Julday_1,month,day)

        write(month_1,'(i0)') month
        write(day_1  ,'(i0)') day

        if (len(trim(month_1)) == 1) month_1='0'//trim(month_1)
        if (len(trim(day_1)) == 1) day_1='0'//trim(day_1)
       
        OT_dir  ="/data/keeling/a/yulanh/c/OT_output/"//yrindex//"/"
        mod03dir="/data/keeling/a/yulanh/satellite/TerraDataArchive/MOD03/"//yrindex//"/"//Julday//"/"
        merradir="/data/keeling/a/yulanh/c/MERRA-2/"//yrindex//"/"
        gpmdir  ="/data/accp/a/snesbitt/gpmdata/"//"radar"//"/"//yrindex//"/"//month_1//"/"
        
        yyddhhmm=myday(79:90)

        !=========== read modis file ===========================================
        !======== search modis file in the same day as cldclass file ============
        !mod03fname=trim(mod03dir)//myday
        mod03fname=trim(myday)
        OT_fname=trim(OT_dir)//"MODIS_OTinfo_"//yyddhhmm//".txt"
        merra2fname=trim(merradir)//"MERRA2_400.inst1_2d_asm_Nx."//yrindex//month_1//day_1//".hdf"     
        !print *, merra2fname
        read(myday(87:88),'(i2)') modhh
        read(myday(89:90),'(i2)') modmm
        !print *, myday,modhh,modmm
        tphour = modhh+modmm/60.0
        hour_scp=floor(modhh+modmm/60.0)+1
        !stop

        call system("ls "//trim(gpmdir)//"2A.GPM.DPR.V8-20180723."//yrindex//month_1//day_1//"*"//&
              "  > gpmdprfile"//yrindex//month_1//day_1//yyddhhmm,status)
        call system("cat gpmdprfile"//yrindex//month_1//day_1//yyddhhmm//&
                " | wc -l > countgpm"//yrindex//month_1//day_1//yyddhhmm)
        open(100,file="countgpm"//yrindex//month_1//day_1//yyddhhmm)
        read(100,*) gpm_nfname 
        close(100)
        allocate(gpm_list(gpm_nfname))

        open(200,file="gpmdprfile"//yrindex//month_1//day_1//yyddhhmm)
        read(200,fmt="(a108)",iostat=status) gpm_list
        close(200)

        call gpm_height_lun()

        Do fi=1,gpm_nfname-1

             gpm_fname=gpm_list(fi)
             read(gpm_fname(78:79),'(i2)') gpm_start_hour
             read(gpm_fname(80:81),'(i2)') gpm_start_mm
             read(gpm_fname(82:83),'(i2)') gpm_start_sec
             gpm_start_time_p=gpm_start_hour+gpm_start_mm/60.0+gpm_start_sec/3600.0

             read(gpm_fname(93:98),'(i6)') gpm_sn ! swath number

             if (fi == 1 .and. tphour < gpm_start_time_p-0.04) then !  2.5 min as a threshold to go to be
                write(gpm_sn_p,'(i0)') gpm_sn-1
                if (day_1 == '01' .and. modhh == 0) then ! need to go back to previous year
                   gpm_year_p=year_1-1
                   write(gpm_year_p1,'(i0)') gpm_year_p
                
                   call system("ls /data/accp/a/snesbitt/gpmdata/radar/"//gpm_year_p1//"/"//"12"//"/"&
                        //"*"//gpm_sn_p//".V06A.HDF5"//&
                        " > gpmdprfile_previous"//gpm_year_p1//"1231",status)
                   open(300,file="gpmdprfile_previous"//gpm_year_p1//"1231")
                   read(300,fmt="(a108)",iostat=status) gpm_fname 
                   close(300)
                   exit
                endif

                if (day_1 .ne. '01' ) then ! in the same year
                   call system("ls /data/accp/a/snesbitt/gpmdata/radar/"//yrindex//"/"//month_1//"/"&
                        //"*"//gpm_sn_p//".V06A.HDF5"//&
                        " > gpmdprfile_previous"//gpm_year_p1//"1231",status)
                   open(300,file="gpmdprfile_previous"//gpm_year_p1//"1231")
                   read(300,fmt="(a108)",iostat=status) gpm_fname
                   close(300)
                   exit 
                endIf 

             endif ! go to previous day

             gpm_fname=gpm_list(fi+1)
             read(gpm_fname(78:79),'(i2)') gpm_start_hour
             read(gpm_fname(80:81),'(i2)') gpm_start_mm
             read(gpm_fname(82:83),'(i2)') gpm_start_sec
             gpm_start_time=gpm_start_hour+gpm_start_mm/60.0+gpm_start_sec/3600.0

               
             if (tphour >= gpm_start_time_p-0.04 .and. tphour <= gpm_start_time) then
                gpm_fname=gpm_list(fi)
                exit
             endif
            !!! dealing with the last file !!!
            if (tphour > gpm_start_time) gpm_fname=gpm_list(fi+1) 
        EndDo

        print *,'get colocated file',gpm_fname
        read(gpm_fname(93:98),'(i6)') gpm_sn

        INQUIRE (file=merra2fname,exist=merraflag)
        INQUIRE (file=OT_fname, exist=OTextflag,size=OText_size)

        IF (OText_size == 0) Then
           call system("rm -rf "//OT_fname)
        EndIf

        IF (merraflag .and. OTextflag .and. OText_size > 0) Then 

        call read_modis03(mod03fname)
        call read_merra2_hdf(merra2fname)
!        call read_OToutput(OT_fname)
        call read_gpmdpr(gpm_fname,yyddhhmm)
        call merra2_pres2hgt()
        !==== read OT file =====
        OPEN(1000,file=OT_fname,status='old',action='read')
        OPEN(2000,file='colocated_file_1505_parallax_'//yyddhhmm//'.txt') 
        rd_status=0
        Do 
                
             read(1000,*,iostat=rd_status) tpdata
             IF (rd_status < 0) then
                 exit
             EndIF
            ! print *, tpdata  
             
             modlon_scp=tpdata(1)
             modlat_scp=tpdata(2)
             tpmodlon=tpdata(3)
             tpmodlat=tpdata(4)
             tp_proba=tpdata(5)
       
        !===first to check whether MODIS and GPM Swath overlap or not
        !==== search in five minutes, change time time diff by setting tphour
        !and tphour_end
        search_hour_end   = modhh+modmm/60.0+5.0/60.0
        search_hour_begin = modhh+modmm/60-5.0/60.0

        if (search_hour_begin < 0) search_hour_begin=0
        !print *,search_hour_end,search_hour_begin,minval(gpm_time_utc),maxval(gpm_time_utc)
        time_min_loc = minloc(abs(search_hour_begin-gpm_time_utc))
        time_max_loc = minloc(abs(search_hour_end-gpm_time_utc))

        gpm_min_lat=minval(gpm_lat(:,time_min_loc(1):time_max_loc(1)))
        gpm_max_lat=maxval(gpm_lat(:,time_min_loc(1):time_max_loc(1)))
        gpm_min_lon=minval(gpm_lon(:,time_min_loc(1):time_max_loc(1)))
        gpm_max_lon=maxval(gpm_lon(:,time_min_loc(1):time_max_loc(1)))
        !print *,time_min_loc,time_max_loc
        !print *,gpm_min_lat,gpm_max_lat,gpm_min_lon,gpm_max_lon,tpmodlon,tpmodlat
 
        ! ==== the modis pixel is in the 10-minute gpm box  ====================
        IF (gpm_max_lat > tpmodlat .and. gpm_min_lat < tpmodlat &
           .and. gpm_max_lon > tpmodlon .and. gpm_min_lon < tpmodlon) Then 

               colocate_scp=minloc(abs(tpmodlon-gpm_lon(:,time_min_loc(1):time_max_loc(1))) +&
                 abs(tpmodlat-gpm_lat(:,time_min_loc(1):time_max_loc(1))))
              
               cogpm_lon=gpm_lon(colocate_scp(1),time_min_loc(1)+colocate_scp(2)-1)
               cogpm_lat=gpm_lat(colocate_scp(1),time_min_loc(1)+colocate_scp(2)-1)
               cogpm_preci=precisurf(colocate_scp(1),time_min_loc(1)+colocate_scp(2)-1)

               cogpm_ze=data_ze(:,colocate_scp(1),time_min_loc(1)+colocate_scp(2)-1)
               cogpm_hgt=range_height(:,colocate_scp(1))
    
               maxhgt20=0.0
               maxhgt15=0.0 ! by seting the threshod of 0.05, distance < 5, 
               !print *,tpmodlon,tpmodlat,cogpm_lon,cogpm_lat,colocate_scp,&
               ! gpm_time_utc(time_min_loc(1)+colocate_scp(2)-1),&
               ! search_hour_end,search_hour_begin,gpm_sn
               ! stop

               IF (sqrt((tpmodlon-cogpm_lon)**2+(tpmodlat-cogpm_lat)**2) < 0.1) Then  !0.1~11 km, 0.19 ~ 20 km

                 !search for maxhgt20 maxhgt15
               
                mod2merra2_lonscp=(tpmodlon+180)/0.625
                mod2merra2_latscp=(tpmodlat+90)/0.5

                Do ri=1,176
                    
                   IF (cogpm_ze(ri) >= 20.0 .and. maxhgt20 .eq. 0.0) maxhgt20=cogpm_hgt(ri)     
                   IF (cogpm_ze(ri) >= 15.0 .and. maxhgt15 .eq. 0.0) maxhgt15=cogpm_hgt(ri)     
                      
                EndDo   
!               print *,trop_hgt(mod2merra2_lonscp,mod2merra2_latscp,hour_scp),maxhgt20,maxhgt15,colocate_scp,time_min_loc,gpm_sn
        !       cotime_diff=gpm_time_utc(time_min_loc(1)+colocate_scp(2)-1)-(tphour+0.08333*colj/modNcol)

!                print*,time_min_loc,colocate_scp,gpm_time_utc(time_min_loc(1)+colocate_scp(2)-1)
 !               print *,tphour+0.08333*colj/modNcol,modNcol
 !               print *,cotime_diff 
 !               stop
                write(2000,*) colocate_scp(1),colocate_scp(2)+time_min_loc(1)-1,cogpm_lon,cogpm_lat,&
                trop_hgt(mod2merra2_lonscp,mod2merra2_latscp,hour_scp),maxhgt20,maxhgt15,&
                gpm_time_utc(colocate_scp(2)+time_min_loc(1)-1),gpm_fname(93:98),surf_height(modlon_scp,modlat_scp),&
                tpdata
                

                !write(*,*) tplon, tplat, cogpm_lon,cogpm_lat,trop_hgt(mod2merra2_lonscp,mod2merra2_latscp,hour_scp),&
                !maxhgt20,maxhgt15,gpm_sn,OT_BT(colj,rowi),OT_BTD(colj,rowi),OT_BTD_trop(colj,rowi),mean_cirrus_bt(colj,rowi)

               !print *, sensorazimuth(colj,rowi),sensorzenith(colj,rowi)
                EndIf ! ENDIf there is pixels less than 5 km              
              
             EnDIF ! end if modis pixel is in gpm 10-min box
	!   EndDo
	!EndDo 
        EndDo 
        close(1000) ! end reading
        close(2000)
        !call write_OT(wfname)

        include 'deallocate_array.file'
        include 'deallocate_gpm.file'
 !       call system("rm -rf countgpm"//yrindex//month_1//day_1//yyddhhmm)
      !  deallocate(r_parallax)
      !  deallocate(modlon_parallax)
      !  deallocate(modlat_parallax)
      !  call system("rm -rf gpmdprfile"//yrindex//month_1//day_1//yyddhhmm)

        EndIf ! endif file

        IF (myid == my_root) Then
           deallocate(mod03_list)
           call system("rm -rf inputfile"//cld_date)
           call system("rm -rf mod03file*")
           call system("rm -rf count"//cld_date)
           call system("rm -rf countgpm*")
           call system("rm -rf gpmdprfile*")
           call system("rm -rf *.bin")      
           call date_and_time(values=values)
         
           print *,'finish ',values
        EndIf


    call MPI_FINALIZE(mpi_err)

 end
