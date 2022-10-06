	program main_modis_OT_parallel2

	use global

	implicit none

	include 'mpif.h'

    !******* initialize gaussian filter ****
    real, dimension(:,:), allocatable :: kernel

	!*********** initialze *****************
	character(len=200) :: mod03dir,mod02dir,merradir,wfname
	character(len=12) :: strday
	character(len=4) :: yrindex
    character(len=3) :: Julday,Julday_as,Julday_bs
	character(len=17) :: cld_date
    character(len=12) :: yyddhhmm,byyddhhmm,ayyddhhmm
	character(len=81) :: myday
	character(len=81), allocatable :: mod03_list(:)

	character(len=210) :: mod03fname,bmod03fname,amod03fname
	!character(len=80) :: mod02fname,bmod02fname,amod02fname
	character(len=87) :: mod02fname,bmod02fname,amod02fname
    character(len=200) :: merra2fname

 	integer (kind=2) :: mod03_nfname,mod02_nfname
    integer (kind=4) :: status
    integer :: values(8)
    integer*2 :: month,day
    character(len=2) :: month_1,day_1
    integer :: year_1,Julday_1,Julday_a,Julday_b
    integer :: modhh, modmm, hour_scp
    integer :: bmodhh,bmodmm,amodhh,amodmm
    character (len=2) :: bmodhh_1,bmodmm_1,amodhh_1,amodmm_1
    integer :: bmod_read_flag,amod_read_flag

	!=================== modis ================================
	integer :: modlat_scp, modlon_scp, rowi, colj 
    integer :: merralon_scp, merralat_scp

	real :: tplon,tplat

    integer :: lat_scp, lon_scp 
    real, parameter :: planck_c1=1.191042e8, planck_c2=1.4387752e4

    logical :: mod02flag, mod03flag, merraflag

    !========= some local variables ==================
    real :: tp_tropopause_t

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

	    mod03dir=&
        "/data/gdi/f/yulanh/MODIS/MYD03/"//&
        yrindex//"/"//Julday//"/"//cld_date//"*"

        call system("ls "//trim(mod03dir)//" > inputfile"//cld_date,status)
        call system("cat inputfile"//cld_date//" | wc -l > count"//cld_date)
        
        open(100,file="count"//cld_date)
        read(100,*) Nodes
        close(100)
        !print *,'number of file', Nodes
        allocate(mod03_list(Nodes))
        open(200,file="inputfile"//cld_date)
        read(200,fmt="(a81)",iostat=status) mod03_list
        close(200)

        ! allocate array 
        allocate(back_obsnumh(dimx,dimy,Nodes))
        allocate(back_bt180_numh(dimx,dimy,Nodes))
        allocate(back_bt200_numh(dimx,dimy,Nodes))
        allocate(back_bt210_numh(dimx,dimy,Nodes))
        allocate(back_bt220_numh(dimx,dimy,Nodes))
        allocate(back_bt230_numh(dimx,dimy,Nodes))
        allocate(back_bt240_numh(dimx,dimy,Nodes))
        allocate(back_cdtp_numh(dimx,dimy,Nodes))
        allocate(back_cdtp2_numh(dimx,dimy,Nodes))
        allocate(back_cdtp5_numh(dimx,dimy,Nodes))
        allocate(back_cdtp10_numh(dimx,dimy,Nodes))
    
        wfname='/data/keeling/a/yulanh/f/OT_project/Aqua/MODIS_obsnum_'//cld_date(8:17)//'.hdf'
        !print *,mod03_list(1)
        
	EndIf ! end root

	call MPI_SCATTER(mod03_list,81,MPI_CHARACTER,myday,&
                81,MPI_CHARACTER,my_root,MPI_COMM_WORLD,mpi_err)
        !print*,myday
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
       
	mod03dir=&
        "/data/gdi/f/yulanh/MODIS/MYD03/"//yrindex//"/"//Julday//"/"
	mod02dir=&
        "/data/gdi/f/yulanh/MODIS/MYD021KM/"//yrindex//"/"//Julday//"/"

    merradir="/data/keeling/a/yulanh/c/MERRA-2/"//yrindex//"/"

        !yyddhhmm=myday(41:52)
    yyddhhmm=myday(48:59)
    !print *,myid,yyddhhmm
        
    call system("ls "//trim(mod02dir)//"MYD021KM.A"//yyddhhmm//"*"//" &
                > mod02file"//yyddhhmm,status)
    open(400,file="mod02file"//yyddhhmm)
    read(400,fmt="(a87)",iostat=status) mod02fname
    close(400)

        !print*, mod02fname 
       
        !=========== read modis file ===========================================
        !======== search modis file in the same day as cldclass file ============
        !mod03fname=trim(mod03dir)//myday
    mod03fname=trim(myday)
 
    merra2fname=trim(merradir)//"MERRA2_400.inst1_2d_asm_Nx."//yrindex//month_1//day_1//".nc4"     

!print *, merra2fname
        INQUIRE (file=mod02fname,exist=mod02flag)
        INQUIRE (file=merra2fname,exist=merraflag)

        print *, myid,merra2fname,mod02flag,merraflag,mod02fname,mod03fname
       
        IF (mod02flag .and. merraflag) Then 

        call read_modis03(mod03fname)
        call read_modis02(mod02fname)
        !call read_merra2_hdf(merra2fname)
        call read_merra2(merra2fname)

       ! print *, trop_temp(1,1,hour_scp)
       ! print *,'dimension,ncol,nrow :',modNcol,modNrow, shape(rad_1000emis)  
        allocate(BT11(modNcol,modNrow))
        !allocate(BT11_sm(modNcol,modNrow))
        !allocate(BT67(modNcol,modNrow))

        BT11=planck_c2/(11.03*log(planck_c1/(rad_1000emis(:,:,11)*11.03**5)+1))
       ! BT67=planck_c2/(6.72*log(planck_c1/(rad_1000emis(:,:,7)*6.72**5)+1))

        read(myday(56:57),'(i2)') modhh
        read(myday(58:59),'(i2)') modmm

        hour_scp=floor(modhh+modmm/60.0)+1
        

        Do colj=1,modNcol
           Do rowi=1, modNrow

                tplat=modlat(colj,rowi)
                tplon=modlon(colj,rowi)

                merralon_scp=nint((tplon+180)/0.625)+1
                merralat_scp=nint((tplat+90)/0.5)+1
                if (merralon_scp < 1) merralon_scp=1
                if (merralon_scp > merra2_x) merralon_scp=merra2_x
                if (merralat_scp < 1) merralat_scp=1
                if (merralat_scp > merra2_y) merralat_scp=merra2_y
 
                tp_tropopause_t=trop_temp(merralon_scp,merralat_scp,hour_scp)
              
                lat_scp=nint(1+(tplat+90-latbin_res/2.0)/latbin_res)
                lon_scp=nint(1+(tplon+180-lonbin_res/2.0)/lonbin_res)
!       print *,tplat,tplon,lon_scp,lat_scp
                if (lat_scp < 1) lat_scp=1
                if (lon_scp < 1) lon_scp=1
                if (lat_scp > dimy) lat_scp=dimy
                if (lon_scp > dimx) lon_scp=dimx

                send_obsnumh(lon_scp,lat_scp) = send_obsnumh(lon_scp,lat_scp)+1
                
                IF (BT11(colj,rowi) <= 180) Then
                    send_bt180_numh(lon_scp,lat_scp)=send_bt180_numh(lon_scp,lat_scp)+1
                EndIf
                IF (BT11(colj,rowi) <= 200) Then
                    send_bt200_numh(lon_scp,lat_scp)=send_bt200_numh(lon_scp,lat_scp)+1
                EndIf
                IF (BT11(colj,rowi) <= 210) Then
                    send_bt210_numh(lon_scp,lat_scp)=send_bt210_numh(lon_scp,lat_scp)+1
                EndIf
                IF (BT11(colj,rowi) <= 220) Then
                    send_bt220_numh(lon_scp,lat_scp)=send_bt220_numh(lon_scp,lat_scp)+1
                EndIf
                IF (BT11(colj,rowi) <= 230) Then
                    send_bt230_numh(lon_scp,lat_scp)=send_bt230_numh(lon_scp,lat_scp)+1
                EndIf
                IF (BT11(colj,rowi) <= 240) Then
                    send_bt240_numh(lon_scp,lat_scp)=send_bt240_numh(lon_scp,lat_scp)+1
                EndIf

                IF (BT11(colj,rowi) < tp_tropopause_t) Then
                    send_cdtp_numh(lon_scp,lat_scp)=send_cdtp_numh(lon_scp,lat_scp)+1
                EndIf
                IF (BT11(colj,rowi) < (tp_tropopause_t -2)) Then
                    send_cdtp2_numh(lon_scp,lat_scp)=send_cdtp2_numh(lon_scp,lat_scp)+1
                EndIf
                IF (BT11(colj,rowi) < (tp_tropopause_t -5)) Then
                    send_cdtp5_numh(lon_scp,lat_scp)=send_cdtp5_numh(lon_scp,lat_scp)+1
                EndIf
                IF (BT11(colj,rowi) < (tp_tropopause_t -10)) Then
                    send_cdtp10_numh(lon_scp,lat_scp)=send_cdtp10_numh(lon_scp,lat_scp)+1
                EndIf
	        EndDo
	    EndDo 
        !print *,sum(OT_flag)

        deallocate(BT11)

        include 'deallocate_array.file'
    
        !print *, myid,modNrow,modNcol,sum(send_obsnumh) 
        call MPI_Gather(send_obsnumh,dimx*dimy,MPI_INTEGER,back_obsnumh,dimx*dimy,&
            MPI_INTEGER,my_root,MPI_COMM_WORLD,mpi_err)
        call MPI_Gather(send_bt180_numh,dimx*dimy,MPI_INTEGER,back_bt180_numh,dimx*dimy,&
            MPI_INTEGER,my_root,MPI_COMM_WORLD,mpi_err)
        call MPI_Gather(send_bt200_numh,dimx*dimy,MPI_INTEGER,back_bt200_numh,dimx*dimy,&
            MPI_INTEGER,my_root,MPI_COMM_WORLD,mpi_err)
        call MPI_Gather(send_bt210_numh,dimx*dimy,MPI_INTEGER,back_bt210_numh,dimx*dimy,&
            MPI_INTEGER,my_root,MPI_COMM_WORLD,mpi_err)
        call MPI_Gather(send_bt220_numh,dimx*dimy,MPI_INTEGER,back_bt220_numh,dimx*dimy,&
            MPI_INTEGER,my_root,MPI_COMM_WORLD,mpi_err)
        call MPI_Gather(send_bt230_numh,dimx*dimy,MPI_INTEGER,back_bt230_numh,dimx*dimy,&
            MPI_INTEGER,my_root,MPI_COMM_WORLD,mpi_err)
        call MPI_Gather(send_bt240_numh,dimx*dimy,MPI_INTEGER,back_bt240_numh,dimx*dimy,&
            MPI_INTEGER,my_root,MPI_COMM_WORLD,mpi_err)
        call MPI_Gather(send_cdtp_numh,dimx*dimy,MPI_INTEGER,back_cdtp_numh,dimx*dimy,&
            MPI_INTEGER,my_root,MPI_COMM_WORLD,mpi_err)
        call MPI_Gather(send_cdtp2_numh,dimx*dimy,MPI_INTEGER,back_cdtp2_numh,dimx*dimy,&
            MPI_INTEGER,my_root,MPI_COMM_WORLD,mpi_err)
        call MPI_Gather(send_cdtp5_numh,dimx*dimy,MPI_INTEGER,back_cdtp5_numh,dimx*dimy,&
            MPI_INTEGER,my_root,MPI_COMM_WORLD,mpi_err)
        call MPI_Gather(send_cdtp10_numh,dimx*dimy,MPI_INTEGER,back_cdtp10_numh,dimx*dimy,&
            MPI_INTEGER,my_root,MPI_COMM_WORLD,mpi_err)


        EndIf ! Endif merra2flag =1 or mod03flag=1

        IF (myid == my_root) Then
           deallocate(mod03_list)
           !print *,'all num',sum(back_obsnumh)
           total_obsnumh=sum(back_obsnumh,3) 
           total_bt180_numh=sum(back_bt180_numh,3) 
           total_bt200_numh=sum(back_bt200_numh,3) 
           total_bt210_numh=sum(back_bt210_numh,3) 
           total_bt220_numh=sum(back_bt220_numh,3) 
           total_bt230_numh=sum(back_bt230_numh,3) 
           total_bt240_numh=sum(back_bt240_numh,3) 
           total_cdtp_numh=sum(back_cdtp_numh,3) 
           total_cdtp2_numh=sum(back_cdtp2_numh,3) 
           total_cdtp5_numh=sum(back_cdtp5_numh,3) 
           total_cdtp10_numh=sum(back_cdtp10_numh,3) 
           !print *,'cld_date',cld_date

           call write_result(wfname)
            
            deallocate(back_obsnumh)
            deallocate(back_bt180_numh)
            deallocate(back_bt200_numh)
            deallocate(back_bt210_numh)
            deallocate(back_bt220_numh)
            deallocate(back_bt230_numh)
            deallocate(back_bt240_numh)
            deallocate(back_cdtp_numh)
            deallocate(back_cdtp2_numh)
            deallocate(back_cdtp5_numh)
            deallocate(back_cdtp10_numh)

            call system("rm -rf "//"inputfile"//cld_date)
            call system("rm -rf "//"count"//cld_date)
            call system("rm -rf "//"mod02file"//yyddhhmm)
            call system("rm -rf "//"mod03file"//yyddhhmm)

           call date_and_time(values=values)
           print *,'finish ',values
        EndIf

        call MPI_FINALIZE(mpi_err)

        end
