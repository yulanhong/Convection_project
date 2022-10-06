	program main_modis_OT_parallel

	use global

	implicit none

	include 'mpif.h'

	!*********** initialze *****************
	character(len=200) :: mod03dir,mod02dir,merradir,wfname
	character(len=12) :: strday
	character(len=4) :: yrindex
        character(len=3) :: Julday,Julday_as,Julday_bs
	character(len=17) :: cld_date
        character(len=12) :: yyddhhmm,byyddhhmm,ayyddhhmm
	character(len=106) :: myday
	character(len=106), allocatable :: mod03_list(:)

	character(len=200) :: mod03fname,bmod03fname,amod03fname
	character(len=200) :: mod02fname,bmod02fname,amod02fname
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

	!=================== modis ================================
	integer :: modlat_scp, modlon_scp, rowi, colj 
        integer :: merralon_scp, merralat_scp

	real :: tplon,tplat
        real, allocatable :: BT11(:,:),BT67(:,:),&
                BT11_a(:,:),BT11_b(:,:)
	integer :: colupres,collowres,rowupres,rowlowres
        real, parameter :: planck_c1=1.191042e8, planck_c2=1.4387752e4
       ! real, parameter :: BT_threshold=215,cirrus_threshold=225
       ! integer, parameter :: win_size=7,box_size=8, Nray=16
        integer :: ri

        real :: tpbt11_box(win_size*2+1,win_size*2+1)
               ! tpbt11_ray(win_size+1,Nray),tpbt11_ray_1(win_size+1)

        real :: cirrus_bt(Nray)

        logical :: mod02flag, mod03flag, merraflag

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

	mod03dir="/data/keeling/a/yulanh/satellite/TerraDataArchive/MOD03/"//&
        yrindex//"/"//Julday//"/"//cld_date//"*"

	call system("ls "//trim(mod03dir)//" > inputfile"//cld_date,status)
        call system("cat inputfile"//cld_date//" | wc -l > count"//cld_date)
 
        open(100,file="count"//cld_date)
        read(100,*) Nodes
        close(100)
        print *,'number of file', Nodes
        allocate(mod03_list(Nodes))
        open(200,file="inputfile"//cld_date)
        read(200,fmt="(a106)",iostat=status) mod03_list
        close(200)
       
	EndIf ! end root

	call MPI_SCATTER(mod03_list,106,MPI_CHARACTER,myday,&
                106,MPI_CHARACTER,my_root,MPI_COMM_WORLD,mpi_err)

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
     
       
	mod03dir="/data/keeling/a/yulanh/satellite/TerraDataArchive/MOD03/"//yrindex//"/"//Julday//"/"
	mod02dir="/data/keeling/a/yulanh/satellite/TerraDataArchive/MOD021KM/"//yrindex//"/"//Julday//"/"
        merradir="/data/keeling/a/yulanh/c/MERRA-2/"//yrindex//"/"

        yyddhhmm=myday(73:85)

        call system("ls "//trim(mod02dir)//"MOD021KM.A"//yyddhhmm//"*"//" &
                > mod02file"//yyddhhmm,status)
        open(400,file="mod02file"//yyddhhmm)
        read(400,fmt="(a112)",iostat=status) mod02fname
        close(400)

        !=========== read modis file ===========================================
        !======== search modis file in the same day as cldclass file ============
        !mod03fname=trim(mod03dir)//myday
        mod03fname=trim(myday)
 
        merra2fname=trim(merradir)//"MERRA2_400.inst1_2d_asm_Nx."//yrindex//month_1//day_1//".hdf"     

        print *, merra2fname
        INQUIRE (file=mod02fname,exist=mod02flag)
        INQUIRE (file=merra2fname,exist=merraflag)

        print *, myid,mod02flag,merraflag,mod02fname,mod03fname

        IF (mod02flag .and. merraflag) Then 

        call read_modis03(mod03fname)
        call read_modis02(mod02fname)
        call read_merra2_hdf(merra2fname)
       
       ! print *, trop_temp(1,1,hour_scp)
        !print *, shape(rad_1000emis)  
        allocate(BT11(modNcol,modNrow))
        allocate(BT67(modNcol,modNrow))
        allocate(BT11_a(modNcol,modNrow))
        allocate(BT11_b(modNcol,modNrow))

        allocate(OT_BT(modNcol,modNrow))
        allocate(OT_BTD(modNcol,modNrow))
        allocate(OT_BTD_trop(modNcol,modNrow))
        allocate(OT_flag(modNcol,modNrow))
        allocate(mean_cirrus_bt(modNcol,modNrow))


        OT_BT=0.0
        OT_BTD=0.0
        OT_BTD_trop=0.0
        OT_flag=0
        mean_cirrus_bt=0.0

        BT11=planck_c2/(11.03*log(planck_c1/(rad_1000emis(:,:,11)*11.03**5)+1))
        BT67=planck_c2/(6.72*log(planck_c1/(rad_1000emis(:,:,7)*6.72**5)+1))


        read(myday(81:82),'(i2)') modhh
        read(myday(83:84),'(i2)') modmm

        hour_scp=floor(modhh+modmm/60.0)+1

        !print *,'1',myid,modhh,modmm,hour_scp,myday

	Do colj=1,modNcol
           Do rowi=1, modNrow

		tplat=modlat(colj,rowi)
		tplon=modlon(colj,rowi)

                merralon_scp=nint((tplon+180)/0.625)+1
                merralat_scp=nint((tplat+90)/0.5)+1

                !print *,tplat,merralat_scp,tplon,merralon_scp,modhh,hour_scp

           !     print *, BT11(colj,rowi),trop_temp(merralon_scp,merralat_scp,hour_scp),&
            !           merralon_scp,merralat_scp 
                
                IF ((BT11(colj,rowi) <= trop_temp(merralon_scp,merralat_scp,hour_scp))) Then ! <= BT_threshold)) Then ! .and. BT11 <= trop_temp(merralon_scp,merralat_scp,hour_scp)) Then 

                OT_BTD_trop(colj,rowi)=BT11(colj,rowi)-trop_temp(merralon_scp,merralat_scp,hour_scp)
               ! print *,colj,rowi,OT_BTD_trop(colj,rowi)
                ! select a box with diameter of 15 km
		colupres=colj+ win_size
		collowres=colj- win_size
		rowupres=rowi + win_size
		rowlowres=rowi - win_size
                
!           call date_and_time(values=values)
!           print *,'1 ',values
                ! deal with boundary 
		IF (colupres < 1) colupres=1
		IF (colupres > modNcol) colupres=modNcol
                ! deal with boundary
		IF (rowlowres < 1) Then
                    rowlowres=1
                    !go to previous image    
                    IF (modmm .ge. 5) Then
                        bmodmm=modmm-5
                        bmodhh=modhh
                        Julday_as=Julday
                    EndIf
                    IF (modmm .lt. 5) Then
                        if (bmodhh .ge. 1) then 
                        bmodhh=modhh-1
                        bmodmm=55
                        Julday_as=Julday
                        endif

                        if (bmodhh .eq. 0) then
                        bmodhh=23
                        bmodmm=55
                        Julday_a=Julday_1-1
                        write(Julday_as,'(i3)') Julday_a
                        print *,Julday_as

                        endif

                    EndIf

                    write(bmodmm_1,'(i0)') bmodmm
                    if (len(trim(bmodmm_1)) == 1) bmodmm_1='0'//trim(bmodmm_1)
                    write(bmodhh_1,'(i0)') bmodhh
                    if (len(trim(bmodhh_1)) == 1) bmodhh_1='0'//trim(bmodhh_1)

                    byyddhhmm=yrindex//Julday_as//'.'//bmodhh_1//bmodmm_1
                    !print *, byyddhhmm,myid

                    call system("ls "//trim(mod02dir)//"MOD021KM.A"//byyddhhmm//"*"//" &
                                > mod02file"//byyddhhmm,status)
                    open(500,file="mod02file"//byyddhhmm)
                    read(500,fmt="(a112)",iostat=status) bmod02fname
                    close(500)

                    !call system("ls "//trim(mod03dir)//"MOD03.A"//byyddhhmm//"*"//" &
                    !            > mod03file"//byyddhhmm,status)
                    !open(600,file="mod03file"//byyddhhmm)
                    !read(600,fmt="(a105)",iostat=status) bmod03fname
                    !close(600)
                    print *,'go to before', bmod02fname!,bmod03fname

                    call read_modis02_b(bmod02fname)
                    BT11_b=planck_c2/(11.03*log(planck_c1/(rad_1000emis_b(:,:,11)*11.03**5)+1))


                    !print *, colj,rowi,win_size+1-colj+collowres,win_size+1+colupres-colj,win_size+1-rowi+rowlowres
                    tpbt11_box(win_size+1-(colj-collowres):win_size+1+colupres-colj,&
                                  win_size+1-(rowi-rowlowres):2*win_size+1)&
                        =BT11(collowres:colupres,rowlowres:rowupres)
     
                    !print *,colj,rowi, modNrow-win_size+rowi-rowlowres+1,modNrow,win_size-rowi+rowlowres

                    tpbt11_box(win_size+1-colj+collowres:win_size+1+colupres-colj,&
                                1:win_size-rowi+rowlowres)&
                        =BT11_b(win_size+1-colj+collowres:win_size+1+colupres-colj,&
                        modNrow-win_size+rowi-rowlowres+1:modNrow)                       
                        deallocate(rad_1000emis_b)
                
                EndIf ! end if rowllowres <1

        !   call date_and_time(values=values)
        !   print *,'2 ',values
                IF (rowupres > modNrow) Then
                       
                        rowupres=modNrow
                        IF (modmm .le. 50) Then
                            amodmm=modmm+5
                            amodhh=modhh    
                        EndIf
                        IF (modhh .eq. 55) Then 
                            amodmm=0    
                            amodhh=modhh+1    
                        EndIf

                        write(amodmm_1,'(i0)') amodmm
                        if (len(trim(amodmm_1)) == 1) amodmm_1='0'//trim(amodmm_1)
                        write(amodhh_1,'(i0)') amodhh
                        if (len(trim(amodhh_1)) == 1) amodhh_1='0'//trim(amodhh_1)

                        ayyddhhmm=yrindex//Julday//'.'//amodhh_1//amodmm_1

                        !print *, ayyddhhmm,myid

                        call system("ls "//trim(mod02dir)//"MOD021KM.A"//ayyddhhmm//"*"//" &
                                > mod02file"//ayyddhhmm,status)
                        open(600,file="mod02file"//ayyddhhmm)
                        read(600,fmt="(a112)",iostat=status) amod02fname
                        close(600)

                        call system("ls "//trim(mod03dir)//"MOD03.A"//ayyddhhmm//"*"//" &
                                > mod03file"//ayyddhhmm,status)
                        open(600,file="mod03file"//ayyddhhmm)
                        read(600,fmt="(a105)",iostat=status) amod03fname
                        close(600)

                        call read_modis02_a(amod02fname)

                        BT11_a=planck_c2/(11.03*log(planck_c1/(rad_1000emis_a(:,:,11)*11.03**5)+1))

                        !print *,'go to after', amod02fname,amod03fname
                        !print *,win_size+1-(colj-collowres),win_size+1+colupres-colj,&
                        !        rowupres-rowlowres,collowres,colupres,rowlowres,rowupres

                        tpbt11_box(win_size+1-(colj-collowres):win_size+1+(colupres-colj),&
                                  1:rowupres-rowlowres)&
                        =BT11(collowres:colupres,rowlowres:rowupres)

                             
                        !print*,lowres+1,2*win_size+1-rowupres+rowlowres
                        tpbt11_box(win_size+1-(colj-collowres):win_size+1+(colupres-colj),&
                                rowupres-rowlowres+1:2*win_size+1)&
                        =BT11_a(collowres:colupres,&
                                1:2*win_size+1-rowupres+rowlowres)                       
                       

                         deallocate(rad_1000emis_a)
               
                EndIf

                IF (rowupres <= modNrow .and. rowlowres >= 1) Then 
                      tpbt11_box=BT11(collowres:colupres,rowlowres:rowupres)  
                      
              !  print *,colj,rowi,rowupres,rowlowres,win_size+1-(colj-collowres),win_size+1+(colupres-colj),&
              !  rowlowres,rowupres,BT11(colj,rowi),minval(tpbt11_box)
               
                EndIf

        !   call date_and_time(values=values)
        !   print *,'3 ',values
                ! only when the box minimum BT eq BT11(colj,rowi)
                cirrus_bt=0.0
           !     print *,colj,rowi,rowupres,rowlowres,BT11(colj,rowi),minval(tpbt11_box)
                
                IF (BT11(colj,rowi) .eq. minval(tpbt11_box)) Then

            !    print *,'min BT',colj,rowi, BT11(colj,rowi), minval(tpbt11_box)
         !  call date_and_time(values=values)
         !  print *,'4 ',values

                        call array_vector_2(tpbt11_box,BT11(colj,rowi),cirrus_bt)

         !  call date_and_time(values=values)
         !  print *,'5 ',values
                
                        !tpbt11_ray_1=0.0
                        !Do ri=1,Nray
                        !print *,tpbt11_ray(:,ri)
                        !where(tpbt11_ray(:,ri) > BT11(colj,rowi) .and. tpbt11_ray(:,ri) <= cirrus_threshold)
                        ! tpbt11_ray_1=tpbt11_ray(:,ri)
                        !endwhere
                        ! cirrus_bt(ri)=sum(tpbt11_ray_1)/count((tpbt11_ray_1)>0)
                        !print *, tpbt11_ray_1
                       !print *,cirrus_bt(ri),sum(tpbt11_ray_1),count(tpbt11_ray_1 > 0)
                        
                        !EndDo ! end ri
                        ! judge cirrus 
                        
                        IF (count(cirrus_bt >0) >=5) Then 
                             OT_flag(colj,rowi) = 1
                             OT_BT(colj,rowi)   = BT11(colj,rowi)
                             OT_BTD(colj,rowi)  = BT67(colj,rowi)-BT11(colj,rowi)       
                             mean_cirrus_bt(colj,rowi) = sum(cirrus_bt)/count(cirrus_bt > 0)
                        !    print *, OT_flag(colj,rowi),colj,rowi
                        EndIF
                        !calculate cirrus temperature
          ! call date_and_time(values=values)
          ! print *,'7 ',values
               
                EndIf ! end if the box

          ! call date_and_time(values=values)
          ! print *,'8 ',values

                EnDIf ! end bt_threhold
      
	   EndDo
	EndDo 

        deallocate(BT11)
        deallocate(BT11_a)
        deallocate(BT11_b)

        wfname='/data/keeling/a/yulanh/c/OT_output/MODIS_OTinfo_'//myday(73:85)//'hdf'
        !print *, wfname
        !print *,OT_BTD_trop(650,969)

        call write_OT(wfname)

        include 'deallocate_array.file'

        EndIf

        IF (myid == my_root) Then
           deallocate(mod03_list)
           call system("rm -rf inputfile*")
           call system("rm -rf mod02file*")
           call system("rm -rf mod03file*")
           call date_and_time(values=values)
           print *,'finish ',values
        EndIf

    call MPI_FINALIZE(mpi_err)

 end
