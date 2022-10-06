	program main_modis_OT_parallel1

	use global
        use gaussian_filter, only: gaussian_kernel, convolve

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
	character(len=112) :: myday
	character(len=112), allocatable :: mod03_list(:)

	character(len=210) :: mod03fname,bmod03fname,amod03fname
	character(len=210) :: mod02fname,bmod02fname,amod02fname
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
        real, allocatable :: &
                BT11_a(:,:),BT11_b(:,:),BT67_a(:,:),BT67_b(:,:),&
                BT11_sm(:,:),BT11_asm(:,:),BT11_bsm(:,:)

	integer :: colupres,collowres,rowupres,rowlowres
        integer :: st_colupres,st_collowres,st_rowupres,st_rowlowres
 
        real, parameter :: planck_c1=1.191042e8, planck_c2=1.4387752e4
        real :: BT_threshold
       ! integer, parameter :: win_size=7,box_size=8, Nray=16
        integer :: ri

        real :: tpbt11_box(win_size*2+1,win_size*2+1),&
                tpbt11_box_sm(win_size*2+1,win_size*2+1),&
                tpbt67_box(win_size*2+1,win_size*2+1)
               ! tpbt11_ray(win_size+1,Nray),tpbt11_ray_1(win_size+1)

        real (kind=4) :: cirrus_bt(Nray),cirrus_bt_sum(Nray),&
                ot_bt(Nray),ot_bt_var(Nray),ot_bt67(Nray),ci_bt_var(Nray)
        integer :: OT_edge(Nray),ot_bt_var_num

        real :: storm_bt(Nray),storm_edge(Nray)

        logical :: mod02flag, mod03flag, merraflag

        !========= some local variables ==================
        real :: tp_ave_otbt11,tp_ave_otbt67,tp_ave_cibt11,&
                tp_std_otbt11,tp_std_cibt11,tp_std_otci,tp_tropopause_t,tpfx
        real :: tpdata(30)

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
        "/data/keeling/a/yulanh/satellite/TerraDataArchive/MODIS/MOD03/"//&
        yrindex//"/"//Julday//"/"//cld_date//"*"

        call system("ls "//trim(mod03dir)//" > inputfile"//cld_date,status)
        call system("cat inputfile"//cld_date//" | wc -l > count"//cld_date)
 
        open(100,file="count"//cld_date)
        read(100,*) Nodes
        close(100)
        print *,'number of file', Nodes
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
       
	mod03dir=&
        "/data/keeling/a/yulanh/satellite/TerraDataArchive/MODIS/MOD03/"//yrindex//"/"//Julday//"/"
	mod02dir=&
        "/data/keeling/a/yulanh/satellite/TerraDataArchive/MODIS/MOD021KM/"//yrindex//"/"//Julday//"/"

        merradir="/data/keeling/a/yulanh/c/MERRA-2/"//yrindex//"/"

        yyddhhmm=myday(79:91)

        
        call system("ls "//trim(mod02dir)//"MOD021KM.A"//yyddhhmm//"*"//" &
                > mod02file"//yyddhhmm,status)
        open(400,file="mod02file"//yyddhhmm)
        read(400,fmt="(a118)",iostat=status) mod02fname
        close(400)

        !=========== read modis file ===========================================
        !======== search modis file in the same day as cldclass file ============
        !mod03fname=trim(mod03dir)//myday
        mod03fname=trim(myday)
 
        merra2fname=trim(merradir)//"MERRA2_300.inst1_2d_asm_Nx."//yrindex//month_1//day_1//".hdf"     

!        print *, merra2fname
        INQUIRE (file=mod02fname,exist=mod02flag)
        INQUIRE (file=merra2fname,exist=merraflag)

        print *, myid,merra2fname,mod02flag,merraflag,mod02fname,mod03fname
       
        IF (mod02flag .and. merraflag) Then 

        call read_modis03(mod03fname)
        call read_modis02(mod02fname)
        call read_merra2(merra2fname)
       
       ! print *, trop_temp(1,1,hour_scp)
       ! print *,'dimension,ncol,nrow :',modNcol,modNrow, shape(rad_1000emis)  
        allocate(BT11(modNcol,modNrow))
        allocate(BT11_sm(modNcol,modNrow))
        allocate(BT67(modNcol,modNrow))


        !allocate(OT_BT(modNcol,modNrow))
        !allocate(OT_BTD(modNcol,modNrow))
        allocate(OT_BTD_trop(modNcol,modNrow))
        !allocate(OT_flag(modNcol,modNrow))
        allocate(mean_cirrus_bt(modNcol,modNrow))
        !allocate(cirrus_radii_num(modNcol,modNrow))
        allocate(OT_area(modNcol,modNrow))
        !allocate(storm_area(modNcol,modNrow))
        !allocate(mean_storm_bt(modNcol,modNrow))
        allocate(mean_OT_BT(modNcol,modNrow))
        allocate(OT_proba(modNcol,modNrow))        

        !OT_BT=0.0
        !OT_BTD=0.0
        OT_BTD_trop=0.0
        !OT_flag=0
        mean_cirrus_bt=0.0
        OT_area=0.0
        OT_proba=0.0

        BT11=planck_c2/(11.03*log(planck_c1/(rad_1000emis(:,:,11)*11.03**5)+1))
        BT67=planck_c2/(6.72*log(planck_c1/(rad_1000emis(:,:,7)*6.72**5)+1))

        ! smooth BT11 by using gaussian filter
        call gaussian_kernel(1.0,kernel)
        !print *,'bt11 shape',shape(BT11)
        call convolve(BT11,kernel,BT11_sm) 

        !print *, modNcol, modNrow,BT11(1,1),BT11_sm(1,1)
        
        !open(100, file='test_smooth.txt')
        !Do colj=1,modNcol
        !   Do rowi=1,modNrow
                !   write(100,*) BT11(colj,rowi), BT11_sm(colj,rowi)
        !   EndDo
        !EndDo
        
        read(myday(87:88),'(i2)') modhh
        read(myday(89:90),'(i2)') modmm

        hour_scp=floor(modhh+modmm/60.0)+1

        !print *,'1',myid,modhh,modmm,hour_scp,myday

        wfname='/data/keeling/a/yulanh/c/OT_output/MODIS_OTinfo_'//myday(79:91)//'txt'

        OPEN(1000,file=wfname)

        ! for one data file, it will have one data file before, one file
        ! after that 5 minute window
               
        bmod_read_flag=0
        amod_read_flag=0
 
        Do colj=1,modNcol
           Do rowi=1, modNrow

                tpbt11_box_sm=0.0
                tpbt11_box=0.0
                tpbt67_box=0.0

                tplat=modlat(colj,rowi)
                tplon=modlon(colj,rowi)

                merralon_scp=nint((tplon+180)/0.625)+1
                merralat_scp=nint((tplat+90)/0.5)+1
                if (merralon_scp < 1) merralon_scp=1
                if (merralon_scp > merra2_x) merralon_scp=merra2_x
                if (merralat_scp < 1) merralat_scp=1
                if (merralat_scp > merra2_y) merralat_scp=merra2_y
 
                !print *,colj,rowi,tplat,merralat_scp,tplon,merralon_scp,modhh,hour_scp

            !    merralon_scp,merralat_scp 
            !    BT_threshold=186.81031+0.254929*tplat+0.0218272*tplat**2-0.00027805*tplat**3

                IF (abs(tplat) <= 25) BT_threshold = 200
                IF (abs(tplat) > 25) BT_threshold = 230

                ! === tropopause information
                if (abs(tplat) < 60) then
                        OT_BTD_trop(colj,rowi)=trop_temp(merralon_scp,merralat_scp,hour_scp)
                        tp_tropopause_t=trop_temp(merralon_scp,merralat_scp,hour_scp)
                endif
                
                IF ((BT11(colj,rowi) <= BT_threshold) .and. &
                    (BT11(colj,rowi) < tp_tropopause_t) .and. &
                     abs(tplat) < 60) Then ! .and. &

                !OT_BTD_trop(colj,rowi)=BT11(colj,rowi)-trop_temp(merralon_scp,merralat_scp,hour_scp)
               ! print *,colj,rowi,OT_BTD_trop(colj,rowi)
                ! select a box with diameter of 15 km
		    colupres=colj+ win_size
		    collowres=colj- win_size
		    rowupres=rowi + win_size
		    rowlowres=rowi - win_size
                
!           call date_and_time(values=values)
!           print *,'1 ',values
                ! deal with boundary 
		!IF (colupres < 1) colupres=1
		!IF (colupres > modNcol) colupres=modNcol
                ! deal with boundary

		IF (rowlowres < 1 .and. collowres > 0 &
                .and. colupres <= modNcol) Then

                    rowlowres=1
                    !go to previous image and the data is not read   
                    print *,bmod_read_flag, modmm
                    IF (bmod_read_flag ==0) Then 
                
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

                        print *,bmod_read_flag, bmodmm,bmodmm_1,bmodhh,bmodhh_1
                        byyddhhmm=yrindex//Julday_as//'.'//bmodhh_1//bmodmm_1

                        print *,myid,byyddhhmm 
                       
                        call system("ls "//trim(mod02dir)&
                                //"MOD021KM.A"//byyddhhmm//"*"//" &
                                > mod02file"//byyddhhmm,status)
                        open(500,file="mod02file"//byyddhhmm)
                        read(500,fmt="(a118)",iostat=status) bmod02fname
                        close(500)

                    !call system("ls "//trim(mod03dir)//"MOD03.A"//byyddhhmm//"*"//" &
                    !            > mod03file"//byyddhhmm,status)
                    !open(600,file="mod03file"//byyddhhmm)
                    !read(600,fmt="(a105)",iostat=status) bmod03fname
                    !close(600)
                        print *,'go to before',myid,byyddhhmm,&
                        colj,rowi,rowlowres, bmod02fname!,bmod03fname


                        call read_modis02_b(bmod02fname)
                        allocate(BT11_b(modNcol_b,modNrow_b))
                        allocate(BT67_b(modNcol_b,modNrow_b))
                        allocate(BT11_bsm(modNcol_b,modNrow_b))
                        !print *,'bt11_b shape',shape(BT11_bsm),modNcol_b,modNrow_b

                        BT11_b=planck_c2/(11.03*log(planck_c1/(rad_1000emis_b(:,:,11)*11.03**5)+1))
                        BT67_b=planck_c2/(6.72*log(planck_c1/(rad_1000emis_b(:,:,7)*6.72**5)+1))
                        call gaussian_kernel(1.0,kernel)
                        call convolve(BT11_b,kernel,BT11_bsm) 
                        bmod_read_flag=1
                        deallocate(rad_1000emis_b)
                        !print *,shape(BT11_bsm),BT11_b(1,1),BT11_bsm(1,1)
                   EndIF ! end reading     

                    !print *, colj,rowi,rowlowres,rowupres,&
                    !    win_size+1-rowi+rowlowres

                    tpbt11_box_sm(:,&
                                  win_size+1-(rowi-rowlowres):2*win_size+1)&
                        =BT11_sm(collowres:colupres,rowlowres:rowupres)
                    tpbt11_box(:,&
                                  win_size+1-(rowi-rowlowres):2*win_size+1)&
                        =BT11(collowres:colupres,rowlowres:rowupres)
     
                    tpbt67_box(:,&
                                  win_size+1-(rowi-rowlowres):2*win_size+1)&
                        =BT67(collowres:colupres,rowlowres:rowupres)

                   ! print *,colj,rowi, modNrow_b-win_size+rowi-rowlowres+1,&
                   !     modNrow_b,win_size-rowi+rowlowres

                    tpbt11_box_sm(:,&
                                1:win_size-rowi+rowlowres)&
                        =BT11_bsm(collowres:colupres,&
                        modNrow_b-win_size+rowi-rowlowres+1:modNrow_b)  

                    tpbt11_box(:,&
                                1:win_size-rowi+rowlowres)&
                        =BT11_b(collowres:colupres,&
                        modNrow_b-win_size+rowi-rowlowres+1:modNrow_b)


                    tpbt67_box(:,&
                                1:win_size-rowi+rowlowres)&
                        =BT67_b(collowres:colupres,&
                        modNrow_b-win_size+rowi-rowlowres+1:modNrow_b)  
               
                     !open(1500,file='output_btarraybefor.txt')

                     !do ri=1,41
                     !write(1500,*) tpbt11_box_sm(:,ri)
                     !enddo
                  
                EndIf ! end if rowllowres <1

        !   call date_and_time(values=values)
        !   print *,'2 ',values
                IF (rowupres > modNrow .and. collowres > 0 .and. &
                colupres <= modNcol) Then
                       
                        rowupres=modNrow

                        IF (amod_read_flag == 0) THEN

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
                        read(600,fmt="(a118)",iostat=status) amod02fname
                        close(600)

                        call system("ls "//trim(mod03dir)//"MOD03.A"//ayyddhhmm//"*"//" &
                                > mod03file"//ayyddhhmm,status)
                        open(600,file="mod03file"//ayyddhhmm)
                        read(600,fmt="(a112)",iostat=status) amod03fname
                        close(600)


                        call read_modis02_a(amod02fname)
                        allocate(BT67_a(modNcol_a,modNrow_a))
                        allocate(BT11_a(modNcol_a,modNrow_a))
                        allocate(BT11_asm(modNcol_a,modNrow_a))

                        BT11_a=planck_c2/(11.03*log(planck_c1/(rad_1000emis_a(:,:,11)*11.03**5)+1))
                        BT67_a=planck_c2/(6.72*log(planck_c1/(rad_1000emis_a(:,:,7)*6.72**5)+1))
                        call gaussian_kernel(1.0,kernel)
                        call convolve(BT11_a,kernel,BT11_asm) 

                        deallocate(rad_1000emis_a)
                        amod_read_flag=1
                        print *,'go to and read after file', amod02fname,amod03fname
                        EndIF

                        !print *,win_size+1-(colj-collowres),win_size+1+colupres-colj,&
                        !        rowupres-rowlowres,collowres,colupres,rowlowres,rowupres

                        tpbt11_box_sm(:,&
                                  1:rowupres-rowlowres+1)&
                        =BT11_sm(collowres:colupres,rowlowres:rowupres)

                        tpbt11_box(:,&
                                  1:rowupres-rowlowres+1)&
                        =BT11(collowres:colupres,rowlowres:rowupres)

                        tpbt67_box(:,&
                                  1:rowupres-rowlowres+1)&
                        =BT67(collowres:colupres,rowlowres:rowupres)

                             
                     !   print*,lowres+1,2*win_size+1-rowupres+rowlowres
                        tpbt11_box_sm(win_size+1-(colj-collowres):win_size+1+(colupres-colj),&
                                rowupres-rowlowres+2:2*win_size+1)&
                        =BT11_asm(collowres:colupres,&
                                1:2*win_size-rowupres+rowlowres)                       

                        tpbt11_box(win_size+1-(colj-collowres):win_size+1+(colupres-colj),&
                                rowupres-rowlowres+2:2*win_size+1)&
                        =BT11_a(collowres:colupres,&
                                1:2*win_size-rowupres+rowlowres)                       


                        tpbt67_box(win_size+1-(colj-collowres):win_size+1+(colupres-colj),&
                                rowupres-rowlowres+2:2*win_size+1)&
                        =BT67_a(collowres:colupres,&
                                1:2*win_size-rowupres+rowlowres)                       

                        !IF (colj == 1274 .and. rowi == 2029) Then
                        !print *,rowupres,rowlowres,colj,rowi
                        !print *,colj,rowi
                        !print *,tpbt11_box(21,:)
                        !print *, BT11(colj,rowlowres:rowupres)
                        !print *,tpbt11_box(:,21)
                        !print *, BT11_a(colj,1:8)
                        ! print *,BT11_asm(collowres:colupres,&
                        !        1:2*win_size-rowupres+rowlowres)
                        ! print *, rowupres-rowlowres+2,2*win_size+1
                        !! print *,tpbt11_box_sm(:,&
                        !        rowupres-rowlowres+2:2*win_size+1)
                       ! print *,tpbt11_box_sm(:,33:41)
               
                        !EndIf
                EndIf

                !        IF (colj == 1274 .and. rowi == 2029) Then
                !        print *,colj,rowi
                !        print *,tpbt11_box_sm(:,33:41)
                !        EndIf

                !print *,rowupres,rowlowres,collowres,colupres
                IF (rowupres < modNrow .and. rowlowres > 1 &
                  .and. collowres >= 1 .and. colupres <= modNcol) Then 

                      tpbt11_box_sm=BT11_sm(collowres:colupres,rowlowres:rowupres)  
                      tpbt11_box=BT11(collowres:colupres,rowlowres:rowupres)  
                      tpbt67_box=BT67(collowres:colupres,rowlowres:rowupres)  
              !  print *,colj,rowi,rowupres,rowlowres,win_size+1-(colj-collowres),win_size+1+(colupres-colj),&
              !  rowlowres,rowupres,BT11(colj,rowi),minval(tpbt11_box)
                 EndIf
                !        IF (colj == 1274 .and. rowi == 2029) Then
                !        print *,colj,rowi
                !        print *,tpbt11_box_sm(:,33:41)
                !        stop
                !        EndIf

        !   call date_and_time(values=values)
        !   print *,'3 ',values
                ! only when the box minimum BT eq BT11(colj,rowi)
               
                
                IF (BT11_sm(colj,rowi) .eq. minval(tpbt11_box_sm)) Then

                !print *,colj,rowi,rowupres,rowlowres,BT11(colj,rowi),minval(tpbt11_box)
                !print *,'min BT',colj,rowi, BT11(colj,rowi), minval(tpbt11_box)
                !call date_and_time(values=values)
                !print *,'4 ',values
                !print *,'center bt',colj,rowi,BT11(colj,rowi),BT11_sm(colj,rowi),BT67(colj,rowi)
                !print *,colj,rowi 
              
                !print *,tpbt11_box_sm(:,33:41)
                 call array_vector_2(tpbt11_box_sm,tpbt11_box,tpbt67_box,cirrus_bt,&
                       cirrus_bt_sum,ot_bt,ot_bt_var,ot_bt67,OT_edge,ot_bt_var_num,&
                        ci_bt_var)

                
                 !OT_BT(colj,rowi)   = BT11(colj,rowi)
                 !OT_BTD(colj,rowi)  = BT67(colj,rowi)-BT11(colj,rowi)       
   
                 tp_ave_otbt11 = sum(ot_bt)/sum(OT_edge)!count(ot_bt > 0.0)
                 tp_ave_otbt67 = sum(ot_bt67)/count(ot_bt67 > 0.0)
                 tp_ave_cibt11 = sum(cirrus_bt_sum)/&
                        (ot_bt_var_num-sum(OT_edge))!count(cirrus_bt > 0)
                 tp_std_otci = sqrt((sum(ot_bt_var)+sum(ci_bt_var))/ot_bt_var_num- &
                       ((sum(ot_bt)+sum(cirrus_bt_sum))/ot_bt_var_num)**2)
                     ! (((tp_ave_otbt11+tp_ave_cibt11)/2.0)**2.0))

                 tp_std_otbt11 = sqrt(sum(ot_bt_var)/sum(OT_edge) - &
                       tp_ave_otbt11**2)

                 tp_std_cibt11 = sqrt(sum(ci_bt_var)/(ot_bt_var_num-sum(OT_edge)) - &
                       tp_ave_cibt11**2)
                 !print *,ot_bt_var,ot_bt_var_num,sum(ot_bt_var)
                 !print *,tp_ave_otbt11,tp_std_otbt11,sum(ot_bt_var)/ot_bt_var_num
                 !print *,(tp_ave_otbt11+tp_ave_cibt11)/2.0,&
                 !       ((tp_ave_otbt11+tp_ave_cibt11)/2.0)**2.0
                 !print *,((sum(ot_bt)+sum(cirrus_bt))/ot_bt_var_num)**2
                 !print *,tp_std_otbt11,tp_std_cibt11,tp_std_otci
                 !print *,tp_ave_otbt67,ot_bt67
                 !print *,tp_ave_cibt11,cirrus_bt 
                 !stop

                 IF ((tp_ave_otbt67 > tp_ave_otbt11) .and. (tp_ave_cibt11 > tp_ave_otbt11)) Then
                         mean_cirrus_bt(colj,rowi) = tp_ave_cibt11
                         OT_area(colj,rowi)=pi*(float(sum(OT_edge))/count(OT_edge > 0))**2.0
                         !!cirrus_radii_num(colj,rowi)=count(cirrus_bt > 0.0)

                         !OT_flag(colj,rowi) = 1
                         mean_OT_BT(colj,rowi) = tp_ave_otbt11
                         ! calculate the probability === 
                         tpfx=const+coef_1*(tp_ave_cibt11-BT11(colj,rowi))+coef_2*(tp_tropopause_t-BT11(colj,rowi))+&
                                coef_3*(tp_ave_otbt67-tp_ave_otbt11)

                         OT_proba(colj,rowi)=1.0/(1+exp(0.0-tpfx))
                        ! print *,OT_proba(colj,rowi),colj,rowi,tp_ave_otbt11,tp_ave_otbt67,tp_ave_cibt11
                         
                 !===== investigate the storm property ========
                 !===== get the bt for the array of bt arround 1000 km 
                 
                        !call get_storm_array(BT11,BT67,colj,rowi,storm_edge,storm_bt)      
                        !storm_area(colj,rowi) = &
                        !pi*(sum(storm_edge)/count(storm_edge > 0))**2.0 

                        !mean_storm_bt(colj,rowi) = &
                        !sum(storm_bt)/count(storm_edge > 0)

                        write(1000,*) colj,rowi,tplon,tplat,OT_proba(colj,rowi),OT_area(colj,rowi),&
                        BT11(colj,rowi),tp_ave_otbt11,BT67(colj,rowi),tp_ave_otbt67,&
                        tp_tropopause_t,tp_ave_cibt11,cirrus_bt(1),cirrus_bt(2),cirrus_bt(3),&
                        cirrus_bt(4),cirrus_bt(5),cirrus_bt(6),cirrus_bt(7),cirrus_bt(8),&
                        OT_edge(1),OT_edge(2),OT_edge(3),OT_edge(4),OT_edge(5),OT_edge(6),&
                        OT_edge(7),OT_edge(8),tp_std_otbt11,tp_std_cibt11,tp_std_otci!,&
                        !mean_storm_bt(colj,rowi),storm_area(colj,rowi) 
                 EndIF
                 !print *,storm_edge,storm_area(colj,rowi),sum(storm_edge)/count(storm_edge > 0)
                 !print *, mean_storm_bt(colj,rowi),storm_bt

                EndIf ! end if the box

                EnDIf ! end bt_threhold
      
	   EndDo
	EndDo 
        !print *,sum(OT_flag)
        close(1000)

        !wfname='/data/keeling/a/yulanh/c/OT_output/MODIS_OTinfo_'//myday(79:91)//'hdf'
        !call write_OT(wfname)
        deallocate(BT11_sm)
        !deallocate(OT_flag)
        deallocate(BT11)
        deallocate(BT67)
        deallocate(OT_BTD_trop)
        deallocate(mean_cirrus_bt)
        deallocate(OT_area)
        !deallocate(storm_area)
        !deallocate(mean_storm_bt)
        !deallocate(cirrus_radii_num)
        deallocate(mean_OT_BT)

        IF (amod_read_flag == 1) Then
           deallocate(BT11_a)
           deallocate(BT67_a)
           deallocate(BT11_asm)
        EndIF

        IF (bmod_read_flag ==1) Then
           deallocate(BT11_bsm,stat=status)
           deallocate(BT67_b)
           deallocate(BT11_b)
        EndIf

        include 'deallocate_array.file'

        EndIf ! Endif merra2flag =1 or mod03flag=1

        IF (myid == my_root) Then
           deallocate(mod03_list)
           !call system("rm -rf "//"inputfile"//cld_date)
           !call system("rm -rf "//"count"//cld_date)
           !call system("rm -rf "//"mod02file"//yyddhhmm)
           !call system("rm -rf "//"mod03file"//yyddhhmm)
           call date_and_time(values=values)
           print *,'finish ',values
        EndIf

        call MPI_FINALIZE(mpi_err)

        end
