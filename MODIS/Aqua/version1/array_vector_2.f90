
        !https://en.wikipedia.org/wiki/Xiaolin_Wu%27s_line_algorithm
        !Xiaolin Wu's line algorithm

	subroutine array_vector_2(array,array_bs,array_bt67,cirrus_bt,&
                cirrus_bt_sum,ot_bt,ot_bt_var,ot_bt67,OT_edge,&
                ot_bt_var_num,ci_bt_var)

        use global, only : cirrus_threshold,win_size,Nray

        implicit none

        integer :: x0,y0,x1,y1,x,y
        integer :: tpx,tpy,ri,wi
        real :: xpx11,ypx11,xpx12,ypx12,xend,intery
        logical :: steep
        real :: dy,dx, gradient
        real :: angle

        real :: array(2*win_size+1,2*win_size+1),&
                array_bs(2*win_size+1,2*win_size+1),& !
                array_bt67(2*win_size+1,2*win_size+1)

        real (kind=4) :: cirrus_bt(Nray),cirrus_bt_sum(Nray),&
                ot_bt(Nray),ot_bt67(Nray),ot_bt_var(Nray),&
                ci_bt_var(Nray)
        integer :: edge_loc,OT_edge(Nray),ot_bt_var_num

        real, parameter :: pi=3.1415927

        real :: radical_value(win_size+1,Nray),tpbt11_ray_1(win_size+1)
        real :: radical_bt67(win_size+1,Nray)
        real :: radical_bt11_bs(win_size+1,Nray) ! record the values before sm
        real :: first_deriv(win_size+1), second_deriv(win_size+1)
        real :: bdt_6711


        cirrus_bt=0.0
        cirrus_bt_sum=0.0
        ci_bt_var=0.0

        OT_edge=0
        ot_bt=0.0
        ot_bt_var=0.0
        ot_bt_var_num=0
        ot_bt67=0.0
        radical_value=0.0
        radical_bt67=0.0
        radical_bt11_bs=0.0
        

        Do ri=0,Nray-1

        tpbt11_ray_1=0.0

        x0=win_size+1
        y0=win_size+1

        angle=ri*360.0/Nray

        !print *,angle,(sin(angle*pi/180.0)),(cos(angle*pi/180.0))
        IF (cos(angle*pi/180.0) >=0.001) x1=x0+win_size!ceiling(((win_size)*cos(angle*pi/180.0)))
        IF (cos(angle*pi/180.0) > -0.001 .and. cos(angle*pi/180.0) < 0.001) x1=x0
        IF (cos(angle*pi/180.0) <=  -0.001) x1=x0-win_size!+floor(((win_size)*cos(angle*pi/180.0)))
        IF (sin(angle*pi/180.0) >=0.001) y1=y0+win_size!ceiling(((win_size)*sin(angle*pi/180.0)))
        IF (sin(angle*pi/180.0) > -0.001 .and. sin(angle*pi/180.0) < 0.001) y1=y0
        IF (sin(angle*pi/180.0) <= -0.001) y1=y0-win_size!+floor(((win_size)*sin(angle*pi/180.0)))

        steep=abs(y1-y0) > abs(x1-x0)
!        print *,'x0,y0,x1,y1,steep',x0,y0,x1,y1,steep
      
        if (steep) then
                tpx=x0
                x0=y0
                y0=tpx

                tpx=x1
                x1 =y1
                y1=tpx
        endif
        if (x0 > x1) then

                tpx=x0
                x0=x1
                x1=tpx

                tpx=y0
                y0 =y1
                y1=tpx

        endif

        dx=x1-x0
        dy=y1-y0
        gradient=dy/dx        

        !print *,'x0,y0,x1,y1,gradient',x0,y0,x1,y1,gradient

        !handle the first endpoint
        xend=x0
        intery=y0+gradient*(xend-x0)

        
        ! main loop
        IF (steep) Then
          Do x=x0, x1
              y=nint(intery)
  
              radical_value(x-x0+1,ri+1)=array(x,y)         
              radical_bt67(x-x0+1,ri+1)=array_bt67(x,y)         
              radical_bt11_bs(x-x0+1,ri+1)=array_bs(x,y)         

              intery=intery+gradient
              !print *,ri+1,x,y,radical_value(x-x0+1,ri+1)
           EndDo
        ELSE IF (.not. steep) Then

          Do x=x0, x1
              
              y=nint(intery)
              radical_value(x-x0+1,ri+1)=array(y,x)         
              radical_bt67(x-x0+1,ri+1)=array_bt67(y,x)         
              radical_bt11_bs(x-x0+1,ri+1)=array_bs(y,x)         

              intery=intery+gradient
              ! deal with vertical  
             
              !if (dx == 0.0) then
              !y=y0
              !radical_value(x,ri+1)=array(x,y)         
              !endif
              !print *,ri+1,x,y,radical_value(x-x0+1,ri+1)
           EndDo
        EndIF
        !==== if the array doesn't starting from center, reverse the
        !array
        if (x-1 .eq. y .and. y .eq. win_size+1 ) then
             radical_value(:,ri+1)=radical_value(x1-x0+1:1:-1,ri+1)
             radical_bt67(:,ri+1)=radical_bt67(x1-x0+1:1:-1,ri+1)
             radical_bt11_bs(:,ri+1)=radical_bt11_bs(x1-x0+1:1:-1,ri+1)
        endif
        !print *, radical_value(:,ri+1),x0,x1
        !print *, radical_bt67(:,ri+1),x0,x1
        !calculate the first and second derivative 

        first_deriv=0.0
        second_deriv=0.0

        call deriv(radical_value(:,ri+1),first_deriv) ! for derivative using smoothed data 
        call deriv(first_deriv,second_deriv)

         !open(1000,file='test_deriv.txt')
         !write(1000,*) radical_value(:,ri+1)
         !write(1000,*) first_deriv
         !write(1000,*) second_deriv

!         stop  
        ! search for zero values  
!        write(*,*) 'sign',sign(-1.0,second_deriv)
        !edge_loc=findloc(sign(-1.0,second_deriv),-1)
        call findvalue(second_deriv,0.0,edge_loc) 
        OT_edge(ri+1)=edge_loc
      
        ot_bt(ri+1) = sum(radical_bt11_bs(1:edge_loc,ri+1))!/edge_loc
        ot_bt67(ri+1) = sum(radical_bt67(1:edge_loc,ri+1))/edge_loc
        !==== calculate cirrus temperature ====
        !=== cirrus definition:1 bt <= 230 and btd >= -3,starting from
        !the edge of OT 
        !=== update cirrus BT<260,using data before smooth
        where((radical_bt11_bs(edge_loc+1:win_size+1,ri+1) <= &
               cirrus_threshold)) 
        !      (radical_bt11_bs(edge_loc+1:win_size+1,ri+1) > &
        !      ot_bt(ri+1)/edge_loc))
        !      (radical_bt67(edge_loc+1:win_size+1,ri+1)- &
        !       radical_value(edge_loc+1:win_size+1,ri+1) >= -3))

               tpbt11_ray_1(edge_loc+1:win_size+1)=&
               radical_bt11_bs(edge_loc+1:win_size+1,ri+1)
        endwhere
        
        cirrus_bt(ri+1)=sum(tpbt11_ray_1)/count((tpbt11_ray_1)>0)
        cirrus_bt_sum(ri+1)=sum(tpbt11_ray_1)!/count((tpbt11_ray_1)>0)
        ot_bt_var(ri+1) =&
        sum(radical_bt11_bs(1:edge_loc,ri+1)**2)
        ci_bt_var(ri+1)=sum(tpbt11_ray_1**2)
        ot_bt_var_num=ot_bt_var_num+(edge_loc+count(tpbt11_ray_1 > 0))

        if (count(tpbt11_ray_1 > 0) == 0) cirrus_bt(ri+1)=0.0
      
        !print *,ot_bt_var(ri+1), ot_bt_var_num
        !print *,radical_bt11_bs(1:edge_loc,ri+1)
        !print *,tpbt11_ray_1
        !print *, sum(radical_bt11_bs(1:edge_loc,ri+1)**2)
        !print *, sum(tpbt11_ray_1**2)
        !print *,(edge_loc+count(tpbt11_ray_1 > 0))

        !print *,ot_bt(ri+1),radical_bt11_bs(:,ri+1)
        !print *, ot_bt_var(ri+1)
        !print *, edge_loc
        !print *,radical_bt11_bs(:,ri+1)!(1:edge_loc,ri+1),edge_loc
       ! print *,radical_value(edge_loc:win_size+1,ri+1)
       ! print *,tpbt11_ray_1
        !print *,cirrus_bt(ri+1)
        !stop
        EndDo ! end each ray
        
!        close(1000)

!        Do ri=1,2*win_size+1 
!         write(*,*) array(ri,:)
!        EndDo
        !print *, radical_value

        !print *, array(:,33:41)
        !open(1000,file='output_bt.txt')

        !do ri=1,Nray
        !     write(1000,*) radical_value(:,ri)
        !enddo

        !close(1000)

        end subroutine array_vector_2

