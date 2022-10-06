
        !https://en.wikipedia.org/wiki/Xiaolin_Wu%27s_line_algorithm
        !Xiaolin Wu's line algorithm

	subroutine array_vector_2(array,BT_candidate,cirrus_bt)

        use global, only : cirrus_threshold,win_size,Nray

        integer :: x0,y0,x1,y1,x,y
        integer :: tpx,tpy,ri
        real :: xpx11,ypx11,xpx12,ypx12,xend,intery
        logical :: steep
        real :: dy,dx, gradient

        real :: array(2*win_size+1,2*win_size+1)

        real :: BT_candidate,cirrus_bt(Nray)

        real, parameter :: pi=3.1415927

        real :: radical_value(win_size+1,Nray),tpbt11_ray_1(win_size+1)

        real :: angle


        cirrus_bt=0.0

        Do ri=0,Nray-1

        tpbt11_ray_1=0.0

        x0=win_size+1
        y0=win_size+1

        angle=ri*360.0/Nray

        !print *,angle,(sin(angle*pi/180.0)),(cos(angle*pi/180.0))
        IF (cos(angle*pi/180.0) >=0.001) x1=x0+ceiling(((win_size)*cos(angle*pi/180.0)))
        IF (cos(angle*pi/180.0) > -0.001 .and. cos(angle*pi/180.0) < 0.001) x1=x0
        IF (cos(angle*pi/180.0) <=  -0.001) x1=x0+floor(((win_size)*cos(angle*pi/180.0)))
        IF (sin(angle*pi/180.0) >=0.001) y1=y0+ceiling(((win_size)*sin(angle*pi/180.0)))
        IF (sin(angle*pi/180.0) > -0.001 .and. sin(angle*pi/180.0) < 0.001) y1=y0
        IF (sin(angle*pi/180.0) <= -0.001) y1=y0+floor(((win_size)*sin(angle*pi/180.0)))

        steep=abs(y1-y0) > abs(x1-x0)
        !print *,'x0,y0,x1,y1,steep',x0,y0,x1,y1,steep
      
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
              intery=intery+gradient
              !print *,x,y,radical_value(x-x0+1,ri+1)
           EndDo
        ELSE IF (.not. steep) Then

          Do x=x0, x1
              
              y=nint(intery)
              radical_value(x-x0+1,ri+1)=array(y,x)         
              intery=intery+gradient
              ! deal with vertical  
             
              !if (dx == 0.0) then
              !y=y0
              !radical_value(x,ri+1)=array(x,y)         
              !endif
       !       print *,ri+1,x,y,radical_value(x-x0+1,ri+1)
           EndDo
        EndIF

        !==== calculate cirrus temperature ====
         where(radical_value(:,ri+1) > BT_candidate .and. &
         radical_value(:,ri+1) <= cirrus_threshold)
                tpbt11_ray_1=radical_value(:,ri+1)
         endwhere

         cirrus_bt(ri+1)=sum(tpbt11_ray_1)/count((tpbt11_ray_1)>0)

        
        EndDo ! end each ray
        !open(1000,file='output_bt.txt')
        !  do i=1,16
         !       write(1000,'(15(x,f9.3))') array(i,:)   
         ! enddo
       ! close(1000)
       ! stop
        end subroutine array_vector_2

