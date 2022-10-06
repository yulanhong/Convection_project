
        !https://en.wikipedia.org/wiki/Xiaolin_Wu%27s_line_algorithm
        !Xiaolin Wu's line algorithm

	subroutine get_storm_array(array,array_bt67,colj,rowi,storm_edge,storm_bt)

        use global, only : Nray,modNrow,modNcol,storm_threshold,&
        cirrus_btd_threshold

        implicit none

        integer :: x0,y0,x1,y1,x,y
        integer :: tpx,tpy,ri,wi
        real :: xpx11,ypx11,xpx12,ypx12,xend,intery
        logical :: steep
        real :: dy,dx, gradient
        real :: angle
        integer :: colj,rowi

        real :: array(modNcol,modNrow),array_bt67(modNcol,modNrow)

        real :: storm_bt(Nray),storm_edge(Nray)
        integer :: edge_loc,ci_loc
        integer :: mark_ci(500) ! to record the location of ci      

        real, parameter :: pi=3.1415927
        integer, parameter :: rad_size=500

        real :: radical_value(rad_size,Nray),tpbt11_ray_1(rad_size)
        real :: radical_bt67(rad_size,Nray)
        real :: bdt_6711


        storm_bt=0.0
        storm_edge=0.0
        radical_value=0.0
        radical_bt67=0.0

        Do ri=0,Nray-1

        tpbt11_ray_1=0.0
        x0=colj
        y0=rowi

        angle=ri*360.0/Nray

        !print *,angle,(sin(angle*pi/180.0)),(cos(angle*pi/180.0))
        IF (cos(angle*pi/180.0) >=0.001) x1=x0+ceiling(((rad_size)*cos(angle*pi/180.0)))
        IF (cos(angle*pi/180.0) > -0.001 .and. cos(angle*pi/180.0) < 0.001) x1=x0
        IF (cos(angle*pi/180.0) <=  -0.001) x1=x0+floor(((rad_size)*cos(angle*pi/180.0)))
        IF (sin(angle*pi/180.0) >=0.001) y1=y0+ceiling(((rad_size)*sin(angle*pi/180.0)))
        IF (sin(angle*pi/180.0) > -0.001 .and. sin(angle*pi/180.0) < 0.001) y1=y0
        IF (sin(angle*pi/180.0) <= -0.001) y1=y0+floor(((rad_size)*sin(angle*pi/180.0)))

        steep=abs(y1-y0) > abs(x1-x0)
        !print *,'x0,y0,x1,y1,steep',x0,y0,x1,y1,steep

        if (x1 < 1) x1=1
        if (y1 < 1) y1=1 
        if (y1 > modNrow) y1=modNrow
        if (x1 > modNcol) x1=modNcol
      
        if (steep) then
                tpx=x0
                x0=y0
                y0=tpx

                tpx=x1
                x1 =y1
                y1=tpx

                if (x1 > modNrow) x1=modNrow
                if (y1 > modNcol) y1=modNcol
                if (x0 > modNrow) x0=modNrow
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
          Do x=x0, x1-1
              y=nint(intery)
  
              radical_value(x-x0+1,ri+1)=array(y,x)         
              radical_bt67(x-x0+1,ri+1)=array_bt67(y,x)         

              intery=intery+gradient
              !print *,ri+1,x,y,x0,x1,x-x0+1,radical_value(x-x0+1,ri+1)
           EndDo
        ELSE IF (.not. steep) Then

          Do x=x0, x1-1
              
              !print *,ri+1,x,y,x0,x1,x-x0+1,radical_value(x-x0+1,ri+1)
              y=nint(intery)
              radical_value(x-x0+1,ri+1)=array(x,y)         
              radical_bt67(x-x0+1,ri+1)=array_bt67(x,y)         
              intery=intery+gradient
              ! deal with vertical  
             
              !if (dx == 0.0) then
              !y=y0
              !radical_value(x,ri+1)=array(x,y)         
              !endif
             
           EndDo
        EndIF
        !==== if the array doesn't starting from center(coj,rowi), reverse the
        !array
        !print *, colj,rowi,x-1,y,x1-x0+1
        if (x-1 == rowi .and. y == colj ) then
             radical_value(:,ri+1)=radical_value(x1-x0+1:1:-1,ri+1)
             radical_bt67(:,ri+1)=radical_bt67(x1-x0+1:1:-1,ri+1)
        endif
        if (x-1 == colj .and. y == rowi ) then
             radical_value(1:x1-x0+1,ri+1)=radical_value(x1-x0+1:1:-1,ri+1)
             radical_bt67(1:x1-x0+1,ri+1)=radical_bt67(x1-x0+1:1:-1,ri+1)
        endif
       
        !==== to calculate the radius of storm
        where((radical_value(:,ri+1) <= storm_threshold) .and. &
              ((radical_bt67(:,ri+1)- &
               radical_value(:,ri+1)) >= -3))
        
                tpbt11_ray_1=radical_value(:,ri+1)
        endwhere

        !print *,tpbt11_ray_1
        !print *,radical_value(:,ri+1)
        !print *,radical_bt67(:,ri+1)-radical_value(:,ri+1)
        !=== to get the first value of tpbt11_ray_1 lt 0 as the edge of
        !=== storm ======
        Do wi=2, rad_size-1 
           if (tpbt11_ray_1(wi) == 0) exit 
        EndDo

        storm_edge(ri+1)=wi-1
        storm_bt(ri+1)=sum(tpbt11_ray_1(1:wi-1))/(wi-1)
       ! print *,wi,storm_bt(ri+1)
       
        EndDo ! end each ray
!        close(1000)
        
       
        !open(1000,file='output_bt.txt')
       do ri=1,Nray
        write(*,*) radical_value(:,ri)   
        enddo
        stop
        print *, radical_value(:,1)
        print *, radical_value(:,8)
        stop
        !close(1000)
        !print *,storm_edge
        !print *,storm_bt 
        !stop
        end subroutine get_storm_array

