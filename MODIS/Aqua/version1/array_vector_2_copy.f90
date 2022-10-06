
        !https://en.wikipedia.org/wiki/Xiaolin_Wu%27s_line_algorithm
        !Xiaolin Wu's line algorithm

	program array_vector_2

        integer :: x0,y0,x1,y1,x,y
        integer :: tpx,tpy,ri
        real :: xpx11,ypx11,xpx12,ypx12,xend,yend,intery
        logical :: steep
        real :: dy,dx, gradient,dx_mark

        integer, parameter :: win_size=2,Nray=16
        real, parameter :: pi=3.1415927

        real :: array(2*win_size+1,2*win_size+1)

        real :: radical_value(win_size+1,16)

        real :: angle

             array(1,:)=(/1.0,2.3,3.0,4.0,2.0/)
             array(2,:)=(/2.1,4.2,5.0,0.0,9.2/)
             array(3,:)=(/7.0,8.0,10.0,2.9,3.4/)
             array(4,:)=(/6.0,4.7,9.5,10.4,2.8/)
             array(5,:)=(/1.6,3.5,6.0,15.0,8.5/)



        Do ri=0,Nray-1

        x0=win_size+1
        y0=win_size+1

        angle=360.0*ri/Nray
        print *,angle,(sin(angle*pi/180.0)),(cos(angle*pi/180.0))
        IF (cos(angle*pi/180.0) >=0.001) x1=x0+ceiling(((win_size)*cos(angle*pi/180.0)))
        IF (cos(angle*pi/180.0) > -0.001 .and. cos(angle*pi/180.0) < 0.001) x1=x0
        IF (cos(angle*pi/180.0) <=  -0.001) x1=x0+floor(((win_size)*cos(angle*pi/180.0)))
        IF (sin(angle*pi/180.0) >=0.001) y1=y0+ceiling(((win_size)*sin(angle*pi/180.0)))
        IF (sin(angle*pi/180.0) > -0.001 .and. sin(angle*pi/180.0) < 0.001) y1=y0
        IF (sin(angle*pi/180.0) <= -0.001) y1=y0+floor(((win_size)*sin(angle*pi/180.0)))

        steep=abs(y1-y0) > abs(x1-x0)
!        steep=y0 > y1
        print *,'x0,y0,x1,y1,steep',x0,y0,x1,y1,steep
      
!        dx_mark=1.0
       ! IF (x0 == x1) Then
       !         tpx=x0
       !         x0=y0
       !         y0=tpx
       !         tpx=x1
       !         x1 =y1
       !         y1=tpx
       !         dx_mark=0.0
       ! EndIf
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

        print *,'x0,y0,x1,y1,gradient',x0,y0,x1,y1,gradient

        !handle the first endpoint
        xend=x0
        intery=y0+gradient*(xend-x0)
!       xpx11=xend
!       intery=yend

        !handle the second endpoint
        !xend=x1
        !yend=y1+gradient*(xend-x1)
        !xpx12=xend
        !ypx12=ipart(yend)
        
        ! main loop
        IF (steep) Then
          Do x=x0, x1
              y=floor(intery)
              radical_value(x,ri+1)=array(x,y)         
              intery=intery+gradient
              print *,x,y,radical_value(x,ri+1)
           EndDo
        ELSE IF (.not. steep) Then

          Do x=x0, x1
              y=floor(intery)
              radical_value(x,ri+1)=array(y,x)         
              intery=intery+gradient
              ! deal with vertical  
             
              if (dx == 0.0) then
              y=y0
              radical_value(x,ri+1)=array(x,y)         
              endif
              print *,dx,x,y,radical_value(x,ri+1)
           EndDo
        EndIF
        EndDo ! end each ray

          do i=1,5 
                print *,array(i,:)   
          enddo
        end program array_vector_2

        !integer function ipart(x)
         ! implicit none
         ! real :: x
         ! ipart=floor(x)      
        !end function ipart

        !integer function iround(x)
        !  implicit none
        !  real :: x
        !  iround= ipart(x+0.5)
        !end function iround

        !integer function fpart(x)
        !  implicit none
        !  real :: x
         ! fpart= x-floor(x)
        !end function fpart

        !integer function rfpart(x)
        !  implicit none
        !  real :: x
        !  rfpart= 1-fpart(x)
       ! end function rfpart

       ! IF (steep) then

!           if (x0 < x1) then 
!           Do x=x0,x1
!           y=y0+gradient*(x-x0)
!           radical_value(x-x0+1,ri+1)=array(y,x)        
!            print *, array(y,x)   
!           EndDo
!           endif
!
!           if (x0 > x1) then 
!           Do x=x0,x1,-1
!           y=y0+gradient*(x-x0)
!           radical_value(x+1,ri+1)=array(y,x)        
!            print *, array(y,x)   
!           EndDo
!           endif
!
!        Else
!
!        if (x0 > x1) then
!        Do x=x0,x1,-1
!            y=y0+gradient*(x-x0)
!            radical_value(x,ri+1)=array(x,y)    
!            print *, array(x,y)   
!        EndDo
!        endif
!
!        if (x0 < x1) then
!        Do x=x0,x1
!            y=y0+gradient*(x-x0)    
!            radical_value(x-x0+1,ri+1)=array(x,y)        
!            print *, array(x,y)   
!        enddo
!        endif
!                
!
!        if (dx == 0.0) then
!            if (y1 .gt. y0) then
!            Do y=y0,y1
!            x=x0
!            radical_value(y-y0+1,ri+1)=array(x,y)        
!            print *, array(x,y)   
!            enddo
!            endif
!
!            if (y1 .lt. y0) then
!            Do y=y0,y1,-1
!            x=x0
!            radical_value(y-y1+1,ri+1)=array(x,y)        
!            print *, array(x,y)   
!            enddo
!               
!        
!            endif
!        endif
!
!        EndIf
!
!        EndDo ! end 16 radical
!          do i=1,5 
!                print *,array(i,:)   
!          enddo
