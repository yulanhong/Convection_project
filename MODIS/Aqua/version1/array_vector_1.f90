
	subroutine array_vector_1(win_size,array,radical_value,&
                BT_candidate,cirrus_bt)

        integer :: x0,y0,x1,y1,x,y,tpx,tpy,ri
        logical :: steep
        real :: dy,dx, gradient

        real, parameter :: pi=3.1415927

        integer :: win_size

        real :: array(2*win_size+1,2*win_size+1)

        real :: radical_value(win_size+1,16)

        real :: BT_candidate,cirrus_bt(16)

        real :: angle

        !print *,shape(array)


        Do ri=0,15

        x0=win_size+1
        y0=win_size+1

        angle=22.5*ri
       ! print *,angle,(sin(angle*pi/180.0)),(cos(angle*pi/180.0))
        IF (cos(angle*pi/180.0) >=0.001) x1=x0+ceiling(((win_size)*cos(angle*pi/180.0)))
        IF (cos(angle*pi/180.0) > -0.001 .and. cos(angle*pi/180.0) < 0.001) x1=x0
        IF (cos(angle*pi/180.0) <=  -0.001) x1=x0+floor(((win_size)*cos(angle*pi/180.0)))
        IF (sin(angle*pi/180.0) >=0.001) y1=y0+ceiling(((win_size)*sin(angle*pi/180.0)))
        IF (sin(angle*pi/180.0) > -0.001 .and. sin(angle*pi/180.0) < 0.001) y1=y0
        IF (sin(angle*pi/180.0) <= -0.001) y1=y0+floor(((win_size)*sin(angle*pi/180.0)))


        ! deal with the loop (loop enough)
        steep=abs(y1-y0) > abs(x1-x0)
        !print *,steep

        if (steep) then
                tpx=x0
                x0=y0
                y0=tpx

                tpx=x1
                x1 =y1
                y1=tpx
        endif
       

        dx=x1-x0
        dy=y1-y0
        
        gradient=dy/dx
        !print *,'x0,x1,y0,y1,gradient',x0,x1,y0,y1,gradient

        IF (steep) then

           if (x0 > x1) then 
           Do x=x0,x1,-1
           y=y0+gradient*(x-x0)
           radical_value(x0-x+1,ri+1)=array(y,x)        
            !print *, x0-x+1,array(y,x)   
           EndDo
           endif

           if (x0 < x1) then 
           Do x=x0,x1
           y=y0+gradient*(x-x0)
           radical_value(x-x0+1,ri+1)=array(y,x)        
         !   print *, x-x0+1,array(y,x)   
           EndDo
           endif


        Else

        if (x0 > x1) then
        Do x=x0,x1,-1
            y=y0+gradient*(x-x0)
            radical_value(x0-x+1,ri+1)=array(x,y)    
           ! print *,x0-x+1, array(x,y)   
        EndDo
        endif

        if (x0 < x1) then
        Do x=x0,x1
            y=y0+gradient*(x-x0)    
            radical_value(x-x0+1,ri+1)=array(x,y)        
           ! print *,x-x0+1, array(x,y)   
        enddo
        endif
        
                                
        ! dealing with vertical 
        if (dx == 0.0) then
            if (y1 .gt. y0) then
            Do y=y0,y1
            x=x0
            radical_value(y-y0+1,ri+1)=array(x,y)        
           ! print *, y-y0+1,array(x,y)   
            enddo
            endif

            if (y1 .lt. y0) then
            Do y=y0,y1,-1
            x=x0
            radical_value(y-y1+1,ri+1)=array(x,y)        
           ! print *, y-y1+1,array(x,y)   
            enddo
               
        
            endif
        endif

        EndIf

        EndDo ! end 16 radical
       !   do i=1,15
       !         write(*,'(15(x,f9.3))') array(i,:)
       !   enddo

       ! stop
	end
