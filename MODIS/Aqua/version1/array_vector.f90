
	program array_vector

        integer :: x0,y0,x1,y1,x,y,tpx,tpy,ri
        logical :: steep
        real :: dy,dx, gradient

        integer, parameter :: win_size=2
        real, parameter :: pi=3.1415927

        real :: array(2*win_size+1,2*win_size+1)

        real :: radical_value(win_size+1,16)

        real :: angle

             array(1,:)=(/1.0,2.3,3.0,4.0,2.0/)
             array(2,:)=(/2.1,4.2,5.0,0.0,9.2/)
             array(3,:)=(/7.0,8.0,10.0,2.9,3.4/)
             array(4,:)=(/6.0,4.7,9.5,10.4,2.8/)
             array(5,:)=(/1.6,3.5,6.0,15.0,8.5/)


        x0=win_size+1
        y0=win_size+1

        Do ri=11,15

        angle=22.5*ri
        print *,angle,(sin(angle*pi/180.0)),(cos(angle*pi/180.0))
        IF (cos(angle*pi/180.0) >=0.001) x1=x0+ceiling(((win_size)*cos(angle*pi/180.0)))
        IF (cos(angle*pi/180.0) > -0.001 .and. cos(angle*pi/180.0) < 0.001) x1=x0
        IF (cos(angle*pi/180.0) <=  -0.001) x1=x0+floor(((win_size)*cos(angle*pi/180.0)))
        IF (sin(angle*pi/180.0) >=0.001) y1=y0+ceiling(((win_size)*sin(angle*pi/180.0)))
        IF (sin(angle*pi/180.0) > -0.001 .and. sin(angle*pi/180.0) < 0.001) y1=y0
        IF (sin(angle*pi/180.0) <= -0.001) y1=y0+floor(((win_size)*sin(angle*pi/180.0)))

        steep=abs(y1-y0) > abs(x1-x0)
        print *,steep

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
        print *,'x0,x1,y0,y1,gradient',x0,x1,y0,y1,gradient

        IF (steep) then

           if (x0 < x1) then 
           Do x=x0,x1
           y=y0+gradient*(x-x0)
           radical_value(x-x0+1,ri+1)=array(y,x)        
            print *, array(y,x)   
           EndDo
           endif

           if (x0 > x1) then 
           Do x=x0,x1,-1
           y=y0+gradient*(x-x0)
           radical_value(x+1,ri+1)=array(y,x)        
            print *, array(y,x)   
           EndDo
           endif

        Else

        if (x0 > x1) then
        Do x=x0,x1,-1
            y=y0+gradient*(x-x0)
            radical_value(x,ri+1)=array(x,y)    
            print *, array(x,y)   
        EndDo
        endif

        if (x0 < x1) then
        Do x=x0,x1
            y=y0+gradient*(x-x0)    
            radical_value(x-x0+1,ri+1)=array(x,y)        
            print *, array(x,y)   
        enddo
        endif
                

        if (dx == 0.0) then
            if (y1 .gt. y0) then
            Do y=y0,y1
            x=x0
            radical_value(y-y0+1,ri+1)=array(x,y)        
            print *, array(x,y)   
            enddo
            endif

            if (y1 .lt. y0) then
            Do y=y0,y1,-1
            x=x0
            radical_value(y-y1+1,ri+1)=array(x,y)        
            print *, array(x,y)   
            enddo
               
        
            endif
        endif

        EndIf

        EndDo ! end 16 radical
          do i=1,5 
                print *,array(i,:)   
          enddo

	end
