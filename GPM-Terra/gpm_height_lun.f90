
        !=== to obtain the range at different scan angle
        subroutine gpm_height_lun

        use global, only : range_height
        implicit none        

        real, parameter :: x2=2*17.04, re=6378.0, pi=3.1415927
        real(kind=4) ,dimension (49) :: theta


        integer :: i,j
        real (kind=4) :: a

        Do i=0,48 
        theta(i+1)=((x2/48.0)*i-x2/2)*pi/180.0
        EndDo


        Do j=1, 49
            Do i=1, 176
                a=asin(((re+407.0)/re)*sin(theta(j)))-theta(j)

                range_height(i,j)=(176-i+1)*0.125*cos(theta(j)+a)
   
            EndDo
        EnDDo


        end
