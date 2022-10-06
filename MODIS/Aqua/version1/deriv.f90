        subroutine deriv(y,first_deriv)
        ! x -- the abscissas 
        ! y -- the array needs to be interpolated 
        ! first_deriv, same size array as x and y, to save the first
        ! derive
        ! secod_deriv, same size array as x and y, to save the second
        ! derive
        use global, only : win_size

        implicit none

        integer :: nx, i

        real :: first_deriv(win_size+1) 
        real :: y(win_size+1)
        real :: deriv3        

        nx=win_size+1
        first_deriv=0.0
        ! only dealing with evenly spacing array

        first_deriv=(cshift(y,1)-cshift(y,-1))/2.0
        first_deriv(1)=(-3.0*y(1)+4.0*y(2)-y(3))/2.0
        first_deriv(nx)=(3.0*y(nx)-4.0*y(nx-1)+y(nx-2))/2.0

        end subroutine deriv 


