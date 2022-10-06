        subroutine deriv(y,first_deriv,second_deriv)
        ! x -- the abscissas 
        ! y -- the array needs to be interpolated 
        ! first_deriv, same size array as x and y, to save the first
        ! derive
        ! secod_deriv, same size array as x and y, to save the second
        ! derive
        use global, only : win_size

        integer :: nx, i

        real :: first_deriv, second_deriv
        real :: x(win_size+1)
        
        nx=win_size+1

        Do i=1, nx
           x(i)=i*1.0
        EndDo

        Do i=1, nx 
            first_deriv(i)=deriv3(x(i),x,y,nx,1)
            second_deriv(i)=deriv3(x(i),x,y,nx,2)
        EndDo 
        
        end subroutine deriv 


        function deriv3(xx, xi, yi, ni, m)
!====================================================================
! Evaluate first- or second-order derivatives 
! using three-point Lagrange interpolation 
! written by: Alex Godunov (October 2009)
!--------------------------------------------------------------------
! input ...
! xx    - the abscissa at which the interpolation is to be evaluated
! xi()  - the arrays of data abscissas
! yi()  - the arrays of data ordinates
! ni - size of the arrays xi() and yi()
! m  - order of a derivative (1 or 2)
! output ...
! deriv3  - interpolated value
!============================================================================*/

        implicit none
        integer, parameter :: n=3
        double precision deriv3, xx
        integer ni, m
        double precision xi(ni), yi(ni)
        double precision x(n), f(n)
        integer i, j, k, ix

! exit if too high-order derivative was needed,
        if (m > 2) then
        deriv3 = 0.0
        return
        end if

! if x is ouside the xi(1)-xi(ni) interval set deriv3=0.0
        if (xx < xi(1) .or. xx > xi(ni)) then
        deriv3 = 0.0
        return
        end if

! a binary (bisectional) search to find i so that xi(i-1) < x < xi(i)
        i = 1
        j = ni
        do while (j > i+1)
        k = (i+j)/2
        if (xx < xi(k)) then
        j = k
        else
        i = k
        end if
        end do

! shift i that will correspond to n-th order of interpolation
! the search point will be in the middle in x_i, x_i+1, x_i+2 ...
        i = i + 1 - n/2

! check boundaries: if i is ouside of the range [1, ... n] -> shift i
        if (i < 1) i=1
        if (i + n > ni) i=ni-n+1

!  old output to test i
!  write(*,100) xx, i
!  100 format (f10.5, I5)

! just wanted to use index i
        ix = i

! initialization of f(n) and x(n)
        do i=1,n
        f(i) = yi(ix+i-1)
        x(i) = xi(ix+i-1)
        end do

! calculate the first-order derivative using Lagrange interpolation
        if (m == 1) then
        deriv3 =  2.0*xx -&
                (x(2)+x(3)))*f(1)/((x(1)-x(2))*(x(1)-x(3)))
        deriv3 = deriv3 + (2.0*xx -&
                (x(1)+x(3)))*f(2)/((x(2)-x(1))*(x(2)-x(3)))
        deriv3 = deriv3 + (2.0*xx -&
                (x(1)+x(2)))*f(3)/((x(3)-x(1))*(x(3)-x(2)))
! calculate the second-order derivative using Lagrange interpolation
          else
        deriv3 =          2.0*f(1)/((x(1)-x(2))*(x(1)-x(3)))
        deriv3 = deriv3 + 2.0*f(2)/((x(2)-x(1))*(x(2)-x(3)))
        deriv3 = deriv3 + 2.0*f(3)/((x(3)-x(1))*(x(3)-x(2)))
        end if
        end function deriv3

