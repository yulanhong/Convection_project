        ! input array -- one dimension array to search 
        !       value -- the value to be search
        ! return -- return the location of the value

        subroutine findvalue(array,fvalue,edge_loc)
                
        use global, only : win_size

        implicit none

        real :: array(win_size+1),fvalue 

        integer :: i
        integer :: edge_loc
        
        real, parameter :: epsi=0.05

        Do i=2, win_size-1

               !==only when it is a quick drop of the second derivative,
               !it readches to reflection point ====

               if (array(i) > fvalue .and. &
                 array(i+1) <= fvalue-epsi) then         
                   exit 
                endif

                !=== modorlate flat
                if (array(i) > fvalue .and. &
                    array(i+1) < fvalue .and. &
                    array(i+1) >= fvalue-epsi .and. &
                    array(i+2) < fvalue-epsi) exit

               ! == or if at least three continuous points < 0
               ! fvalue-epsi, exit, very flat curve
               if ( array(i) < fvalue .and. &
                    array(i) >= fvalue-epsi .and. &
                    array(i+1) < fvalue .and. &
                   array(i+1) >= fvalue-epsi .and. &
                   array(i+2) < fvalue .and. &
                   array(i+2) >= fvalue-epsi) exit              

               ! 
        EndDo
        !print *,i
        edge_loc=i
 
        end
