
        program test_fortran_shipt


                real :: a(5)
        
                 a=(/1.0,2.0,3.0,4.0,5.0/)

                print *,a
                print *,'shipt -1 a',cshift(a,-1)
                print *,'shift 1 a',cshift(a,1)



        end
