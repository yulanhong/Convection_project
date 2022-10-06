
        program test

        real, dimension (8,2) :: &
        a1=reshape([1,2,3,4,5,6,7,8,11,12,13,14,15,16,17,18],[8,2])
      
        real :: tpa=12

        integer ::  scp1(2)

        scp1=minloc(abs(tpa-a1(3:7,:)))

        print *,shape(a1)
        print *,a1(3:7,:)
        print *,a1(3:7,:)
        print *,scp1,a1(scp1(1)+3,scp1(2))

        
        print*,sin(30*3.1415/180.0),cos(60*3.1415/180.0),tan(45*3.1415/180.0)
        end
