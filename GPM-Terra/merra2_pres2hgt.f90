
        subroutine merra2_pres2hgt

        use global, only : trop_pres, trop_temp, surf_temp,&
                surf_pres,trop_hgt,merra2_x,merra2_y,merra2_z

        implicit none

        real, parameter :: R=8.3143, M=0.02896, g=9.807,&
                        Lb=-0.0065

        real :: C1
        real, dimension(merra2_x,merra2_y,merra2_z) ::  C2, C3

       
        C1=0-R*Lb/(g*M)

        C2=trop_pres/surf_pres 
        
        C3=C2**C1

        trop_hgt=(surf_temp*(C3-1)/Lb)/1000.0

        !print *,trop_hgt(1,1,1)

        end
