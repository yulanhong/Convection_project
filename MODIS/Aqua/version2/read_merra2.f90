
        subroutine read_merra2(fname)

                use netcdf
                use global, only : trop_temp,merra2_x,merra2_y,merra2_z

                implicit none

                character (len=200) :: fname

                integer :: ncid, varid,fid, sid

                integer :: ndims_in, nvars_in, ngatts_in, unlimdimid_in

                !print *,fname,shape(trop_temp)

                !fid=nf90_open(fname, NF90_NOWRITE, ncid)
                call check(nf90_open(fname, NF90_NOWRITE, ncid))
                !print *,ncid

                call check(nf90_inquire(ncid,ndims_in,nvars_in,&
                        ngatts_in,unlimdimid_in))

                !print *, ndims_in, nvars_in, ngatts_in,unlimdimid_in

                call check(nf90_inq_varid(ncid,"TROPT", varid))

                !print *,varid
                call check(nf90_get_var(ncid, varid, trop_temp))

                !print *, ncid,sid
                !print *,trop_temp(1,1,:)
        
                contains

                subroutine check(status)
                integer, intent ( in) :: status
    
                if(status /= nf90_noerr) then 
                print *, trim(nf90_strerror(status))
                stop "Stopped"
                end if
                end subroutine check  


        end
