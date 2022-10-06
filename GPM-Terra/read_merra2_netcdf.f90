
        subroutine read_merra2_netcdf(fname)

                use netcdf
                use global, only : trop_temp,trop_pres,&
                        surf_temp,surf_pres

                implicit none

                character (len=200) :: fname

                integer :: ncid, varid,fid, sid

                integer :: ndims_in, nvars_in, ngatts_in, unlimdimid_in


               ! fid=nf90_open(fname, NF90_NOWRITE, ncid)
                call check(nf90_open(fname, NF90_NOWRITE, ncid))


                call check(nf90_inq_varid(ncid,"TROPT", varid))
                call check(nf90_get_var(ncid, varid, trop_temp))

                call check(nf90_inq_varid(ncid,"TROPPB", varid))
                call check(nf90_get_var(ncid, varid, trop_pres))

                call check(nf90_inq_varid(ncid,"PS", varid))
                call check(nf90_get_var(ncid, varid, surf_pres))

                call check(nf90_inq_varid(ncid,"TS", varid))
                call check(nf90_get_var(ncid, varid, surf_temp))

                call check(nf90_close(ncid))
               ! print *, ncid,sid
               ! print *,trop_temp(570,360,:)

        
                contains

                subroutine check(status)
                integer, intent ( in) :: status
    
                if(status /= nf90_noerr) then 
                print *, trim(nf90_strerror(status))
                stop "Stopped"
                 end if
                end subroutine check  


        end
