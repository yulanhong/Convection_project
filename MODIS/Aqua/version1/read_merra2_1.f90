
                program read_merra2_1

                use netcdf

                implicit none

                character (len=200) :: fname


                integer :: ncid, varid,fid, sid

                real :: trop_temp(576,361,24)

!                integer :: ndims_in, nvars_in, ngatts_in, unlimdimid_in


                fname='/data/keeling/a/yulanh/c/MERRA-2/2018/'&
                        //'MERRA2_400.inst1_2d_asm_Nx.20180101.nc4'

                !print *,fname

               ! fid=nf90_open(fname, NF90_NOWRITE, ncid)
                call check(nf90_open(fname, NF90_NOWRITE, ncid))


               ! call check(nf90_inquire(ncid,ndims_in,nvars_in,&
                !        ngatts_in,unlimdimid_in))

                !print *, ndims_in, nvars_in, ngatts_in,unlimdimid_in
            !    if ((ndims_in .ne. 2) .or. (nva;rs_in .ne. 4) .or. &
             !           (ngatts_in .ne. 0) &
             !           .or. (unlimdimid_in .ne. -1)) then
              !          print *,'merra2 read fail', fname
              !          stop 
              !  endif
                

                call check(nf90_inq_varid(ncid,"TROPT", varid))

                !print *,varid
                call check(nf90_get_var(ncid, varid, trop_temp))

               ! print *, ncid,sid
                print *,trop_temp(1,1,:)
        
                contains

                subroutine check(status)
                integer, intent ( in) :: status
    
                if(status /= nf90_noerr) then 
                print *, trim(nf90_strerror(status))
                stop "Stopped"
                 end if
                end subroutine check  


        end
