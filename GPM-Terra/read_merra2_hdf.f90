
	subroutine read_merra2_hdf(fname)
        
         use global, only : trop_temp,trop_pres,surf_temp,surf_pres,&
                merra2_x,merra2_y,merra2_z

         implicit none

         integer :: fid,sds, sid,attr_id,ranki, DFACC_READ=1
         integer :: rank, data_type, n_attrs
         integer :: dim_sizes(32), start(32), edges(32), stride(32)
         integer :: sfstart,sfselect,sfrdata,sfendacc,sfend,sffattr,&
                  sfrattr,  err_code, sfn2index
         integer :: status
         integer :: sffinfo , sfginfo


         !==================================================
         character (len=200) :: fname

         fid=sfstart(fname,DFACC_READ)

         sid=sfn2index(fid,'TROPT')
         sds=sfselect(fid,sid)

         !status=sfginfo(sds,'TROPT',rank,dim_sizes,data_type,n_attrs)
         start(1:3)=0
         edges(1)= merra2_x
         edges(2)= merra2_y
         edges(3)= merra2_z
         stride(1:3)=1

         status=sfrdata(sds,start,stride,edges,trop_temp)
         status=sfendacc(sds)

         sid=sfn2index(fid,'TROPPB')
         sds=sfselect(fid,sid)
         status=sfrdata(sds,start,stride,edges,trop_pres)
         status=sfendacc(sds)

         sid=sfn2index(fid,'PS')
         sds=sfselect(fid,sid)
         status=sfrdata(sds,start,stride,edges,surf_pres)
         status=sfendacc(sds)

         sid=sfn2index(fid,'TS')
         sds=sfselect(fid,sid)
         status=sfrdata(sds,start,stride,edges,surf_temp)
         status=sfendacc(sds)

         status=sfend(fid)

        !print *,trop_temp(1,1,1)
        !print *,trop_pres(1,1,1)
        !print *,surf_pres(1,1,1)
        !print *,surf_temp(1,1,1)

        end

