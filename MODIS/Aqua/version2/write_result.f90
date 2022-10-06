      subroutine write_result(wfname)

        use global, only: dimx,dimy, total_obsnumh,total_bt180_numh,&
        total_bt200_numh,total_bt210_numh,total_bt220_numh,&
        total_bt230_numh,total_bt240_numh,total_cdtp_numh,&
        total_cdtp2_numh,total_cdtp5_numh,total_cdtp10_numh

        implicit none

         integer sfstart,sfcreate, sfsnatt, sfwdata, sfendacc, sfend, &
            sfsattr

        character(len=200) :: wfname
        character(len=50) :: SDS_NAME

        integer  ::  RANK
        integer :: parameter,DFACC_CREATE=4,DFNT_INT16=22,&
          DFNT_INT32=24,&
          DFNT_FLOAT32=5,DFNT_CHAR8=4,DFNT_FLOAT=5,DFNT_INT8=21
        integer :: fid, sds_id, status
        integer :: start ( 4 ), edges ( 4 ), stride ( 4 ),dim_sizes(4)
        integer :: i, j,k

        fid = sfstart( wfname, DFACC_CREATE )
        IF (fid==-1) then
             print *,'process stop at writing file: ',wfname
            stop
        ENDIF

         RANK=2
         dim_sizes(1)=dimx
         dim_sizes(2)=dimy
         edges(1)=dimx
         edges(2)=dimy
         start(1)=0
         start(2)=0
         stride(1)=1
         stride(2)=1


        SDS_NAME='obs_num'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges,total_obsnumh)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,120,&
             'obsnum')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,4,' ')
        status = sfendacc ( sds_id )

        SDS_NAME='num_bt180'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges,total_bt180_numh)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,120,&
             'number of pixels with bt colder than 180')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,4,' ')
        status = sfendacc ( sds_id )

        SDS_NAME='num_bt200'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges,total_bt200_numh)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,120,&
             'number of pixels with bt colder than 200')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,4,' ')
        status = sfendacc ( sds_id )

        SDS_NAME='num_bt210'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges,total_bt210_numh)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,120,&
             'number of pixels with bt colder than 210')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,4,' ')
        status = sfendacc ( sds_id )

        SDS_NAME='num_bt220'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges,total_bt220_numh)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,120,&
             'number of pixels with bt colder than 220')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,4,' ')
        status = sfendacc ( sds_id )

        SDS_NAME='num_bt230'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges,total_bt230_numh)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,120,&
             'number of pixels with bt colder than 230')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,4,' ')
        status = sfendacc ( sds_id )

        SDS_NAME='num_bt240'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges,total_bt240_numh)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,120,&
             'number of pixels with bt colder than 240')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,4,' ')
        status = sfendacc ( sds_id )

        SDS_NAME='num_colder_tropopause'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges,total_cdtp_numh)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,120,&
             'number of pixels with bt colder than tropopause')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,4,' ')
        status = sfendacc ( sds_id )

        SDS_NAME='num_colder_tropopause2'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges,total_cdtp2_numh)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,120,&
             'number of pixels with bt colder than tropopause-2')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,4,' ')
        status = sfendacc ( sds_id )

        SDS_NAME='num_colder_tropopause5'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges,total_cdtp5_numh)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,120,&
             'number of pixels with bt colder than tropopause-5')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,4,' ')
        status = sfendacc ( sds_id )

        SDS_NAME='num_colder_tropopause10'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges,total_cdtp10_numh)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,120,&
             'number of pixels with bt colder than tropopause-10')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,4,' ')
        status = sfendacc ( sds_id )



        status = sfend ( fid )

      end
