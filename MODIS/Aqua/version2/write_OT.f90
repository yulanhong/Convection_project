
        subroutine write_OT(FILE_NAME)

        use global, only : modNrow, modNcol, OT_flag,cirrus_radii_num,& ! OT_BT, OT_BTD,&
                mean_cirrus_bt,OT_BTD_trop,OT_area,mean_OT_BT,&
                storm_area,mean_storm_bt,BT11,BT67,OT_proba

        implicit none

        integer sfstart,sfcreate, sfsnatt, sfwdata, sfendacc, sfend, &
                sfsattr
        !**** Variable declaration *******************************************
        character(len=200) ::  FILE_NAME
        character(len=20) :: SDS_NAME

        integer :: RANK
        integer :: parameter,DFACC_CREATE=4,DFNT_INT16=22,DFNT_INT32=24,&
            DFNT_FLOAT32=5,DFNT_CHAR8=4,DFNT_FLOAT=5,DFNT_INT8=21
        integer :: fid, sds_id, status
        integer :: start ( 3 ), edges ( 3 ), stride ( 3),dim_sizes(3)
        integer :: i, j

        fid = sfstart( trim(FILE_NAME), DFACC_CREATE )
        IF (fid==-1) then
                print *,'process stop at writing file: ',FILE_NAME
                stop
        ENDIF

        RANK=2
        start(1)=0
        start(2)=0
        stride(1:2)=1
        dim_sizes(1)=modNcol
        dim_sizes(2)=modNrow
        edges(1)=modNcol
        edges(2)=modNrow

        print *,sum(OT_flag)

        SDS_name='OT_Flag'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT8,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges, OT_flag)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
                '1 indicates OT pixel center')
        status = sfendacc (sds_id)


        SDS_name='cirrus_rays'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT8,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges, cirrus_radii_num)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
                'the radii contain valid cirrus bt')
        status = sfendacc (sds_id)

        SDS_name='OT_Probability'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges, OT_proba)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
                'probability of OT')
        status = sfendacc(sds_id)


        SDS_name='BT11'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges, BT11)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
                'BT11')
        status = sfendacc(sds_id)

        SDS_name='BT6.7'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
status=sfwdata(sds_id, start, stride, edges, BT67)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
                'BT6.7')
        status = sfendacc(sds_id)


        SDS_name='Tropopause_T'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges, OT_BTD_trop)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
                'tropopause temp')
        status = sfendacc(sds_id)
       
         !SDS_name='OT_BTD'
        !sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        !status=sfwdata(sds_id, start, stride, edges, OT_BTD)
        !status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
        !        'OT BTD between 6.7 and 11um for center pixel')
        !status = sfendacc (sds_id)

        !SDS_name='OT_BTD_trop'
        !sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        !status=sfwdata(sds_id, start, stride, edges, OT_BTD_trop)
        !status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
        !        'OT BTD for pixel BT<tropopause temp')
        !status = sfendacc (sds_id)

        SDS_name='cirrus_BT'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges, mean_cirrus_bt)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
                'mean cirrus BT11um around center pixel')
        status = sfendacc(sds_id)

        SDS_name='OT_Area'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges, OT_area)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
                'OT area')
        status = sfendacc(sds_id)

        SDS_name='OT_BT'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges, mean_OT_BT)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
                'mean bt for the ot determines from second &
                 derivative method')
        status = sfendacc(sds_id)


        SDS_name='storm_Area'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges, storm_area)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
                'storm area BT11 <260&BTD>-3')
        status = sfendacc(sds_id)

        SDS_name='storm_BT'
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata(sds_id, start, stride, edges, mean_storm_bt)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
                'storm area mean bt')
        status = sfendacc(sds_id)


        status = sfend ( fid )

        deallocate(OT_flag)
        deallocate(BT11)
        deallocate(BT67)
        deallocate(OT_BTD_trop)
        deallocate(mean_cirrus_bt)
        deallocate(OT_area)
        deallocate(storm_area)
        deallocate(mean_storm_bt)
        deallocate(cirrus_radii_num)
        deallocate(mean_OT_BT)

        end

