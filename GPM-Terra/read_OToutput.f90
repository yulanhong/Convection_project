
        subroutine read_OToutput(OT_fname)

        use global, only: OT_BT,OT_BTD, OT_BTD_trop, &
                OT_flag,mean_cirrus_bt, modNcol,modNrow

        implicit none

        character (len=108) :: OT_fname
        character (len=10) :: sds_name

        integer :: fid,sd, sid, attr_id, DFACC_READ=1
        !integer :: rank, data_type, n_attrs
        integer :: dim_sizes(32), start(32), edges(32),stride(32)
        integer :: sfstart,sfselect,sfrdata,sfendacc,sfend,sffattr,&
                sfrattr,  err_code, sfn2index
        integer :: status
        integer :: sffinfo , sfginfo

        
         start(1:2)=0
         edges(1)=modNcol
         edges(2)=modNrow
         stride(1:2)=1

         allocate(OT_BT(modNcol,modNrow))
         allocate(OT_BTD(modNcol,modNrow))
         allocate(OT_BTD_trop(modNcol,modNrow))
         allocate(OT_flag(modNcol,modNrow))
         allocate(mean_cirrus_bt(modNcol,modNrow))


        fid=sfstart(OT_fname,DFACC_READ)
        
        sds_name='OT_Flag'
        sid=sfn2index(fid,'OT_Flag')
        sd =sfselect(fid,sid)
        status=sfrdata(sd,start,stride,edges,OT_flag)
        status=sfendacc(sd)


        sds_name='OT_BTD'
        sid=sfn2index(fid,'OT_BTD')
        sd =sfselect(fid,sid)
        status=sfrdata(sd,start,stride,edges,OT_BTD)
        status=sfendacc(sd)

        sds_name='OT_BT11'
        sid=sfn2index(fid,'OT_BT11')
        sd =sfselect(fid,sid)
        status=sfrdata(sd,start,stride,edges,OT_BT)

        sds_name='OT_BTD_trop'
        sid=sfn2index(fid,'OT_BTD_trop')
        sd =sfselect(fid,sid)
        status=sfrdata(sd,start,stride,edges,OT_BTD_trop)
        status=sfendacc(sd)


        sds_name='cirrus BT'
        sid=sfn2index(fid,'cirrus BT')
        sd =sfselect(fid,sid)
        status=sfrdata(sd,start,stride,edges,mean_cirrus_bt)
        status=sfendacc(sd)

        status=sfend(fid)

        end
