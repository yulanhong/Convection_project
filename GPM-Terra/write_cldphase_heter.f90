        subroutine write_cldphase_heter (FILE_NAME)

        use global, only : Thetero_pdf,	hetero_x, Tmodis_clrnum, Tmodis_liqnum,&
			Tmodis_icenum,Tmodis_undernum,Tmodis_obsnum,Tmodis_clrhetero,Tmodis_liqhetero,&
			Tmodis_icehetero,Tmodis_underhetero,Tmodis_obshetero, Ninterval, Tmodcldfraction,&
			Tmodliqfraction
        
        implicit none
 
!******* Function declaration.**************************************
!
        integer sfstart,sfcreate, sfsnatt, sfwdata, sfendacc, sfend, &
                sfsattr
 
!**** Variable declaration *******************************************
!
        character(len=200) ::  FILE_NAME
        character(len=20) :: SDS_NAME

        integer  ::     RANK
        integer :: parameter,DFACC_CREATE=4,DFNT_INT16=22,DFNT_INT32=24,&
                DFNT_FLOAT32=5,DFNT_CHAR8=4,DFNT_FLOAT=5,DFNT_INT8=21
        integer :: fid, sds_id, status
        integer :: start ( 3 ), edges ( 3 ), stride ( 3 ),dim_sizes(3)
        integer :: i, j
        real :: miss_value,offset,factor,datarange(2)

        fid = sfstart( FILE_NAME, DFACC_CREATE )
        IF (fid==-1) then
                print *,'process stop at writing file: ',FILE_NAME
                stop
        ENDIF

!***************WRITE GEODATAFIELD************************************     
		RANK=1
        SDS_NAME="hetero_interval"
        dim_sizes(1)=Ninterval
        start  ( 1 ) = 0
        edges  ( 1 ) = Ninterval 
        stride ( 1 ) = 1
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata( sds_id, start, stride, edges, hetero_x )
		status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,'hetero interval')
        status = sfendacc ( sds_id )

        RANK=2           
        SDS_NAME="subarea_hetero_pdf"
        dim_sizes(1)=Ninterval
	    dim_sizes(2)=6
        
        start  ( 1:2 ) = 0
        edges  ( 1 ) = Ninterval
        edges  ( 2 ) = 6
        stride ( 1:2 ) = 1
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata (sds_id, start, stride, edges, Thetero_pdf )
		status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,'1-clear,2-liquid,3-ice,&
			4-mix,5-undertermined,6-all')
        status = sfendacc ( sds_id )

        SDS_NAME="subarea_cldfraction"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata (sds_id, start, stride, edges,Tmodcldfraction/Thetero_pdf)
        status = sfendacc ( sds_id )

        SDS_NAME="subarea_liqfraction"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata (sds_id, start, stride, edges,Tmodliqfraction/Thetero_pdf)
        status = sfendacc ( sds_id )

		dim_sizes(1)=360
		dim_sizes(2)=180
        edges(1) =360 
        edges(2) =180

        SDS_NAME="modis_clrnum"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata (sds_id, start, stride, edges,Tmodis_clrnum)
        status = sfendacc ( sds_id )
		
        SDS_NAME="modis_liqnum"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata (sds_id, start, stride, edges,Tmodis_liqnum)
        status = sfendacc ( sds_id )
	
        SDS_NAME="modis_icenum"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata (sds_id, start, stride, edges,Tmodis_icenum)
        status = sfendacc ( sds_id )

        SDS_NAME="modis_undernum"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata (sds_id, start, stride, edges,Tmodis_undernum)
        status = sfendacc ( sds_id )

        SDS_NAME="modis_obsnum"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT32,RANK,dim_sizes)
        status=sfwdata (sds_id, start, stride, edges,Tmodis_obsnum)
        status = sfendacc ( sds_id )

        SDS_NAME="modis_clrhetero"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata (sds_id, start, stride, edges,Tmodis_clrhetero/Tmodis_clrnum)
        status = sfendacc ( sds_id )

        SDS_NAME="modis_liqhetero"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata (sds_id, start, stride, edges,Tmodis_liqhetero/Tmodis_liqnum)
        status = sfendacc ( sds_id )

        SDS_NAME="modis_icehetero"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata (sds_id, start, stride, edges,Tmodis_icehetero/Tmodis_icenum)
        status = sfendacc ( sds_id )

        SDS_NAME="modis_underhetero"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata (sds_id, start, stride, edges,Tmodis_underhetero/Tmodis_undernum)
        status = sfendacc ( sds_id )

        SDS_NAME="modis_obshetero"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        status=sfwdata (sds_id, start, stride, edges,Tmodis_obshetero/Tmodis_obsnum)
        status = sfendacc ( sds_id )

        status = sfend ( fid )
!        print *,status
        print *,'finish writing',file_name

        end subroutine write_cldphase_heter
