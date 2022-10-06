        subroutine read_modis06(fname)

	use global, only: modcth 

        implicit none

        !-----------------------------------------
        character(len=94) :: fname,sds_name
	real :: vscale

        integer :: fid,sd, sid,attr_id,ranki, DFACC_READ=1
	integer :: rank, data_type, n_attrs
	integer :: dim_sizes(32), start(32), edges(32), stride(32)
        integer :: sfstart,sfselect,sfrdata,sfendacc,sfend,sffattr,&
                sfrattr,  err_code, sfn2index
        integer :: status
        integer :: sffinfo , sfginfo


        fid=sfstart(fname,DFACC_READ)

        
	sds_name='Cloud_Top_Height'
        sid=sfn2index(fid,sds_name)
        sd =sfselect(fid,sid)
	status=sfginfo(sd,sds_name,rank,dim_sizes,data_type,n_attrs)
	start(1:2)=0
	edges(1)=dim_sizes(1)
	edges(2)=dim_sizes(2)
	stride(1:2)=1
	allocate(modcth(dim_sizes(1),dim_sizes(2)))
	status=sfrdata(sd,start,stride,edges,modcth)
	status=sfendacc(sd)

	!sds_name='Cloud_Phase_Infrared_1km'
        !sid=sfn2index(fid,sds_name)
        !sd =sfselect(fid,sid)
	!	status=sfginfo(sd,sds_name,rank,dim_sizes,data_type,n_attrs)
	!	start(1:2)=0
	!	edges(1)=dim_sizes(1)
	!	edges(2)=dim_sizes(2)
	!	stride(1:2)=1
	!	allocate(modcldphase(dim_sizes(1),dim_sizes(2)))
	!	status=sfrdata(sd,start,stride,edges,modcldphase)
!		status=sfendacc(sd)

	!	sds_name='Cloud_Mask_SPI'
        !sid=sfn2index(fid,sds_name)
        !sd =sfselect(fid,sid)
	!	status=sfginfo(sd,sds_name,rank,dim_sizes,data_type,n_attrs)
	!	start(1:3)=0
	!	edges(1)=dim_sizes(1)
	!	edges(2)=dim_sizes(2)
	!	edges(3)=dim_sizes(3)
	!	stride(1:3)=1
	!	allocate(cld_spi_1(dim_sizes(1),dim_sizes(2),dim_sizes(3)))
	!	allocate(cld_spi(dim_sizes(1),dim_sizes(2),dim_sizes(3)))

	!	status=sfrdata(sd,start,stride,edges,cld_spi_1)		
	!	vscale=0.01
	!	cld_spi(1,:,:)=cld_spi_1(1,:,:)*vscale
	!	cld_spi(2,:,:)=cld_spi_1(2,:,:)*vscale
	!	status=sfendacc(sd)

		status=sfend(fid)	

		!print *,'nrow in mod06',NROW,fname
        end
