        subroutine read_modis02(fname)

        use global, only: modNrow,modNcol,rad_1000emis,rad_250

        implicit none

        !-----------------------------------------
        character(len=87) :: fname,sds_name
        real, allocatable :: offset(:),vscale(:)
	integer (kind=2), allocatable :: rad_250_1(:,:,:),& !rad_500_1(:,:,:),rad_1000_1(:,:,:),&
                rad_1000emis_1(:,:,:) 

        integer :: fid,sd, sid,attr_id,ranki, DFACC_READ=1
		integer :: rank, data_type, n_attrs
		integer :: dim_sizes(32), start(32), edges(32), stride(32)
        integer :: sfstart,sfselect,sfrdata,sfendacc,sfend,sffattr,&
                sfrattr,  err_code, sfn2index
        integer :: status
        integer :: sffinfo , sfginfo

        !print *,fname
        fid=sfstart(fname,DFACC_READ)

	sds_name='EV_250_Aggr1km_RefSB'
        sid=sfn2index(fid,'EV_250_Aggr1km_RefSB')
        sd =sfselect(fid,sid)
	status=sfginfo(sd,sds_name,rank,dim_sizes,data_type,n_attrs)
	!	modNrow = dim_sizes(2)
	!	modNcol = dim_sizes(1)
	start(1:3)=0
	edges(1)=dim_sizes(1)
	edges(2)=dim_sizes(2)
	edges(3)=dim_sizes(3)
	stride(1:3)=1
	allocate(rad_250_1(dim_sizes(1),dim_sizes(2),dim_sizes(3)))
	allocate(rad_250(dim_sizes(1),dim_sizes(2),dim_sizes(3)))
	
	status=sfrdata(sd,start,stride,edges,rad_250_1)		
	allocate(vscale(dim_sizes(3)))
	allocate(offset(dim_sizes(3)))
	!attr_id=sffattr(sd,'radiance_scales')
	!	status=sfrattr(sd,attr_id,vscale)
	!	!print *,vscale
	!	attr_id=sffattr(sd,'radiance_offsets')
	!	status=sfrattr(sd,attr_id,offset)
	attr_id=sffattr(sd,'reflectance_scales')
	status=sfrattr(sd,attr_id,vscale)
!	print *,vscale
	attr_id=sffattr(sd,'reflectance_offsets')
	status=sfrattr(sd,attr_id,offset)
 !       print *,offset	
		deallocate(vscale)
		deallocate(offset)

	!	Do ranki=1,dim_sizes(3)
	!	rad_250(:,:,ranki)=(rad_250_1(:,:,ranki)-offset(ranki))*vscale(ranki)
	!	Enddo 
		!print *,rad_250(10,10,:),rad_250_1(10,10,:)
	!	deallocate(vscale)
	!	deallocate(offset)
	!	status=sfendacc(sd)
		
	!	sds_name='EV_500_Aggr1km_RefSB'
        !sid=sfn2index(fid,'EV_500_Aggr1km_RefSB')
        !sd =sfselect(fid,sid)
	!	status=sfginfo(sd,sds_name,rank,dim_sizes,data_type,n_attrs)
	!	start(1:3)=0
	!	edges(1)=dim_sizes(1)
	!	edges(2)=dim_sizes(2)
	!	edges(3)=dim_sizes(3)
	!	stride(1:3)=1

	!	allocate(rad_500_1(dim_sizes(1),dim_sizes(2),dim_sizes(3)))
	!	allocate(rad_500(dim_sizes(1),dim_sizes(2),dim_sizes(3)))

	!	status=sfrdata(sd,start,stride,edges,rad_500_1)		
	!	allocate(vscale(dim_sizes(3)))
	!	allocate(offset(dim_sizes(3)))
	!	attr_id=sffattr(sd,'radiance_scales')
	!	status=sfrattr(sd,attr_id,vscale)
		!print *,vscale
	!	attr_id=sffattr(sd,'radiance_offsets')
	!	status=sfrattr(sd,attr_id,offset)
		!print *,offset	
	!	Do ranki=1,dim_sizes(3)
	!	rad_500(:,:,ranki)=(rad_500_1(:,:,ranki)-offset(ranki))*vscale(ranki)
	!	Enddo 
		!print *,rad_500(10,10,:),rad_500_1(10,10,:)
	!	deallocate(vscale)
	!	deallocate(offset)
	!	status=sfendacc(sd)

	!	sds_name='EV_1KM_RefSB'
        !sid=sfn2index(fid,'EV_1KM_RefSB')
        !sd =sfselect(fid,sid)
	!	status=sfginfo(sd,sds_name,rank,dim_sizes,data_type,n_attrs)
	!	start(1:3)=0
	!	edges(1)=dim_sizes(1)
	!	edges(2)=dim_sizes(2)
	!	edges(3)=dim_sizes(3)
	!	stride(1:3)=1

	!	allocate(rad_1000_1(dim_sizes(1),dim_sizes(2),dim_sizes(3)))
	!	allocate(rad_1000(dim_sizes(1),dim_sizes(2),dim_sizes(3)))

	!	status=sfrdata(sd,start,stride,edges,rad_1000_1)		
	!	allocate(vscale(dim_sizes(3)))
	!	allocate(offset(dim_sizes(3)))
	!	attr_id=sffattr(sd,'radiance_scales')
	!	status=sfrattr(sd,attr_id,vscale)
		!print *,vscale
	!	attr_id=sffattr(sd,'radiance_offsets')
	!	status=sfrattr(sd,attr_id,offset)
		!print *,offset	
	!	Do ranki=1,dim_sizes(3)
	!	rad_1000(:,:,ranki)=(rad_1000_1(:,:,ranki)-offset(ranki))*vscale(ranki)
	!	Enddo 
		!print *,rad_1000(10,10,:),rad_1000_1(10,10,:)
	!	deallocate(vscale)
	!	deallocate(offset)
	!	status=sfendacc(sd)

                sds_name='EV_1KM_Emissive'
                sid=sfn2index(fid,'EV_1KM_Emissive')
                sd =sfselect(fid,sid)
                status=sfginfo(sd,sds_name,rank,dim_sizes,data_type,n_attrs)
		!print *,NROW,dim_sizes
                start(1:3)=0
                edges(1)=dim_sizes(1)
                edges(2)=dim_sizes(2)
                edges(3)=dim_sizes(3)
                stride(1:3)=1

		allocate(rad_1000emis_1(dim_sizes(1),dim_sizes(2),dim_sizes(3)))
		allocate(rad_1000emis(dim_sizes(1),dim_sizes(2),dim_sizes(3)))

		status=sfrdata(sd,start,stride,edges,rad_1000emis_1)		
		allocate(vscale(dim_sizes(3)))
		allocate(offset(dim_sizes(3)))
                attr_id=sffattr(sd,'radiance_scales')
		status=sfrattr(sd,attr_id,vscale)
!		print *,vscale
                attr_id=sffattr(sd,'radiance_offsets')
		status=sfrattr(sd,attr_id,offset)
!		print *,offset	
		Do ranki=1,dim_sizes(3)
		rad_1000emis(:,:,ranki)=(rad_1000emis_1(:,:,ranki)-offset(ranki))*vscale(ranki)
		Enddo 
	!	print *,rad_1000emis(10,10,:),rad_1000emis_1(10,10,:)

        !        print *,modnrow,modncol,size(rad_1000emis)

		deallocate(vscale)
		deallocate(offset)
		status=sfendacc(sd)
	
		status=sfend(fid)	

		!deallocate(rad_250_1)
		!deallocate(rad_500_1)
		!deallocate(rad_1000_1)
		deallocate(rad_1000emis_1)

        end
