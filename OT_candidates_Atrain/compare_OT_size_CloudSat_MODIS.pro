
pro compare_OT_size_CloudSat_MODIS

  print,systime()

  planck_c1=1.191042e8
  planck_c2=1.4387752e4

  sensorhgt_parallax=705.0
  pi=3.1415926
  earth_radius=6478.0

  levels=256
  c_level=findgen(levels)*0.15-25
	

  dir='/u/sciteam/yulanh/scratch/CloudSat/2B-GEOPROF/2007/'

  fname='OT_selected_2007.txt'
  Nline=file_lines(fname)
  OT_location=strarr(Nline)

  OT_lon=list()
  OT_lat=list()
  OT_size_Cldsat=list()
  OT_size_modis=list()
  OT_BT11=list()
  OT_BT67=list()
  OT_vis =list()
  OT_above_trop_cld=list()
  OT_above_trop_mod=list()

  png_fname=''

  openr,u,fname,/get_lun

  for i=0,Nline-1 do begin
	readf,u,png_fname
	print,i
	if i lt 0 then goto,jump1

	strflag=strmid(png_fname,0,19)
	geoprof_fname=dir + strflag +'_CS_2B-GEOPROF_GRANULE_P1_R05_E02_F00.hdf'

	year=strmid(strflag,0,4)
	day= strmid(strflag,4,3)
	hour=fix(strmid(strflag,7,2))
	minu=fix(strmid(strflag,9,2))
	secd=strmid(strflag,11,2)
	inital_hhmm=float(hour)+float(minu)/60.0

	OTinfo_si=float(strmid(png_fname,20,5))

	read_dardar,geoprof_fname,'Radar_Reflectivity',radar_ze
 	read_dardar,geoprof_fname,'Height',height
	height=height/1000.	

 	read_cloudsat,geoprof_fname,'2B-GEOPROF','Latitude',lat
    read_cloudsat,geoprof_fname,'2B-GEOPROF','Longitude',lon
    read_cloudsat,geoprof_fname,'2B-GEOPROF','Profile_time',time

	time=time/3600.0 + inital_hhmm ; unit--hour
	Ns=n_elements(lat)
    
	cldclass_fname='/u/sciteam/yulanh/scratch/CloudSat/2B-CLDCLASS-LIDAR/2008/'+strflag+'_CS_2B-CLDCLASS-LIDAR_GRANULE_P1_R05_E02_F00.hdf'
	read_dardar,cldclass_fname,'CloudLayerTop',cldtop ; used for modis parrax-correlction

	; get tropopause 
	trop_hgt_fname='/u/sciteam/yulanh/mydata/OT_modis/CCM/Tropopause_top/Tropopause_cldtop_'+strflag+'_lr.sav' 
	restore,trop_hgt_fname

	hour_1=floor(time[OTinfo_si])
	if (hour_1 eq 24) then begin ;go to next day
		hour_1=0
		day_1=fix(day)+1
		day=strmid(strcompress(string(day_1/1000.0)),3,3)	
	endif

	minu_1=60*(time[OTinfo_si]-floor(time[OTinfo_si]))
	diff_minu=minu_1-10*floor(minu_1/10)
	if (diff_minu lt 5) then minu_2=string(10*floor(minu_1/10)) else minu_2=string(10*floor(minu_1/10)+5)
	
    hour_2=strmid(strcompress(string(hour_1/100.0)),3,2)
	minu_3=strmid(strcompress(string(minu_2/100.0)),3,2)
	

	hhmm=strcompress(string(hour_2)+minu_3,/rem)

	;=== look for MODIS fname ======
    mod03_dir = '/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT/MYD03/'+year+$
		'/'+day+'/'
	mod03_fname=file_search(mod03_dir,'*'+year+day+'.'+hhmm+'.*')
    mod02_dir = '/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT/MYD021KM/'+year+$
		'/'+day+'/'
	mod02_fname=file_search(mod02_dir,'*'+year+day+'.'+hhmm+'.*')

	;=== if file doesn't exist, download file
	if (mod03_fname eq '') then begin
		 spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_mod03.sh > modis_dn_mod03_1.sh'
		 spawn,'sed s/001/'+day+'/g'+' modis_dn_mod03_1.sh > modis_dn_mod03_2.sh'
		 spawn,'sh modis_dn_mod03_2.sh'

		 spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_myd02.sh > modis_dn_myd02_1.sh'		
	  	 spawn,'sed s/001/'+day+'/g'+' modis_dn_myd02_1.sh > modis_dn_myd02_2.sh'		
	  	 spawn,'sh modis_dn_myd02_2.sh'

		 spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_myd06.sh > modis_dn_myd06_1.sh'
		 spawn,'sed s/001/'+day+'/g'+' modis_dn_myd06_1.sh > modis_dn_myd06_2.sh'	
	  	 spawn,'sh modis_dn_myd06_2.sh'
    	mod03_dir = '/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT/MYD03/'+year+$
		'/'+day+'/'
		mod03_fname=file_search(mod03_dir,'*'+year+day+'.'+hhmm+'.*')
    	mod02_dir = '/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT/MYD021KM/'+year+$
		'/'+day+'/'
		mod02_fname=file_search(mod02_dir,'*'+year+day+'.'+hhmm+'.*')
	endif

	;=== search for backward or forward files ======
	;===== only the current data doesn't match, go to previous or backward file ====
	
	read_modis_1,mod03_fname[0],'Latitude',modlat
	read_modis_1,mod03_fname[0],'Longitude',modlon
	diffdis=sqrt(abs(lon[OTinfo_si]- modlon)^2.0+abs(lat[OTinfo_si]- modlat)^2.0)
	disind =where(min(diffdis) eq diffdis)

	if diffdis[disind[0]] gt 0.02 then begin
		if ((diff_minu- (minu_2-10*(minu_2/10))) lt 2.5) then begin ;closer to previous file
			minu_1=60*(time[OTinfo_si]-floor(time[OTinfo_si]))-5	
			if (minu_1 lt 0) then begin
				minu_1=55
				hour_1=hour_1-1
			endif

			diff_minu=minu_1-10*floor(minu_1/10)
			if (diff_minu lt 5) then minu_2=string(10*floor(minu_1/10)) else minu_2=string(10*floor(minu_1/10)+5)
	
    		hour_2=strmid(strcompress(string(hour_1/100.0)),3,2)
			minu_3=strmid(strcompress(string(minu_2/100.0)),3,2)
	
			hhmm=strcompress(string(hour_2)+minu_3,/rem)

			mod03_fname=file_search(mod03_dir,'*'+year+day+'.'+hhmm+'.*')
			mod02_fname=file_search(mod02_dir,'*'+year+day+'.'+hhmm+'.*')

	;=== if file doesn't exist, download file
			if (mod03_fname eq '' or mod02_fname eq '') then begin
		 		spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_mod03.sh > modis_dn_mod03_1.sh'
		 		spawn,'sed s/001/'+day+'/g'+' modis_dn_mod03_1.sh > modis_dn_mod03_2.sh'
		 		spawn,'sh modis_dn_mod03_2.sh'

		 		spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_myd02.sh > modis_dn_myd02_1.sh'		
	  	 		spawn,'sed s/001/'+day+'/g'+' modis_dn_myd02_1.sh > modis_dn_myd02_2.sh'		
	  	 		spawn,'sh modis_dn_myd02_2.sh'

		 		spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_myd06.sh > modis_dn_myd06_1.sh'
		 		spawn,'sed s/001/'+day+'/g'+' modis_dn_myd06_1.sh > modis_dn_myd06_2.sh'	
	  	 		spawn,'sh modis_dn_myd06_2.sh'
    			mod03_dir = '/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT/MYD03/'+year+$
				'/'+day+'/'
				mod03_fname=file_search(mod03_dir,'*'+year+day+'.'+hhmm+'.*')
    			mod02_dir = '/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT/MYD021KM/'+year+$
				'/'+day+'/'
				mod02_fname=file_search(mod02_dir,'*'+year+day+'.'+hhmm+'.*')
			endif

		endif else begin ;closer to later file

			minu_1=60*(time[OTinfo_si]-floor(time[OTinfo_si]))+5	
			diff_minu=minu_1-10*floor(minu_1/10)
			if (diff_minu lt 5) then minu_2=string(10*floor(minu_1/10)) else minu_2=string(10*floor(minu_1/10)+5)
	
    		hour_2=strmid(strcompress(string(hour_1/100.0)),3,2)
			minu_3=strmid(strcompress(string(minu_2/100.0)),3,2)
	
			hhmm=strcompress(string(hour_2)+minu_3,/rem)

			mod03_fname=file_search(mod03_dir,'*'+year+day+'.'+hhmm+'.*')
			mod02_fname=file_search(mod02_dir,'*'+year+day+'.'+hhmm+'.*')

	;=== if file doesn't exist, download file
			if (mod03_fname eq '' or mod02_fname eq '') then begin
		 		spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_mod03.sh > modis_dn_mod03_1.sh'
		 		spawn,'sed s/001/'+day+'/g'+' modis_dn_mod03_1.sh > modis_dn_mod03_2.sh'
		 		spawn,'sh modis_dn_mod03_2.sh'

		 		spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_myd02.sh > modis_dn_myd02_1.sh'		
	  	 		spawn,'sed s/001/'+day+'/g'+' modis_dn_myd02_1.sh > modis_dn_myd02_2.sh'		
	  	 		spawn,'sh modis_dn_myd02_2.sh'

		 		spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_myd06.sh > modis_dn_myd06_1.sh'
		 		spawn,'sed s/001/'+day+'/g'+' modis_dn_myd06_1.sh > modis_dn_myd06_2.sh'	
	  	 		spawn,'sh modis_dn_myd06_2.sh'
    			mod03_dir = '/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT/MYD03/'+year+$
				'/'+day+'/'
				mod03_fname=file_search(mod03_dir,'*'+year+day+'.'+hhmm+'.*')
    			mod02_dir = '/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT/MYD021KM/'+year+$
				'/'+day+'/'
				mod02_fname=file_search(mod02_dir,'*'+year+day+'.'+hhmm+'.*')

			endif	

	   endelse ; end to select file
	endif ; endif doesn't match the 	

	print,mod03_fname

	;==read MODIS data  ====
	read_modis_1,mod03_fname[0],'Latitude',modlat
	read_modis_1,mod03_fname[0],'Longitude',modlon
	read_modis_1,mod03_fname[0],'SensorZenith',SensorZenith
	read_modis_1,mod03_fname[0],'SensorAzimuth',SensorAzimuth

	read_modis_02,mod02_fname[0],rad_062,rad_11,rad_67

	BT11=planck_c2/(11.03*alog(planck_c1/(rad_11*11.03^5.0)+1))
	BT67=planck_c2/(6.72*alog(planck_c1/(rad_67*6.72^5.0)+1))
	modis_bt11_gs=gauss_smooth(bt11,1)
	modis_bt67_gs=gauss_smooth(bt67,1)


	;==== parallax correction === 
	;=== search around OT si 150+/-
	tpNs=301
	colocate_lat_parallax=fltarr(301)
	colocate_lon_parallax=fltarr(301)
	colocate_lat=fltarr(301)
	colocate_lon=fltarr(301)
	colocate_bt11=fltarr(301)
	colocate_bt11_np=fltarr(301)
	colocate_bt67=fltarr(301)
	colocate_ref062=fltarr(301)
	topmost_dbz=fltarr(tpNs)

	ii=0
	For si=OTinfo_si-150,OTinfo_si+150 Do begin

		;calculate the topmost layer of -27dbz 
		tp_dbz=radar_ze[*,si]
		tp_hgt=height[*,si]

        ind1=where(tp_dbz/100. ge -25 and tp_hgt gt 0,cnt)
        Ncontinues=1 ; check at least five continues number
        if ind1[0] ne -1 then begin
        for hi=1,cnt-1 do begin
        	if (ind1[hi]- ind1[hi-1]) eq 1 then Ncontinues=Ncontinues+1
 ;               print,hi,ind1[hi],Ncontinues
	        if Ncontinues ge 5 then break
            if (ind1[hi]- ind1[hi-1]) gt 1 then Ncontinues=1
         endfor
	    if hi le cnt-1 then topmost_dbz[ii]=tp_hgt[ind1[hi]-Ncontinues+1]
        endif ; check t

		if (si ge 0) and (si le Ns-1) then begin	
			tpcldlat=lat[si]
			tpcldlon=lon[si]
			cth_parallax=topmost[si]
  			r_parallax=cth_parallax*earth_radius*tan(SensorZenith*pi/180.0)/(earth_radius-cth_parallax)
    		modlat_parallax=modlat+r_parallax*cos(pi+SensorAzimuth*pi/180.0)/earth_radius
   	 		modlon_parallax=modlon+r_parallax*sin(pi+SensorAzimuth*pi/180.0)/(earth_radius*cos(modlat*pi/180.0))

			diffdis=sqrt(abs(tpcldlon-modlon_parallax)^2.0+abs(tpcldlat-modlat_parallax)^2.0)
			disind =where(min(diffdis) eq diffdis)
			res_scp=array_indices(modlat,disind)
		;	print,diffdis[disind[0]],tpcldlat,tpcldlon,modlon_parallax[res_scp[0],res_scp[1]],modlat_parallax[res_scp[0],res_scp[1]],modis_bt11_gs[res_scp[0],res_scp[1]]

			if diffdis[disind[0]] le 0.02 then begin
				colocate_lat_parallax[ii]=modlat_parallax[res_scp[0],res_scp[1]]
				colocate_lon_parallax[ii]=modlon_parallax[res_scp[0],res_scp[1]]
				colocate_bt11[ii] = modis_bt11_gs[res_scp[0],res_scp[1]]
				colocate_bt67[ii] = modis_bt67_gs[res_scp[0],res_scp[1]]
				colocate_ref062[ii]=rad_062[res_scp[0],res_scp[1]]
			endif

			diffdis=sqrt(abs(tpcldlon-modlon)^2.0+abs(tpcldlat-modlat)^2.0)
			disind =where(min(diffdis) eq diffdis)
			res_scp=array_indices(modlat,disind)
		;	print,diffdis[disind[0]],tpcldlat,tpcldlon,modlon[res_scp[0],res_scp[1]],modlat[res_scp[0],res_scp[1]]
			if diffdis[disind[0]] le 0.02 then begin
			colocate_lat[ii]=modlat[res_scp[0],res_scp[1]]
			colocate_lon[ii]=modlon[res_scp[0],res_scp[1]]
				colocate_bt11_np[ii] = modis_bt11_gs[res_scp[0],res_scp[1]]
			endif

			if (topmost[si] lt 0) then begin
				colocate_bt11[ii] = modis_bt11_gs[res_scp[0],res_scp[1]]
				colocate_bt67[ii] = modis_bt67_gs[res_scp[0],res_scp[1]]
				colocate_ref062[ii]=rad_062[res_scp[0],res_scp[1]]
				colocate_lat_parallax[ii]=modlat[res_scp[0],res_scp[1]]
				colocate_lon_parallax[ii]=modlon[res_scp[0],res_scp[1]]
			endif
	
		endif
	ii=ii+1
	EndFor

    x=findgen(tpNs)
	x2=findgen(tpNs)*1.0
	pp=imsl_cssmooth(x,colocate_bt11)
	colocate_bt11_sm=imsl_spvalue(x2,pp)
	pp=imsl_cssmooth(x,colocate_bt67)
	colocate_bt67_sm=imsl_spvalue(x2,pp)
	;first derivative 
	first_deriv_bt11=deriv(x*0.1,colocate_bt11_sm)
	;second derivativ
	second_deriv_bt11=deriv(x*0.1,first_deriv_bt11)
	;the center of the OT
	tplowscp=OTinfo_si-150
	if tplowscp lt 0 then tplowscp=0
	tpupscp=OTinfo_si+150
	if tpupscp gt n_elements(tropopause_hgt_lr)-1 then tpupscp=n_elements(tropopause_hgt_lr)-1
	
	tptropopause=tropopause_hgt_lr[tplowscp:tpupscp]
    tptopmost = topmost[tplowscp:tpupscp]
	tpcldlon = lon[tplowscp:tpupscp]
	tpcldlat = lat[tplowscp:tpupscp]
	tpheight = height[*,tplowscp:tpupscp]

	; plot the cases 
	newrgb=colortable(3,/reverse)
	tpradar_ze=radar_ze[*,tplowscp:tpupscp]
	c1=contour(transpose(tpradar_ze)/100.0,tpcldlat,reform(tpheight[*,151]),$
		rgb_table=newrgb,c_value=c_level,/fill,xtitle='Lat',ytitle='Alt. (km)',yrange=[0,20],$
		n_levels=levels,buffer=0)
    ptrop=plot(tpcldlat,tptropopause,overplot=c1,color='r',thick=2)
	ptpdb=plot(tpcldlat,topmost_dbz, overplot=c1,color='black',thick=2)	
	pos=c1.position
	c1.scale,1,0.8
;	ct=colorbar(target=c1,position=[pos[0]+0.1,pos[3]-0.15,pos[2]-0.1,pos[3]-0.13])

	;=== check the number of OT ===
	ind=where(topmost_dbz gt tptropopause)
	OT_scp=list()
	cnt=n_elements(ind)
    for ti=1,cnt-1 do begin
		
		if (ti eq 1) then OT=list()
		if (ind[ti]- ind[ti-1] eq 1) then OT.add,ind[ti-1] 
		if (ind[ti]- ind[ti-1] gt 1) then begin
			OT.add,ind[ti-1]
			OT_scp.add,OT
			OT=list()	
		endif
		; deal with the last number
		if (ti eq cnt-1 and ind[ti]-ind[ti-1]) then begin
			OT.add,ind[ti]
			OT_scp.add,OT
	 	endif	

	endfor 
		
	OTnum=n_elements(OT_scp)
	for oti=0,OTnum-1 do begin
		tpotscp=OT_scp[oti]
		tpotscp_1=tpotscp.ToArray()
		tpotsize_cldsat=1.7*(tpotscp_1[n_elements(tpotscp_1)-1] - tpotscp_1[0])			
		tpot_trop_cld=mean(topmost_dbz[ind]- tptropopause[ind])
		;search for OT center
		tpbt11sm=colocate_bt11_sm[tpotscp_1]
		btind=where(min(tpbt11sm) eq tpbt11sm)
		; only deal with the ots not at the end 
	
		if (tpotscp_1[btind] ge 15 and tpotscp_1[btind] le tpNs-15) then begin
			low_scp= tpotscp_1[btind]-15
			if low_scp lt 0 then low_scp=0
			up_scp = tpotscp_1[btind]+15
			if up_scp gt tpNs-1 then up_scp=tpNs-1

			left_deriv=reverse(second_deriv_bt11[low_scp:tpotscp_1[btind]])
			right_deriv=second_deriv_bt11[tpotscp_1[btind]:up_scp]
			leftind=where(left_deriv le 0)
			left_scp=tpotscp_1[btind]-leftind[0]
			rightind=where(right_deriv le 0)

			if leftind[0] ne -1 and rightind[0] ne -1 then begin
			right_scp=tpotscp_1[btind]+rightind[0]
			tpotsize_modsize=1.7*(right_scp-left_scp+1)
			tpot_trop_mod=mean(topmost_dbz[left_scp:right_scp]-tptropopause[left_scp:right_scp])
			; record the OT information
			OT_size_cldsat.add,tpotsize_cldsat ;add cldsat size
			OT_size_modis.add,tpotsize_modsize
			OT_lon.add,tpcldlon[tpotscp_1[btind]]
			OT_lat.add,tpcldlat[tpotscp_1[btind]]	
			OT_BT11.add,colocate_bt11[tpotscp_1[btind]]
			OT_BT67.add,colocate_bt67[tpotscp_1[btind]]
			OT_vis.add,colocate_ref062[tpotscp_1[btind]]
			OT_above_trop_cld.add,tpot_trop_cld
			OT_above_trop_mod.add,tpot_trop_mod
			; overplot the OT position
			pot = plot(tpcldlat[tpotscp_1],topmost_dbz[tpotscp_1],overplot=c1,color='g',thick=3)		
			pot1= plot(tpcldlat[left_scp:right_scp],tptropopause[left_scp:right_scp],overplot=c1,color='b',thick=3)
			endif; end valid subscript
		endif					

	endfor ; end for ot num

	c1.save,strflag+strmid(png_fname,19,6)+'_corrected.png'
	stop
  jump1:
  endfor ;endfor OT file
  free_lun,u

  print,systime()
  stop

end
