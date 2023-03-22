; save the colocated data: colocated lat, lon, bt11, bt67,vis_reflectance, tropopause height/temperature, OT information
pro write_OT_information

 ; in this version using MERRA2 tropopause height

  print,systime()

  planck_c1=1.191042e8
  planck_c2=1.4387752e4

  sensorhgt_parallax=705.0
  pi=3.1415926
  earth_radius=6478.0
  gas_constant=8.31446 ;J K-1 mol-1
  air_molar_mass=28.97 ; g mol-1

  levels=256
  c_level=findgen(levels)*0.15-25

  dir='/u/sciteam/yulanh/scratch/CloudSat/2B-GEOPROF/2007/'

;  fname='OT_selected_2008.txt'
;  fname='OT_information_2008_update.txt'
  fname='OT_selected_2007_updated.txt'
  Nline=file_lines(fname)
  OT_location=strarr(Nline)

;  OT_lon=list()
;  OT_lat=list()
;  OT_size_Cldsat=list()
;  OT_size_modis=list()
;  OT_size_merra=list() ; the diameter where BT11 < tropopause T
;  OT_thick_thin_size=list() ; thick cirrus anvil with BT11 < 230, TB67-TB11 > -3 K, thin cirrus 260
;   OT_meanbt_merra_modis=list() ; record the mean OT BT11
 ; cirrus_meanbt_modis_left=list() ; record the mean BT11 for thick cirrus arround 20 radius
;  cirrus_meanbt_modis_right=list() ; record the mean BT11 for thick cirrus arround 20 radius

;  OT_center_BT11=list()
;  OT_center_BT67=list()
;  OT_meanbt11=list()
;  OT_meanbt67=list()

;  OT_vis =list()
;  OT_above_trop_cld=list()
;  OT_above_trop_mod=list()
;  trop_merra2_T=list()
;  OT_location_cld=list()
;  OT_location_mod=list()
;  OT_filename=list()

  fname_array=strarr(Nline)
  png_fname=''

  openr,u,fname,/get_lun
  print,NLine
  for i=0, Nline-1 do begin
	readf,u,png_fname
	fname_array[i]=png_fname
  endfor
  free_lun,u

  openw,u,'OT_property_2007_280_bf_select.txt',/get_lun	
  
  printf,u,'center_lon, center_lat,OT_diameter, min_OT_bt11,mean_OT_bt11,min_OT_bt6.7,mean_OT_bt6.7,mean_ci_bt11_left,mean_ci_bt11_right,tropopuase_temp_ecmwf,tropopause_temp_merra2,tropopause_ss_b3km,tropopause_ss_a1km,tropopause_ss_a2k,tropopause_ss_a3k,tropopause_wv_a1k,tropopause_wv_a2k,tropopause_wv_a3k,CloudSat_Swath'
 print,Nline
  Nline=Nline
  for i=280,Nline-1 do begin

	png_fname=fname_array[i]

;	if i lt 300 then goto,jump1
	print,i
	strflag=strmid(png_fname,0,19)
	geoprof_fname=dir + strflag +'_CS_2B-GEOPROF_GRANULE_P1_R05_E02_F00.hdf'
	ecmwf_fname='/u/sciteam/yulanh/scratch/CloudSat/ECMWF/2007/'+strflag+$
	'_CS_ECMWF-AUX_GRANULE_P_R05_E02_F00.hdf'

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

	;===== read ecmwf data to get the tropopause again and calculate the static stability ===
	read_dardar,ecmwf_fname,'Temperature',temp
	read_cloudsat,ecmwf_fname,'ECMWF-AUX','EC_height',ec_hgt
	read_dardar,ecmwf_fname,'Pressure',press
	read_dardar,ecmwf_fname,'Specific_humidity',wv_sh

	ec_hgt=ec_hgt/1000.	

	time = time/3600.0 + inital_hhmm ; unit--hour
	Ns=n_elements(lat)
    
	cldclass_fname='/u/sciteam/yulanh/scratch/CloudSat/2B-CLDCLASS-LIDAR/2007/'+strflag+'_CS_2B-CLDCLASS-LIDAR_GRANULE_P1_R05_E02_F00.hdf'
	read_dardar,cldclass_fname,'CloudLayerTop',cldtop ; used for modis parrax-correlction

	;==============================================
	hour_1=floor(time[OTinfo_si])
	if (hour_1 ge 24) then begin ;go to next day
		hour_1=hour_1-24
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
    mod03_dir = '/u/sciteam/yulanh/scratch/MODIS/MYD03/MYD03/'+year+$
		'/'+day+'/'
	mod03_fname=file_search(mod03_dir,'*'+year+day+'.'+hhmm+'.*')
    mod02_dir = '/u/sciteam/yulanh/scratch/MODIS/MYD021KM/'+year+$
		'/'+day+'/'
	mod02_fname=file_search(mod02_dir,'*'+year+day+'.'+hhmm+'.*')

	;=== if file doesn't exist, download file
;	if (mod03_fname eq '') then begin
;		 spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_mod03.sh > modis_dn_mod03_1.sh'
;		 spawn,'sed s/001/'+day+'/g'+' modis_dn_mod03_1.sh > modis_dn_mod03_2.sh'
;		 spawn,'sh modis_dn_mod03_2.sh'

;		 spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_myd02.sh > modis_dn_myd02_1.sh'		
;	  	 spawn,'sed s/001/'+day+'/g'+' modis_dn_myd02_1.sh > modis_dn_myd02_2.sh'		
;	  	 spawn,'sh modis_dn_myd02_2.sh'

;		 spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_myd06.sh > modis_dn_myd06_1.sh'
;		 spawn,'sed s/001/'+day+'/g'+' modis_dn_myd06_1.sh > modis_dn_myd06_2.sh'	
;	  	 spawn,'sh modis_dn_myd06_2.sh'
;    	mod03_dir = '/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT/MYD03/'+year+$
;		'/'+day+'/'
;		mod03_fname=file_search(mod03_dir,'*'+year+day+'.'+hhmm+'.*')
;    	mod02_dir = '/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT/MYD021KM/'+year+$
;		'/'+day+'/'
;		mod02_fname=file_search(mod02_dir,'*'+year+day+'.'+hhmm+'.*')
;	endif

	;=== search for backward or forward files ======
	;===== only the current data doesn't match, go to previous or backward file ====
	
	read_modis_1,mod03_fname[0],'Latitude',modlat
	read_modis_1,mod03_fname[0],'Longitude',modlon
	diffdis=sqrt(abs(lon[OTinfo_si]- modlon)^2.0+abs(lat[OTinfo_si]- modlat)^2.0)
	disind =where(min(diffdis) eq diffdis)

	if diffdis[disind[0]] gt 0.05 then begin
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
			;print ,*mod03_fname,mod02_fname
	;=== if file doesn't exist, download file
;			if (mod03_fname eq '' or mod02_fname eq '') then begin
;		 		spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_mod03.sh > modis_dn_mod03_1.sh'
;		 		spawn,'sed s/001/'+day+'/g'+' modis_dn_mod03_1.sh > modis_dn_mod03_2.sh'
;		 		spawn,'sh modis_dn_mod03_2.sh'

;		 		spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_myd02.sh > modis_dn_myd02_1.sh'		
;	  	 		spawn,'sed s/001/'+day+'/g'+' modis_dn_myd02_1.sh > modis_dn_myd02_2.sh'		
;	  	 		spawn,'sh modis_dn_myd02_2.sh'
;
;		 		spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_myd06.sh > modis_dn_myd06_1.sh'
;		 		spawn,'sed s/001/'+day+'/g'+' modis_dn_myd06_1.sh > modis_dn_myd06_2.sh'	
;	  	 		spawn,'sh modis_dn_myd06_2.sh'
    		;	mod03_dir = '/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT/MYD03/'+year+$
			;	'/'+day+'/'
		;		mod03_fname=file_search(mod03_dir,'*'+year+day+'.'+hhmm+'.*')
    	;		mod02_dir = '/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT/MYD021KM/'+year+$
		;		'/'+day+'/'
	;			mod02_fname=file_search(mod02_dir,'*'+year+day+'.'+hhmm+'.*')
;			endif

		endif else begin ;closer to later file

			minu_1=60*(time[OTinfo_si]-floor(time[OTinfo_si]))+5	
			diff_minu=minu_1-10*floor(minu_1/10)
			if (diff_minu lt 5) then minu_2=string(10*floor(minu_1/10)) else minu_2=string(10*floor(minu_1/10)+5)
	
    		hour_2=strmid(strcompress(string(hour_1/100.0)),3,2)
			minu_3=strmid(strcompress(string(minu_2/100.0)),3,2)
	
			hhmm=strcompress(string(hour_2)+minu_3,/rem)

			mod03_fname=file_search(mod03_dir,'*'+year+day+'.'+hhmm+'.*')
			mod02_fname=file_search(mod02_dir,'*'+year+day+'.'+hhmm+'.*')

			print ,mod03_fname,mod02_fname
	;=== if file doesn't exist, download file
;			if (mod03_fname eq '' or mod02_fname eq '') then begin
;		 		spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_mod03.sh > modis_dn_mod03_1.sh'
;		 		spawn,'sed s/001/'+day+'/g'+' modis_dn_mod03_1.sh > modis_dn_mod03_2.sh'
;		 		spawn,'sh modis_dn_mod03_2.sh'

;		 		spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_myd02.sh > modis_dn_myd02_1.sh'		
;	  	 		spawn,'sed s/001/'+day+'/g'+' modis_dn_myd02_1.sh > modis_dn_myd02_2.sh'		
;	  	 		spawn,'sh modis_dn_myd02_2.sh'

;		 		spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_myd06.sh > modis_dn_myd06_1.sh'
;		 		spawn,'sed s/001/'+day+'/g'+' modis_dn_myd06_1.sh > modis_dn_myd06_2.sh'	
;	  	 		spawn,'sh modis_dn_myd06_2.sh'
 ;   			mod03_dir = '/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT/MYD03/'+year+$
	;			'/'+day+'/'
;				mod03_fname=file_search(mod03_dir,'*'+year+day+'.'+hhmm+'.*')
  ;  			mod02_dir = '/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT/MYD021KM/'+year+$
;				'/'+day+'/'
;				mod02_fname=file_search(mod02_dir,'*'+year+day+'.'+hhmm+'.*')

;			endif	

	   endelse ; end to select file
	endif ; endif doesn't match the 	
	
;	print,mod03_fname
	;== to get tropopause height ===
	caldat,Julday(1,float(day),2007),month,date_day
	month_1=strmid(strcompress(string(month/100.0)),3,2)
	date_day_1=strmid(strcompress(string(date_day/100.0)),3,2)
	
	merra2_fname=strcompress('/u/sciteam/yulanh/scratch/MERRA-2/2007/'+'MERRA2_300.inst1_2d_asm_Nx.2007'+$
		string(month_1)+string(date_day_1)+'.nc4',/rem)
	
	merra2_pre2hgt,merra2_fname,tropopause_hgt,merra2_trop_t

	lonscp=round((lon+180)/0.625)
	ind=where(lonscp lt 0)
	if ind[0] ne -1 then lonscp[ind]=0
	ind=where(lonscp gt 575)
	if ind[0] ne -1 then lonscp[ind]=575
	latscp=round((lat+90)/0.5)
	ind=where(latscp lt 0)
	if ind[0] ne -1 then latscp[ind]=0
	ind=where(latscp gt 360)
	if ind[0] ne -1 then latscp[ind]=360

	;==read MODIS data  ====
	read_modis_1,mod03_fname[0],'Latitude',modlat
	read_modis_1,mod03_fname[0],'Longitude',modlon
;	read_modis_1,mod03_fname[0],'SensorZenith',SensorZenith
;	read_modis_1,mod03_fname[0],'SensorAzimuth',SensorAzimuth

	read_modis_02,mod02_fname[0],rad_062,rad_11,rad_67

	modis_bt11_gs=planck_c2/(11.03*alog(planck_c1/(temporary(rad_11)*11.03^5.0)+1))
	modis_bt67_gs=planck_c2/(6.72*alog(planck_c1/(temporary(rad_67)*6.72^5.0)+1))
;	modis_bt11_gs=gauss_smooth(bt11,1)
;	modis_bt67_gs=gauss_smooth(bt67,1)


	;==== parallax correction === 
	;=== search around OT si 150+/-
	tpNs=601
;	colocate_lat_parallax=fltarr(tpNs)
;	colocate_lon_parallax=fltarr(tpNs)
	colocate_lat=fltarr(tpNs)
	colocate_lon=fltarr(tpNs)
	colocate_bt11=fltarr(tpNs)
	colocate_bt11_np=fltarr(tpNs)
	colocate_bt67=fltarr(tpNs)
	colocate_bt67_np=fltarr(tpNs)
	colocate_ref062=fltarr(tpNs)
	topmost_dbz=fltarr(tpNs)
	tptropopause=fltarr(tpNs)
	tptropopause_t=fltarr(tpNs)
	tptropopause_stat=fltarr(tpNs)
	tptropopause_stat_1a=fltarr(tpNs)
	tptropopause_stat_2a=fltarr(tpNs)
	tptropopause_stat_3a=fltarr(tpNs)

	tptropopause_wv_1a=fltarr(tpNs)
	tptropopause_wv_2a=fltarr(tpNs)
	tptropopause_wv_3a=fltarr(tpNs)

	tptropopause_merra=fltarr(tpNs)
	tptropopause_t_merra=fltarr(tpNs)
	
	ii = 0
	tplowscp=OTinfo_si-tpNs/2
	if tplowscp lt 0 then tplowscp=0
	tpupscp=OTinfo_si+tpNs/2
	if tpupscp gt n_elements(lat)-1 then tpupscp=n_elements(lat)-1

	For si=tplowscp,tpupscp Do begin ;study in the 601 window

		; get tropopause & calculate potential temprature 
    	hgt_ind=where(ec_hgt gt 0)
		dimh=n_elements(ec_hgt)
    	lapse_rate=fltarr(dimh)

		tptemp=temp[*,si]
		tp_prof_p=press[*,si]
		tp_wv_sh=wv_sh[*,si]
		; transfer sh to mass
		wv_mass_prof=1000*tp_wv_sh*tp_prof_p*air_molar_mass/(gas_constant*tptemp) ;mg m-3
		
    	For hi=Dimh-1,1,-1 Do Begin
        if (hi eq 0) then lapse_rate[hi]=0.0
        if (hi gt 0 and tptemp[hi] gt 0  and tptemp[hi-1] gt 0) then lapse_rate[hi]=0-(tptemp[hi]-tptemp[hi-1])/(ec_hgt[hi]-ec_hgt[hi-1])
   	 	EndFor
    	;find the tropopause height based on WMO definition
    	scp_lapserate=0
    	zeronum=0
    	For hi=Dimh-1,9,-1 Do Begin
         ind=where(lapse_rate[hi-9:hi] gt 2.0,count)

         if (count eq 0 and abs(lapse_rate[hi]) gt 0 and (lapse_rate[hi]-2) lt 0 and ec_hgt[hi] gt 3) then begin
              scp_lapserate=hi
              tptropopause[si-tplowscp]=ec_hgt[scp_lapserate]
              ;print,si,scp_lapserate,tp_hgt[scp_lapserate]
              break
         endif
    	EndFor
	
	
    	theta_T1=tptemp(scp_lapserate+13)*(1000.0/tp_prof_p((scp_lapserate+13)))^0.286        
    	theta_T2=tptemp(scp_lapserate)*(1000/tp_prof_p(scp_lapserate))^0.286
    	stat_high=(theta_T2-theta_T1)/3.117
		tptropopause_t[si-tplowscp]=tptemp[scp_lapserate]
		tptropopause_stat[si-tplowscp]=stat_high	

    	theta_T1=tptemp(scp_lapserate-4)*(1000.0/tp_prof_p((scp_lapserate-4)))^0.286        
    	stat_high=(theta_T1-theta_T2)/0.960
		tptropopause_stat_1a[si-tplowscp]=stat_high	

    	theta_T1=tptemp(scp_lapserate-8)*(1000.0/tp_prof_p((scp_lapserate-8)))^0.286        
    	stat_high=(theta_T1-theta_T2)/1.918
		tptropopause_stat_2a[si-tplowscp]=stat_high	

    	theta_T1=tptemp(scp_lapserate-13)*(1000.0/tp_prof_p((scp_lapserate-13)))^0.286        
    	stat_high=(theta_T1-theta_T2)/3.118
		tptropopause_stat_3a[si-tplowscp]=stat_high	

		tptropopause_wv_1a[si-tplowscp]=mean(tp_wv_sh[scp_lapserate-4:scp_lapserate])	
		tptropopause_wv_2a[si-tplowscp]=mean(tp_wv_sh[scp_lapserate-8:scp_lapserate])	
		tptropopause_wv_3a[si-tplowscp]=mean(tp_wv_sh[scp_lapserate-12:scp_lapserate])	
	
		
		
		;calculate the topmost layer of -27dbz 
		tp_dbz=radar_ze[*,si]
		tp_hgt=height[*,si]
		tptropopause_merra[ii]=tropopause_hgt[lonscp[si],latscp[si],hour_1]
		tptropopause_t_merra[ii]=merra2_trop_t[lonscp[si],latscp[si],hour_1]

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
        endif ; end topmost height

		if (si ge 0) and (si le Ns-1) then begin	
			tpcldlat=lat[si]
			tpcldlon=lon[si]
;			cth_parallax=topmost[si]
;  			r_parallax=cth_parallax*earth_radius*tan(SensorZenith*pi/180.0)/(earth_radius-cth_parallax)
;    		modlat_parallax=modlat+r_parallax*cos(pi+SensorAzimuth*pi/180.0)/earth_radius
;   	 		modlon_parallax=modlon+r_parallax*sin(pi+SensorAzimuth*pi/180.0)/(earth_radius*cos(modlat*pi/180.0))

;			diffdis=sqrt(abs(tpcldlon-modlon_parallax)^2.0+abs(tpcldlat-modlat_parallax)^2.0)
;			disind =where(min(diffdis) eq diffdis)
;			res_scp=array_indices(modlat,disind)

;			if diffdis[disind[0]] le 0.05 then begin
;				colocate_lat_parallax[ii]=modlat_parallax[res_scp[0],res_scp[1]]
;				colocate_lon_parallax[ii]=modlon_parallax[res_scp[0],res_scp[1]]
;;				colocate_bt11[ii] = modis_bt11_gs[res_scp[0],res_scp[1]]
;				colocate_bt67[ii] = modis_bt67_gs[res_scp[0],res_scp[1]]
;				colocate_ref062[ii]=rad_062[res_scp[0],res_scp[1]]
;			print,ii,diffdis[disind[0]],tpcldlat,tpcldlon,modlon_parallax[res_scp[0],res_scp[1]],modlat_parallax[res_scp[0],res_scp[1]],modis_bt11_gs[res_scp[0],res_scp[1]]
;			endif

			diffdis=sqrt(abs(tpcldlon-modlon)^2.0+abs(tpcldlat-modlat)^2.0)
			disind =where(min(diffdis) eq diffdis)
			res_scp=array_indices(modlat,disind)
		;	print,diffdis[disind[0]],tpcldlat,tpcldlon,modlon[res_scp[0],res_scp[1]],modlat[res_scp[0],res_scp[1]]
			if diffdis[disind[0]] le 0.05 then begin
				colocate_lat[ii]=modlat[res_scp[0],res_scp[1]]
				colocate_lon[ii]=modlon[res_scp[0],res_scp[1]]
				colocate_bt11_np[ii] = modis_bt11_gs[res_scp[0],res_scp[1]] ; no parallel 
				colocate_bt67_np[ii] = modis_bt67_gs[res_scp[0],res_scp[1]]
				colocate_ref062[ii]=rad_062[res_scp[0],res_scp[1]]
;			print,ii,diffdis[disind[0]],tpcldlat,tpcldlon,modlon_parallax[res_scp[0],res_scp[1]],modlat_parallax[res_scp[0],res_scp[1]],modis_bt11_gs[res_scp[0],res_scp[1]]
			endif

;			if (topmost[si] lt 0) then begin ; if topmost height lt 0, no parallax data
;				colocate_bt11[ii] = modis_bt11_gs[res_scp[0],res_scp[1]]
;				colocate_bt67[ii] = modis_bt67_gs[res_scp[0],res_scp[1]]
;				colocate_ref062[ii]=rad_062[res_scp[0],res_scp[1]]
;				colocate_lat_parallax[ii]=modlat[res_scp[0],res_scp[1]]
;				colocate_lon_parallax[ii]=modlon[res_scp[0],res_scp[1]]
;			print,ii,diffdis[disind[0]],tpcldlat,tpcldlon,modlon_parallax[res_scp[0],res_scp[1]],modlat_parallax[res_scp[0],res_scp[1]],modis_bt11_gs[res_scp[0],res_scp[1]]
;			endif
	
		endif
	ii=ii+1
	EndFor ; end for the window

    x=findgen(tpNs)
	x2=findgen(tpNs)*1.0
	pp=imsl_cssmooth(x,colocate_bt11_np)
	colocate_bt11_sm=imsl_spvalue(x2,pp)
	pp=imsl_cssmooth(x,colocate_bt67_np)
	colocate_bt67_sm=imsl_spvalue(x2,pp)
	;first derivative 
	first_deriv_bt11=deriv(x*0.1,colocate_bt11_sm)
	;second derivative
	second_deriv_bt11=deriv(x*0.1,first_deriv_bt11)
	;the center of the OT
	tpcldlon = lon[tplowscp:tpupscp]
	tpcldlat = lat[tplowscp:tpupscp]
	tpheight = height[*,tplowscp:tpupscp]

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
		; only deal with the ots not at the end 
		;search for OT center
		tpbt11sm=colocate_bt11_sm[tpotscp_1]
		btind=where(min(tpbt11sm) eq tpbt11sm)
		btind=btind[0]
	
		tptropopause_t2=tptropopause_t[tpotscp_1[btind]]
	
		if (tpotscp_1[btind] ge 15 and tpotscp_1[btind] le tpNs-15 and tpbt11sm[btind] lt tptropopause_t2) then begin ; make sure not at the edge of image ; make sure colder than tropopause

;			tpotsize_cldsat=1.8*(tpotscp_1[n_elements(tpotscp_1)-1] - tpotscp_1[0])			
;			tpot_trop_cld=mean(topmost_dbz[ind]- tptropopause[ind])

			low_scp= tpotscp_1[btind]-15 ; set the radius of 15 km
			if low_scp lt 0 then low_scp=0
			up_scp = tpotscp_1[btind]+15
			if up_scp gt tpNs-1 then up_scp=tpNs-1
			; obtain the OT size
			left_deriv=reverse(second_deriv_bt11[low_scp:tpotscp_1[btind]])
			right_deriv=second_deriv_bt11[tpotscp_1[btind]:up_scp]
			otleftind=where(left_deriv le 0)
			otrightind=where(right_deriv le 0)

			if otleftind[0] ne -1 and otrightind[0] ne -1 then begin
			left_scp=tpotscp_1[btind]-otleftind[0]
			right_scp=tpotscp_1[btind]+otrightind[0]
			tpotsize_modsize=1.8*(right_scp-left_scp+1)
			tpmodotscp=fltarr(2)
			tpmodotscp[0]=right_scp
			tpmodotscp[1]=left_scp

			;tpot_trop_mod=mean(topmost_dbz[left_scp:right_scp]-tptropopause[left_scp:right_scp])
			otmean_bt11=mean(colocate_bt11_np[left_scp:right_scp]) ; second derivative method
			otmean_bt67=mean(colocate_bt67_np[left_scp:right_scp]) ; second derivative method


			; only when otmean_bt67 gt otmean_bt11 and otmean_bt11 coloder than tropopause, get the information of cirrus

			IF (otmean_bt67 gt otmean_bt11 and otmean_bt11 lt tptropopause_t2) Then Begin
				;thick cirrus bt excluding OT BT11
		    	;tphick cirrus size and bt11, based on second derivative method
				left_bt=reverse(colocate_bt11_sm[0:left_scp]) ; exclude the OT region
				right_bt=colocate_bt11_sm[right_scp:tpNs-1]
				btd_67_11=colocate_bt67_sm-colocate_bt11_sm
;				btdleftind=where(left_bt le 260 and reverse(btd_67_11[0:tpotscp_1[btind[0]]]) ge -3,btdleftcnt)
				btdleftind=where(left_bt le 260 ,btdleftcnt)
				Nleftci=0
				for cii=0,btdleftcnt-2 do begin
					if (btdleftind[cii+1]-btdleftind[cii]) eq 1  then Nleftci=Nleftci+1
					if (btdleftind[cii+1]-btdleftind[cii]) gt 1  then break 
				endfor
;				btdrightind=where(right_bt le 260 and btd_67_11[tpotscp_1[btind[0]]:tpNs-1] ge -3,btdrightcnt)
				btdrightind=where(right_bt le 260,btdrightcnt)
				Nrightci=0
				for cii=0,btdrightcnt-2 do begin
					if (btdrightind[cii+1] - btdrightind[cii]) eq 1 then Nrightci=Nrightci+1
					if (btdrightind[cii+1] - btdrightind[cii]) gt 1 then break
				endfor					
				
				if Nleftci gt 20 then Nleftci=20
				if Nrightci gt 20 then Nrightci=20
				cileft_scp=left_scp-Nleftci
				ciright_scp=right_scp+Nrightci
				cirrus_bt=0.0

				;get the meanci on the left
			    cirrus_bt_left=0.0
			    cirrus_bt_right=0.0
				if cileft_scp lt left_scp then cirrus_bt_left=total(colocate_bt11_np[cileft_scp:left_scp-1])/n_elements(colocate_bt11_np[cileft_scp:left_scp-1])
				if ciright_scp gt right_scp then cirrus_bt_right=total(colocate_bt11_np[right_scp+1:ciright_scp])/$
				n_elements(colocate_bt11_np[right_scp+1:ciright_scp])
			
			; record the OT information
;			OT_thick_thin_size.add,1.8*(Nleftci+Nrightci+1)
;			OT_size_cldsat.add,tpotsize_cldsat ;add cldsat size
;			OT_size_modis.add,tpotsize_modsize
;			OT_lon.add,tpcldlon[tpotscp_1[btind]]
;			OT_lat.add,tpcldlat[tpotscp_1[btind]]	
;			OT_center_BT11.add,colocate_bt11_np[tpotscp_1[btind]]
;			OT_center_BT67.add,colocate_bt67_np[tpotscp_1[btind]]
;			OT_vis.add,colocate_ref062[tpotscp_1[btind]]
;			OT_above_trop_cld.add,tpot_trop_cld
;			OT_above_trop_mod.add,tpot_trop_mod
;			trop_merra2_T.add,tptropopause_t[tpotscp_1[btind]]
;			OT_location_cld.add,tpotscp_1
;			OT_location_mod.add,tpmodotscp
;			OT_filename.add,strmid(png_fname,0,25)
			printf,u,tpcldlon[tpotscp_1[btind]],tpcldlat[tpotscp_1[btind]],tpotsize_modsize,colocate_bt11_np[tpotscp_1[btind]],otmean_bt11,$
				colocate_bt67_np[tpotscp_1[btind]],otmean_bt67,cirrus_bt_left,cirrus_bt_right,tptropopause_t[tpotscp_1[btind]],$
				tptropopause_t_merra[tpotscp_1[btind]],tptropopause_stat[tpotscp_1[btind]],tptropopause_stat_1a[tpotscp_1[btind]],$
				tptropopause_stat_2a[tpotscp_1[btind]],tptropopause_stat_3a[tpotscp_1[btind]],tptropopause_wv_1a[tpotscp_1[btind]],$
				tptropopause_wv_2a[tpotscp_1[btind]],tptropopause_wv_3a[tpotscp_1[btind]],png_fname,format='(15(f10.4,2x),3(f12.8,2x),46a)'

			EndIf ;end otmean6.7<otmean11 and otmean11 < tropopause t	
		
			endif; end valid subscript
		endif					
	endfor ; end for ot num

  endfor ;endfor OT file

  ;OT_lon1=OT_lon.ToArray()	
  ;OT_lat1=OT_lat.ToArray()	
  ;OT_size_Cldsat1=OT_size_Cldsat.ToArray()	
  ;OT_size_modis1=OT_size_modis.ToArray()	
  ;OT_center_BT11_1=OT_center_BT11.ToArray()	
  ;OT_center_BT67_1=OT_center_BT67.ToArray()	
  ;OT_vis_1=OT_vis.ToArray()	
  ;OT_above_trop_cld_1=OT_above_trop_cld.ToArray()	;the mean difference of ot vs tropopause
  ;OT_above_trop_mod_1=OT_above_trop_mod.ToArray()	
  ;trop_merra2_T1=trop_merra2_T.ToArray()
  ;OT_size_merra_1=OT_size_merra.ToArray()
  ;OT_thick_thin_size_1=OT_thick_thin_size.ToArray()
  ;OT_meanbt_merra_modis_1=OT_meanbt_merra_modis.ToArray()
  ;cirrus_meanbt_modis_1= cirrus_meanbt_modis.ToArray()
  ;OT_meanbt_1= OT_meanbt.ToArray()	

	free_lun,u

  print,systime()
  stop

end
