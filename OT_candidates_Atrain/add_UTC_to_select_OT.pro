
pro add_UTC_to_select_OT

	dir='/u/sciteam/yulanh/scratch/CloudSat/2B-GEOPROF/2007/'

	fname='OT_property_combine_selected_2007.txt'

	openr,u,fname,/get_lun

	openw,uw,'OT_property_combine_selected_addtime_2007.txt',/get_lun

	i=0
	var=''
	while ~eof(u) do begin

		readf,u,var

		if i eq 0 then printf,uw,'Time(dayhhmmss)'+var+' '+'OT flag  '	
	
		if i ge 1 then begin
			str1=strsplit(var,' ',/ext)

			cldsat_sw=str1[12]

			str2=strsplit(cldsat_sw,'_',/ext)

			array_index=float(str2[2])

			geoprof_fname=dir+strmid(cldsat_sw,0,19)+'_CS_2B-GEOPROF_GRANULE_P1_R05_E02_F00.hdf'
	
			read_cloudsat,geoprof_fname,'2B-GEOPROF','Latitude',lat
 		    read_cloudsat,geoprof_fname,'2B-GEOPROF','Longitude',lon
 	   		read_cloudsat,geoprof_fname,'2B-GEOPROF','Profile_time',time
	
		 	day= strmid(cldsat_sw,4,3)
 	     	hour=fix(strmid(cldsat_sw,7,2))
 	    	minu=fix(strmid(cldsat_sw,9,2))
 			secd=strmid(cldsat_sw,11,2)

			inital_time=hour+minu/60.+secd/3600.		
	
			locat_time=time[array_index]/3600.0+inital_time
		
			hour_1=floor(locat_time)
			minu_1=60*(locat_time-hour_1)
			secd_1=60*(minu_1-floor(minu_1))
			minu_1=floor(minu_1)
			secd_1=floor(secd_1)	

			if (hour_1 ge 24) then begin
				hour_1=hour_1-24
				day_1=fix(day)+1
				day=strmid(strcompress(string(day_1/1000.0)),3,3)
			endif
			
			hour_2=strmid(strcompress(string(hour_1/100.0)),3,2)
		    minu_2=strmid(strcompress(string(minu_1/100.0)),3,2)
		    secd_2=strmid(strcompress(string(secd_1/100.0)),3,2)

			utctime=strcompress(day+':'+hour_2+':'+minu_2+':'+secd_2)
			printf,uw,utctime+'   '+var	
		
		endif
				
	i=i+1
	endwhile

  free_lun,u
  free_lun,uw
end
