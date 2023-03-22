
pro download_MODIS_OT 

  dir='/u/sciteam/yulanh/scratch/CloudSat/2B-GEOPROF/'

  fname='OT_selected_2008.txt'
  Nline=file_lines(fname)
  OT_location=strarr(Nline)

  OT_lon=fltarr(Nline)
  OT_lat=fltarr(Nline)

  png_fname=''

  openr,u,fname,/get_lun

  for i=0,Nline-1 do begin
	readf,u,png_fname

	strflag=strmid(png_fname,0,19)
	geoprof_fname=dir + strflag +'_CS_2B-GEOPROF_GRANULE_P1_R05_E02_F00.hdf'
	year=strmid(strflag,0,4)
	day= strmid(strflag,4,3)
	hour=fix(strmid(strflag,7,2))
	minu=fix(strmid(strflag,9,2))
	secd=strmid(strflag,11,2)

	OTinfo_si=fix(strmid(png_fname,20,5))

	read_dardar,geoprof_fname,'Radar_Reflectivity',radar_ze
	
 	read_cloudsat,geoprof_fname,'2B-GEOPROF','Latitude',lat
    read_cloudsat,geoprof_fname,'2B-GEOPROF','Longitude',lon
    read_cloudsat,geoprof_fname,'2B-GEOPROF','Profile_time',time
	time=time/3600.0 ; unit--second
    
	hour_1=hour+floor(time[OTinfo_si])


	minu_1=minu+60*(time[OTinfo_si]-floor(time[OTinfo_si]))

	diff_minu=minu_1-10*floor(minu_1/10)
	if (diff_minu lt 5) then minu_2=string(10*floor(minu_1/10)) else minu_2=string(10*floor(minu_1/10)+5)

	hhmm=strcompress(string(hour_1)+minu_2,/rem)

	spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_mod03.sh > modis_dn_mod03_1.sh'
    spawn,'sed s/001/'+day+'/g'+' modis_dn_mod03_1.sh > modis_dn_mod03_2.sh'
	spawn,'sh modis_dn_mod03_2.sh'
	
	spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_myd06.sh > modis_dn_myd06_1.sh'
    spawn,'sed s/001/'+day+'/g'+' modis_dn_myd06_1.sh > modis_dn_myd06_2.sh'
	spawn,'sh modis_dn_myd06_2.sh'
		
	spawn,'sed s/2315/'+hhmm+'/'+' modis_dn_myd02.sh > modis_dn_myd02_1.sh'
    spawn,'sed s/001/'+day+'/g'+' modis_dn_myd02_1.sh > modis_dn_myd02_2.sh'
	spawn,'sh modis_dn_myd02_2.sh'

;    OT_lon[i]=lon[OTinfo_si]
;    OT_lat[i]=lat[OTinfo_si]

  endfor
  free_lun,u
 
  

  stop

end
