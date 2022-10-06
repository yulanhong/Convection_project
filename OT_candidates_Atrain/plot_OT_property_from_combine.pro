
pro plot_OT_property_from_combine

  fname=file_search('/data/keeling/a/yulanh/mydata/OT_modis','*OTnOT_2007.txt')

;  fname='OT_property_combine_bf_OTnOT_2007.txt'

  nf=n_elements(fname)

;  openw,u,'OT_property_2008.txt',/get_lun

;  printf,u,'center_lon, center_lat,OT_diameter, center_OT_bt11,mean_OT_bt11,center_OT_bt6.7,mean_OT_bt6.7,mean_ci_bt11_left,mean_ci_bt11_right,tropopuase_temp_ecmwf,tropopause_temp_merra2,tropopause_stability'
  lon=fltarr(1)
  lat=fltarr(1)
  OT_size=fltarr(1)
  center_OT_bt11=fltarr(1)
  center_OT_bt67=fltarr(1)
  mean_OT_bt11=fltarr(1)
  mean_OT_bt67=fltarr(1)
  mean_ci_bt11_left=fltarr(1)
  mean_ci_bt11_right=fltarr(1)
  trop_temp_ecmwf=fltarr(1)
  trop_temp_merra=fltarr(1)
  trop_temp_ss=fltarr(1)
  trop_wv_1a=fltarr(1)
  trop_wv_2a=fltarr(1)
  trop_wv_3a=fltarr(1)
  OT_flag=intarr(1)


 for fi=0,Nf-1 do begin

   if (strlen(fname[fi]) ge 76) then begin

   print,fname[fi]

   openr,u1,fname[fi],/get_lun

	i=0
    var=' '

   while ~eof(u1) do begin
	 readf,u1,var
	 if i ge 1 then begin
		var1=strsplit(var,' ',/ext)
				lon=[lon,float(var1[0])]
				lat=[lat,float(var1[1])]
 			OT_size=[OT_size,float(var1[2])]
	 center_OT_bt11=[center_OT_bt11,float(var1[3])]
	   mean_OT_bt11=[mean_OT_bt11,float(var1[4])]
	 center_OT_bt67=[center_OT_bt67,float(var1[5])]
	   mean_OT_bt67=[mean_OT_bt67,float(var1[6])]
  mean_ci_bt11_left=[mean_ci_bt11_left,float(var1[7])]	
 mean_ci_bt11_right=[mean_ci_bt11_right,float(var1[8])]	
	trop_temp_ecmwf=[trop_temp_ecmwf,float(var1[9])]
	trop_temp_merra=[trop_temp_merra,float(var1[10])]
	   trop_temp_ss=[trop_temp_ss,float(var1[11])]
  		 trop_wv_1a=[trop_wv_1a,float(var1[15])]
  		 trop_wv_2a=[trop_wv_2a,float(var1[16])]
  		 trop_wv_3a=[trop_wv_3a,float(var1[17])]
			OT_flag=[OT_flag,fix(var1[19])]
	 endif
	 i=i+1
   endwhile

	free_lun,u1

   endif ; 

 endfor
 symsz=2 
  ind=where(mean_OT_bt11 lt trop_temp_merra and OT_flag eq 1)
  OTsize_1=OT_size[ind]
  center_OT_bt11_1=center_OT_bt11[ind]
  center_OT_bt67_1=center_OT_bt67[ind]
    mean_OT_bt11_1=mean_OT_bt11[ind]
    mean_OT_bt67_1=mean_OT_bt67[ind]
 mean_ci_bt11_left_1=mean_ci_bt11_left[ind]
 mean_ci_bt11_right_1=mean_ci_bt11_right[ind]
 trop_temp_merra_1 = trop_temp_merra[ind]
 lon_1=lon[ind]
 lat_1=lat[ind]

 ; to plot wv
 
  p=plot(lat_1,trop_wv_1a[ind],linestyle='',symbol='circle',sym_filled=0,sym_size=symsz,sym_color='black',xtitle='Latitude',ytitle='WV density(mg/m3)',yrange=[0.000001,0.0001])
  p0=plot(lat_1,trop_wv_2a[ind],linestyle='',symbol='circle',sym_filled=0,sym_size=symsz,sym_color='red',overplot=p);,xtitle='Latitude',ytitle='WV density(mg/m3)',yrange=[0.000001,0.0001])
  p1=plot(lat_1,trop_wv_3a[ind],linestyle='',symbol='circle',sym_filled=0,sym_size=symsz,sym_color='green',overplot=p);,xtitle='Latitude',ytitle='WV density(mg/m3)',yrange=[0.000001,0.0001])
 
  stop
;  p=plot(lat_1,center_OT_bt11_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',xtitle='Latitude',ytitle='Center BT11',yrange=[170,250])
;  p1=plot(lat_1,mean_OT_bt11_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',xtitle='Latitude',ytitle='Ave BT11',yrange=[170,250])
;  p2=plot(lat_1,center_OT_bt67_1-center_OT_bt11_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',xtitle='Latitude',ytitle='Center BT6.7- BT11',yrange=[0,10])
;  p3=plot(lat_1,mean_OT_bt67_1-mean_OT_bt11_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',xtitle='Latitude',ytitle='Ave BT6.7- BT11',yrange=[0,10])
 
  ave_cibt=(mean_ci_bt11_left_1+mean_ci_bt11_right_1)/2.
;  p4=plot(lat_1,mean_OT_bt11_1-ave_cibt,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',xtitle='Latitude',ytitle='Ave OT BT - Ci BT');,yrange=[0,10])
 
;  p5=plot(lat_1,mean_OT_bt11_1-trop_temp_merra_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',xtitle='Latitude',ytitle='OT BT11 - Merra2 T');  

;  p6=plot(mean_OT_bt11_1-ave_cibt,mean_OT_bt67_1-mean_OT_bt11_1,linestyle='',symbol='circle')
 

 ; barplot
 his=histogram(OTsize_1,binsize=2.5,max=25,min=1)
 x=findgen(n_elements(his))*2.5+1
 b1=barplot(x,his,fill_color='grey',xtitle='OT Diameter (km)', ytitle='Number')

 cihis=histogram(ave_cibt-mean_OT_bt11_1,binsize=3,min=0,max=35)
 pdf2cdf,cihis,cicdf
 x1=findgen(n_elements(cihis))*3
 b2=barplot(x1,float(cihis)/total(cihis),fill_color='grey',xtitle='Ave Ci BT11 - Ave OT BT11', ytitle='Probability')
 p7=plot(x1,float(cicdf)/total(cihis),thick=2,overplot=b2) 

 btdhis=histogram(mean_OT_bt67_1-mean_OT_bt11_1,binsize=0.5,max=7,min=0)
 pdf2cdf,btdhis,btdcdf
 x3=findgen(n_elements(btdhis))*0.5
 b3=barplot(x3,float(btdhis)/total(btdhis),fill_color='grey',xtitle='Ave BT6.7 - Ave BT11', ytitle='Probability')
 p7=plot(x3,float(btdcdf)/total(btdhis),thick=2,overplot=b3) 
 
 btdhis_1=histogram(mean_OT_bt11_1-trop_temp_merra_1,binsize=2,max=0,min=-17)
 pdf2cdf,reverse(btdhis_1),btdcdf_1
 x4=findgen(n_elements(btdhis_1))*2-17
 b4=barplot(x4,float(btdhis_1)/total(btdhis_1),fill_color='grey',xtitle='Ave BT11 - Merra2 Tropopause T', ytitle='Probability')
 p7=plot(x4,float(reverse(btdcdf_1))/total(btdhis_1),thick=2,overplot=b4) 

 ; plot map 
 stop 

  mp=map('Geographic',limit=maplimit,transparency=30)
  grid=mp.MAPGRID
  grid.label_position=0
  grid.linestyle='dotted'
  grid.grid_longitude=60
  grid.grid_latitude=30
  grid.font_size=12
  mc=mapcontinents(/continents,transparency=30)
  mc['Longitudes'].label_angle=0
  mp.scale,0.85,1

  for i=0,n_elements(OTsize_1)-1 do begin
	  tplonarr=fltarr(1)
      tplatarr=fltarr(1)
      tplonarr[0]=lon_1[i]
      tplatarr[0]=lat_1[i]
      p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=OTsize_1[i]/10.0,linestyle='',overplot=mp,color='r',transparency=50)
  endfor

  ; to add the label to represent the scale
   tplatarr[0]=-110
   tplonarr[0]=-150
   symsz=5
   p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=symsz/10.,linestyle='',overplot=mp,color='r',transparency=50)
 ;    t=text(0.25,0.10,'5',color='black')

   tplonarr[0]=-125
   symsz=10
   p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=symsz/10.,linestyle='',overplot=mp,color='r',transparency=50)
;    t=text(0.37,0.10,'10',color='black')

     tplonarr[0]=-90
     symsz=15
     p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=symsz/10.,linestyle='',overplot=mp,color='r',transparency=50)
 ;    t=text(0.49,0.10,'15',color='black')

    tplonarr[0]=-30
    symsz=20
    p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=symsz/10.,linestyle='',overplot=mp,color='r',transparency=50)
 ;    t=text(0.61,0.10,'15',color='black')

    tplonarr[0]=0
    symsz=25
    p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=symsz/10.,linestyle='',color='r',transparency=50,overplot=mp)
 ;    t=text(0.73,0.10,'25',color='black')

  stop
end
