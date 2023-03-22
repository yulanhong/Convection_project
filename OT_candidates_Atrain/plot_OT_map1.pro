
pro plot_OT_map1

 fname='OT_map_day_2criteria.txt'
 data=read_ascii(fname,delimiter=',')
 data_day=transpose(data.(0))

; fname='OT_map_daygt0.7.txt'
; data=read_ascii(fname,delimiter=',')
; data_dgt=transpose(data.(0))

 fname='OT_map_night_2criteria.txt'
 data=read_ascii(fname,delimiter=',')
 data_night=transpose(data.(0))

; fname='OT_map_nightgt0.7.txt'
; data=read_ascii(fname,delimiter=',')
; data_ngt=transpose(data.(0))

; fname='OTA_pdfgt0.7.txt'
; data=read_ascii(fname,delimiter=',')
; ota_gt07=data.(0)
 
 fname='OTA_pdf_2criteria.txt'
 data=read_ascii(fname,delimiter=',')
 ota_pdf=data.(0)


 xdim=360
 ydim=180
 fontsz=12
 lon=findgen(xdim)*1-180
 lat=findgen(ydim)-90

 margin=[0.10,0.15,0.10,0.10]
 mp=map('Geographic',limit=maplimit,transparency=30,layout=[1,2,1],margin=margin)
 grid=mp.MAPGRID
 grid.label_position=0
 grid.linestyle='dotted'
 grid.grid_longitude=60
 grid.grid_latitude=30
 grid.font_size=fontsz-3

 p=image(data_day,lon,lat,rgb_table=22,overplot=mp,max=15,title='Day')
  mc=mapcontinents(/continents,transparency=30)
  mc['Longitudes'].label_angle=0

 ;p.scale,0.85,0.9


 mp1=map('Geographic',limit=maplimit,transparency=30,layout=[1,2,2],/current,margin=margin)
 grid=mp1.MAPGRID
 grid.label_position=0
 grid.linestyle='dotted'
 grid.grid_longitude=60
 grid.grid_latitude=30
 grid.font_size=fontsz-3

 p=image(data_night,lon,lat,rgb_table=22,overplot=mp1,max=15,title='Night')
  mc=mapcontinents(/continents,transparency=30)
  mc['Longitudes'].label_angle=0

ct=colorbar(target=p,orientation=1,title='Occurrence')

x=findgen(100)*10.0
 b1=barplot(x,ota_pdf[0,*]/float(total(ota_pdf[0,*])),index=0,nbars=2,color='r',xtitle='OTA $(km^2)$',ytitle='Frequency')
 b2=barplot(x,ota_pdf[1,*]/float(total(ota_pdf[1,*])),index=1,nbars=2,color='g',overplot=b1)
  stop

end
