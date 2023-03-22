
pro plot_OT_map

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
	geoprof_fname=dir+strmid(png_fname,0,19)+'_CS_2B-GEOPROF_GRANULE_P1_R05_E02_F00.hdf'

	OTinfo_si=fix(strmid(png_fname,20,5))

	read_dardar,geoprof_fname,'Radar_Reflectivity',radar_ze
	
 	read_cloudsat,geoprof_fname,'2B-GEOPROF','Latitude',lat
    read_cloudsat,geoprof_fname,'2B-GEOPROF','Longitude',lon

    OT_lon[i]=lon[OTinfo_si]
    OT_lat[i]=lat[OTinfo_si]

  endfor
  free_lun,u
 


 mp=map('Geographic',limit=maplimit,transparency=30)
 grid=mp.MAPGRID
 grid.label_position=0
 grid.linestyle='dotted'
 grid.grid_longitude=60
 grid.grid_latitude=30
; grid.font_size=fontsz-3
  mc=mapcontinents(/continents,transparency=30)
  mc['Longitudes'].label_angle=0


 p=plot(OT_lon,OT_lat,symbol='circle',sym_filled=1,sym_size=0.9,linestyle='',overplot=mp)
  stop

end
