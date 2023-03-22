

pro analyze_OT_properties_2

	fname=file_search('/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT','*dbz25_update.sav')

    fontsz=13
	Nf=n_elements(fname)
	NOT_lat=list()
	NOT_lon=list()
	NOT_BT11=list()
	NOT_center_BT67=list()
	NOT_center_BT11=list()
	NOT_cldsize=list()
	NOT_modsize=list()
	NOT_abv_tpcld=list()
	NOT_abv_tpmod=list()
    Ntrop_T=list()
	NOT_merra_modis_size=list()
	NOT_merra_modis_meanbt=list()
    cirrus_meanbt=list()
	cirrus_size=list()
		

	x=findgen(601)
	tpNs=601
	
	for fi=0,Nf-1 do begin
		print,fi
		restore,fname[fi]
	
		print,fname[fi]
		for Ni=0,n_elements(OT_size_cldsat1)-1 do begin

			colocate_file=strcompress(OT_filename[ni]+'_colocated_information.sav',/rem)
			restore,colocate_file		
			print,colocate_file
			ind=where(colocate_bt11_np eq 0)
			colocate_bt11_np[ind]=!values.f_nan
			
			help,OT_location_mod,OT_lat1,colocate_bt11_np	

			if (OT_meanbt_1[ni] eq 0 ) then begin ; correct the location of OT
				ind=where(min(colocate_bt11_np) eq colocate_bt11_np) ; correct the location of OT
						
				OT_lat1[ni]=colocate_lat[ind]
				OT_lon1[ni]=colocate_lon[ind]
	;			OT_BT11_1[ni]=colocate_bt11_np[ind]
;				OT_BT67_1[ni]=colocate_bt67_np[ind]
			 ;first derivative	
			   	first_deriv_bt11=deriv(x*0.1,colocate_bt11_np)
		     ;second derivativ
				second_deriv_bt11=deriv(x*0.1,first_deriv_bt11)
			
			   left_deriv=reverse(second_deriv_bt11[0:ind[0]])
               right_deriv=second_deriv_bt11[ind[0]:tpNs-1]
               leftind=where(left_deriv le 0)
               left_scp=ind[0]-leftind[0]
	           rightind=where(right_deriv le 0)
			   right_scp=ind[0]+rightind[0]
			   newloc=fltarr(2)
			   newloc[0]=right_scp
			   newloc[1]=left_scp
			   OT_location_mod[ni]=newloc
			   OT_size_modis1[ni]=1.8*(right_scp-left_scp+1)
	 
			endif

			mod_otloc=OT_location_mod[ni]
			ave_cld_tropopauset=mean(tptropopause_t[OT_location_cld[ni]])
			ave_mod_tropopauset=mean(tptropopause_t[mod_otloc[1]:mod_otloc[0]])
			ave_cld_ott=mean(colocate_bt11_np[OT_location_cld[ni]],/nan)
			ave_mod_ott=mean(colocate_bt11_np[mod_otloc[1]:mod_otloc[0]],/nan)
			; deal with duplicate OTs
			ind1=where(OT_lat1[ni] eq OT_lat1,cnt1)
			if cnt1 eq 1  and Trop_merra2_t1[ni] gt OT_meanbt_1[ni] then begin
				NOT_lat.add,OT_lat1[ni]
				NOT_lon.add,OT_lon1[ni]
				NOT_BT11.add,OT_meanbt_1[ni]
				NOT_center_BT67.add,OT_center_BT67_1[ni]
				NOT_center_BT11.add,OT_center_BT11_1[ni]
				NOT_cldsize.add,OT_Size_cldsat1[ni]
				NOT_modsize.add,OT_Size_modis1[ni]
				NOT_abv_tpcld.add,OT_above_trop_cld_1[ni]
				NOT_abv_tpmod.add,OT_above_trop_mod_1[ni]
    			Ntrop_T.add,Trop_merra2_t1[ni]
				NOT_merra_modis_size.add, OT_size_merra_1[ni]
 	     		NOT_merra_modis_meanbt.add,OT_meanbt_merra_modis_1[ni]
 	     		cirrus_meanbt.add,cirrus_meanbt_modis_1[ni]
			    cirrus_size.add,OT_thick_thin_size_1[ni]
			endif
			if cnt1 gt 1 and ind1[0] eq ni and Trop_merra2_t1[ni] gt OT_meanbt_1[ni] then begin
				NOT_lat.add,OT_lat1[ni]
				NOT_lon.add,OT_lon1[ni]
				NOT_BT11.add,OT_meanbt_1[ni]
				NOT_center_BT67.add,OT_center_BT67_1[ni]
				NOT_center_BT11.add,OT_center_BT11_1[ni]
				NOT_cldsize.add,OT_Size_cldsat1[ni]
				NOT_modsize.add,OT_Size_modis1[ni]
				NOT_abv_tpcld.add,OT_above_trop_cld_1[ni]
				NOT_abv_tpmod.add,OT_above_trop_mod_1[ni]
    			Ntrop_T.add,Trop_merra2_t1[ni]
				NOT_merra_modis_size.add, OT_size_merra_1[ni]
 	     		NOT_merra_modis_meanbt.add,OT_meanbt_merra_modis_1[ni]
 	     		cirrus_meanbt.add,cirrus_meanbt_modis_1[ni]
			    cirrus_size.add,OT_thick_thin_size_1[ni]
			endif
		
;		print,fname[fi]
;		help,NOT_lat
		EndFor

	endfor ;end allfile
	
	
	NOT_lat1=NOT_lat.ToArray()
	NOT_lon1=NOT_lon.ToArray()
	NOT_BT11_1=NOT_BT11.ToArray()
	NOT_center_BT67_1=NOT_center_BT67.ToArray()
	NOT_center_BT11_1=NOT_center_BT11.ToArray()
	NOT_cldsize_1=NOT_cldsize.ToArray()
	NOT_modsize_1=NOT_modsize.ToArray()
	NOT_abv_tpcld_1=NOT_abv_tpcld.ToArray()
	NOT_abv_tpmod_1=NOT_abv_tpmod.ToArray()
	Ntrop_t1=Ntrop_t.ToArray()
	NOT_merra_modis_size_1=NOT_merra_modis_size.ToArray()
	NOT_merra_modis_meanbt_1=NOT_merra_modis_meanbt.ToArray()
	cirrus_meanbt_1=cirrus_meanbt.ToArray()
	cirrus_size_1=cirrus_size.ToArray()

	symsz=1.5
    p=plot(NOT_lat1,NOT_cldsize_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',ytitle='OT Diameter (km)',xtitle='Latitude')
	p1=plot(NOT_lat1,NOT_modsize_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='red',overplot=p)

	;get histogram
 	his1=histogram(NOT_cldsize_1,binsize=2.5,max=80,min=0)	
 	his=histogram(NOT_modsize_1,binsize=2.5,max=80,min=0)	
	x=findgen(n_elements(his))*2.5
	b1=barplot(x,his,index=0, Nbars=2,fill_color='green',xtitle='OT Diameter (km)',ytitle='Number')
;	b2=barplot(x,his1,index=1, Nbars=2,fill_color='yellow',/overplot)
	; to get the average BT11 for the range of lat
    ind=where(NOT_center_BT11_1 ge 140)
    x=NOT_lat1[ind]
    y=NOT_center_BT11_1[ind]
	res=poly_fit(x,y,3,yband=yerror,sigma=sigma)
    yfit=res[0]+res[1]*x+res[2]*x*x+res[3]*x*x*x 	
    c0=plot(x,y,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',xtitle='Latitude',ytitle='BT11',yrange=[170,250])
	c01=plot(x,yfit,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='red',overplot=c0)
    yfit1=res[0]+sigma[0]+(res[1]+sigma[1])*x+(res[2]+sigma[2])*x*x+(res[3]+sigma[3])*x*x*x 	
	c02=plot(x,yfit1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='pink',overplot=c0)
    yfit2=res[0]-sigma[0]+(res[1]-sigma[1])*x+(res[2]-sigma[2])*x*x+(res[3]-sigma[3])*x*x*x 	
	c02=plot(x,yfit2,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='pink',overplot=c0)
	
	ind=sort(x)
	x1=x[ind]
	y1=yfit1[ind]	
   
	stop

	c=plot(NOT_lat1,NOT_BT11_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',xtitle='Latitude',ytitle='BT11',yrange=[170,250])
	c2=plot(NOT_lat1,NOT_BT11_1-Ntrop_t1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',xtitle='Latitude',ytitle='BT11-Tropopause T')
	
	c3=plot(NOT_lat1,NOT_center_BT67_1-NOT_center_BT11_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',xtitle='Latitude',ytitle='BT67-BT11')


    h=plot(NOT_lat1,NOT_abv_tpcld_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',title='Height above tropopause (km)',xtitle='Latitude')
	h1=plot(NOT_lat1,NOT_abv_tpmod_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='red',overplot=h)
		
	; plot OT on the map
	 mp=map('Geographic',limit=maplimit,transparency=30)
	 grid=mp.MAPGRID
 	 grid.label_position=0
 	 grid.linestyle='dotted'
 	 grid.grid_longitude=60
 	 grid.grid_latitude=30
 	 grid.font_size=fontsz-3
 	 mc=mapcontinents(/continents,transparency=30)
 	 mc['Longitudes'].label_angle=0
	 mp.scale,0.85,1
 
 	 for i=0,n_elements(NOT_lat1)-1 do begin
	 tplonarr=fltarr(1)
	 tplatarr=fltarr(1)
	 tplonarr[0]=NOT_lon1[i]
	 tplatarr[0]=NOT_lat1[i]
 	 p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=NOT_modsize_1[i]/10.0,linestyle='',overplot=mp,color='r',transparency=50)
	 endfor

	; to add the label to represent the scale	
	 tplatarr[0]=-125
	 tplonarr[0]=-150
	 symsz=5
	 p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=symsz/10.,linestyle='',overplot=mp,color='r',transparency=50)
;	 t=text(0.25,0.10,'5',color='black')

	 tplonarr[0]=-125
	 symsz=10
	 p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=symsz/10.,linestyle='',overplot=mp,color='r',transparency=50)
;	 t=text(0.37,0.10,'10',color='black')

	 tplonarr[0]=-90
	 symsz=15
	 p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=symsz/10.,linestyle='',overplot=mp,color='r',transparency=50)
;	 t=text(0.49,0.10,'15',color='black')

	 tplonarr[0]=-30
	 symsz=20
	 p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=symsz/10.,linestyle='',overplot=mp,color='r',transparency=50)
;	 t=text(0.61,0.10,'15',color='black')

	 tplonarr[0]=0
	 symsz=25
	 p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=symsz/10.,linestyle='',color='r',transparency=50,overplot=mp)
;	 t=text(0.73,0.10,'25',color='black')

	; plot cirrus
	 his1=histogram(cirrus_size_1,binsize=50,max=800,min=0)
	 x=findgen(n_elements(his1))*50
	 b1=barplot(x,his1,fill_color='green',xtitle='Cirrus Diameter (km)',ytitle='Number')

	 his2=histogram(cirrus_meanbt_1,binsize=10,max=230,min=170)
	 x=findgen(n_elements(his2))*10
	 b1=barplot(x,his2,fill_color='orange',xtitle='Cirrus Mean BT (K)',ytitle='Number')

	 mp1=map('Geographic',limit=maplimit,transparency=30)
	 grid=mp1.MAPGRID
 	 grid.label_position=0
 	 grid.linestyle='dotted'
 	 grid.grid_longitude=60
 	 grid.grid_latitude=30
 	 grid.font_size=fontsz-3
 	 mc=mapcontinents(/continents,transparency=30)
 	 mc['Longitudes'].label_angle=0
	 mp.scale,0.85,1
 
 	 for i=0,n_elements(NOT_lat1)-1 do begin
	 tplonarr=fltarr(1)
	 tplatarr=fltarr(1)
	 tplonarr[0]=NOT_lon1[i]
	 tplatarr[0]=NOT_lat1[i]
 	 p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=cirrus_size_1[i]/100.0,linestyle='',overplot=mp1,color='g',transparency=50)
     endfor	
	 tplatarr[0]=-125
	 tplonarr[0]=-150
	 symsz=50
	 p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=symsz/100.,linestyle='',overplot=mp1,color='g',transparency=50)

	 tplonarr[0]=-125
	 symsz=200
	 p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=symsz/100.,linestyle='',overplot=mp1,color='g',transparency=50)
;	 t=text(0.37,0.10,'10',color='black')

	 tplonarr[0]=-90
	 symsz=400
	 p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=symsz/100.,linestyle='',overplot=mp1,color='g',transparency=50)
;	 t=text(0.49,0.10,'15',color='black')

	 tplonarr[0]=-30
	 symsz=600
	 p=plot(tplonarr,tplatarr,symbol='circle',sym_filled=1,sym_size=symsz/100.,linestyle='',overplot=mp1,color='g',transparency=50)
;	 t=text(0.61,0.10,'15',color='black')

	; OT BT11 - cirrus mean BT
	ind=where(NOT_BT11_1 le 160 or cirrus_meanbt_1 le 160)
	NOT_BT11_1[ind]=!values.f_nan
	cirrus_meanbt_1[ind]=!values.f_nan
	BT_OTmCirrus=NOT_BT11_1-cirrus_meanbt_1
	BT_OTmCirrus[ind]=!values.f_nan
	
	
	 his3=histogram(BT_OTmCirrus,binsize=3,max=3,min=-25)
	 x=findgen(n_elements(his3))*3-25
	 b1=barplot(x,his3,fill_color='orange',xtitle='OTBT- Ci BT (K)',ytitle='Number')

	 b3=plot(NOT_lat1,BT_OTmCirrus,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',xtitle='Latitude',ytitle='OT BT - Ci BT')

	stop
end
