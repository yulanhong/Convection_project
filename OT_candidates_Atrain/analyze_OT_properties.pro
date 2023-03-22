

pro analyze_OT_properties

	fname=file_search('/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT','*dbz25.sav')


	Nf=n_elements(fname)
	NOT_lat=list()
	NOT_lon=list()
	NOT_BT11=list()
	NOT_BT67=list()
	NOT_cldsize=list()
	NOT_modsize=list()
	NOT_abv_tpcld=list()
	NOT_abv_tpmod=list()
    Ntrop_T=list()

	for fi=0,Nf-1 do begin
		restore,fname[fi]

		ind=where(OT_size_cldsat1 gt 0,cnt)

		OT_BT11_2=OT_BT11_1[ind]
		OT_BT67_2=OT_BT67_1[ind]
		OT_Lat2 = OT_Lat1[ind]
		OT_Lon2 = OT_Lon1[ind]
		OT_Size_cldsat2=OT_Size_cldsat1[ind]
		OT_Size_modis2 =OT_Size_modis1 [ind]
		OT_vis_2 = OT_vis_1 [ind]	
		OT_abv_trop_cld_2 = OT_above_trop_cld_1[ind]
		OT_abv_trop_mod_2 = OT_above_trop_mod_1[ind]		
		 TROP_MERRA2_T2 = Trop_merra2_t1[ind]  

		for Ni=0,cnt-1 do begin
			ind1=where(OT_lat2[ni] eq OT_lat2,cnt1)
			if OT_lat2[ni] gt 0 and OT_lat2[ni] le 20 then print,OT_lat2[ni]
			if cnt1 eq 1 then begin
				NOT_lat.add,OT_lat2[ni]
				NOT_lon.add,OT_lon2[ni]
				NOT_BT11.add,OT_BT11_2[ni]
				NOT_BT67.add,OT_BT67_2[ni]
				NOT_cldsize.add,OT_Size_cldsat2[ni]
				NOT_modsize.add,OT_Size_modis2[ni]
				NOT_abv_tpcld.add,OT_abv_trop_cld_2[ni]
				NOT_abv_tpmod.add,OT_abv_trop_mod_2[ni]
    			Ntrop_T.add,Trop_merra2_t2[ni]
			endif
			if cnt1 gt 1 and ind1[0] eq ni then begin
				NOT_lat.add,OT_lat2[ni]
				NOT_lon.add,OT_lon2[ni]
				NOT_BT11.add,OT_BT11_2[ni]
				NOT_BT67.add,OT_BT67_2[ni]
				NOT_cldsize.add,OT_Size_cldsat2[ni]
				NOT_modsize.add,OT_Size_modis2[ni]
				NOT_abv_tpcld.add,OT_abv_trop_cld_2[ni]
				NOT_abv_tpmod.add,OT_abv_trop_mod_2[ni]
    			Ntrop_T.add,Trop_merra2_t2[ni]
			endif
		print,fname[fi]
		help,NOT_lat
		EndFor

	endfor ;end allfile
	
	
	NOT_lat1=NOT_lat.ToArray()
	NOT_lon1=NOT_lon.ToArray()
	NOT_BT11_1=NOT_BT11.ToArray()
	NOT_BT67_1=NOT_BT67.ToArray()
	NOT_cldsize_1=NOT_cldsize.ToArray()
	NOT_modsize_1=NOT_modsize.ToArray()
	NOT_abv_tpcld_1=NOT_abv_tpcld.ToArray()
	NOT_abv_tpmod_1=NOT_abv_tpmod.ToArray()
	Ntrop_t1=Ntrop_t.ToArray()

	symsz=1.5
    p=plot(NOT_lat1,NOT_cldsize_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',ytitle='OT Diameter (km)',xtitle='Latitude')
	p1=plot(NOT_lat1,NOT_modsize_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='red',overplot=p)

	;get histogram
 	his=histogram(NOT_cldsize_1,binsize=2.5,max=80,min=0)	
 	his1=histogram(NOT_modsize_1,binsize=2.5,max=80,min=0)	
	x=findgen(n_elements(his))*2.5
	b1=barplot(x,his,index=0, Nbars=2,fill_color='green',xtitle='OT Diameter (km)',ytitle='Number')
	b2=barplot(x,his1,index=1, Nbars=2,fill_color='yellow',/overplot)

	c=plot(NOT_lat1,NOT_BT11_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',xtitle='Latitude',ytitle='BT11')
	c2=plot(NOT_lat1,NOT_BT11_1-Ntrop_t1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='red',xtitle='Latitude',ytitle='BT11-Tropopause T')
	
	c1=plot(NOT_lat1,NOT_BT67_1-NOT_BT11_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',xtitle='Latitude',ytitle='BT67-BT11')
 	

    h=plot(NOT_lat1,NOT_abv_tpcld_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='black',title='Height above tropopause (km)',xtitle='Latitude')
	h1=plot(NOT_lat1,NOT_abv_tpmod_1,linestyle='',symbol='circle',sym_filled=1,sym_size=symsz,sym_color='red',overplot=h)
		
	stop

end
