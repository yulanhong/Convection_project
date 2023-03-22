
pro screen_OT

 fname=file_search('/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT','*colocated_information.sav')

 Nf=n_elements(fname)

 for fi=0,Nf-1 do begin	
	restore,fname[fi]

	for ni=0,n_elements(OT_location)-1 do begin
		tpotscp=OT_location[ni]
		
	endfor

	stop

 endfor

end
