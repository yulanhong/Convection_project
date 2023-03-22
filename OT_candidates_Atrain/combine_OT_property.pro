
pro combine_OT_property

fname=file_search('/u/sciteam/yulanh/mydata/OT_modis/CCM/post_process_OT','*_bf_*')

  nf=n_elements(fname)


; openr,u,'OT_property_combine_select_2007.txt',/get_lun ; the selected OT to give classficiation
  res=read_ascii('OT_property_combine_select_2008.txt',data_start=1)
  res=res.(0)
  alllon=res[0,*]
  alllat=res[1,*]
  allot=res[2,*]
  allbt=res[3,*]

  openw,u,'OT_property_combine_bf_OTnOT_2008.txt',/get_lun
;  openw,u2,'OT_information_update_selected_2007.txt',/get_lun
 printf,u,'center_lon, center_lat,OT_diameter, min_OT_bt11,mean_OT_bt11,min_OT_bt6.7,mean_OT_bt6.7,mean_ci_bt11_left,mean_ci_bt11_right,tropopuase_temp_ecmwf,tropopause_temp_merra2,tropopause_ss_b3km,tropopause_ss_a1km,tropopause_ss_a2k,tropopause_ss_a3k,tropopause_wv_a1k,tropopause_wv_a2k,tropopause_wv_a3k,CloudSat_Swath,OT_flag'

  Numot=0 
 for fi=0,Nf-1 do begin

  str=strsplit(fname[fi],'_',/ext)
  year=str[5]
 
 if (year eq '2008') then begin
;  if (strlen(fname[fi]) ge 86) then begin
  print,fname[fi]

   openr,u1,fname[fi],/get_lun

	i=0
    var=' '
   while ~eof(u1) do begin
	 readf,u1,var


	 if i ge 1 then begin


		str1=strsplit(var,' ',/ext)
		lon=float(str1[0])
		lat=float(str1[1])
		ot=float(str1[2])
 		bt=float(str1[3])
		ind=where(lon eq alllon and lat eq alllat and ot eq allot and bt eq allbt,count)

		if count eq 0 then OTflag=0
		if count gt 0 then begin
			Numot=Numot+1	
			OTflag=1
		endif
	;	printf,u2,str1[12]
		OTflag_1=strcompress(string(OTflag),/rem)
		 printf,u,var+'    '+OTflag_1

	 endif
	 i=i+1
   endwhile

	free_lun,u1
  endif
 endfor

  free_lun,u
;  free_lun,u2

  stop
end
