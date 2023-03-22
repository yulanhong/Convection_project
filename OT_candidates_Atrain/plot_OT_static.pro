
pro plot_OT_static

  fname='OT_property_2008.txt'

  openr,u1,fname,/get_lun

   i=0
  var=' '
  stat_pdf=fltarr(1)
  lat_pdf=fltarr(1)

  while ~eof(u1) do begin
   readf,u1,var
  if i ge 1 then begin
	var1=strsplit(var,' ',/ext)
    tpstat=float(var1[11])
    stat_pdf=[stat_pdf,tpstat]	
	lat_pdf=[lat_pdf,float(var1[1])]
  endif
     i=i+1
  endwhile

   free_lun,u1

 p=plot(lat_pdf,stat_pdf,linestyle=' ',symbol='circle',sym_filled=1)

stop
end
