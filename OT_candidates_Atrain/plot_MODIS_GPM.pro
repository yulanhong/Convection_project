
pro plot_MODIS_GPM

  fname='record_information_gpm_modis_1905.txt'

  data=read_ascii(fname,delimiter=' ')

  data=data.(0)

  OT_lat=reform(data[1,*])
  proba=reform(data[2,*])
  storm_flag=reform(data[4,*])
  OT_flag=reform(data[3,*])
  cld_flag=reform(data[5,*])

  proba_threshold=findgen(20)*0.05

  trop_accuracy=fltarr(20)
  trop_num = intarr(20)

  mid_accuracy=fltarr(20)
  mid_num = intarr(20)
  
  data=OT_flag

  for i=0,19 do begin

  	ind=where(abs(OT_lat) le 25 and proba ge proba_threshold[i],trop_count)
 	ind1=where(abs(OT_lat) le 25 and data eq 1 and proba ge proba_threshold[i],st_count)
    trop_accuracy[i]=float(st_count)/trop_count
	trop_num[i] = trop_count
    print,'trop accuracy ',float(st_count)/trop_count,trop_count,st_count,proba_threshold[i]

  	ind=where(abs(OT_lat) gt 25 and proba ge proba_threshold[i],trop_count)
 	ind1=where(abs(OT_lat) gt 25 and data eq 1 and proba ge proba_threshold[i],st_count)
    mid_accuracy[i]=float(st_count)/trop_count
    mid_num[i] = trop_count
    print,'mid accuracy ',float(st_count)/trop_count,trop_count,st_count,proba_threshold[i]
  endfor

  plot,proba_threshold,trop_accuracy,background='ffffff'xl,ytitle='Agreement',xtitle='Probability',$
	thick=3,psym=2,charsize=2,yrange=[0.55,0.85],color=0
  oplot,proba_threshold,mid_accuracy,thick=3,psym=4,color=0
stop

  b=barplot(proba_threshold,trop_num,nbars=2,index=0,color='g',fill_color='g',xtitle='Probability',$
	ytitle='Sample')
  b1=barplot(proba_threshold,mid_num,nbars=2,index=1,color='r',fill_color='r',overplot=b)

  stop
end
