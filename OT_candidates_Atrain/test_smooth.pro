
pro test_smooth

   file='test_smooth.txt'
   ncol=1354
   nrow=2030
   BT11=fltarr(ncol,nrow)
   BT11_sm=fltarr(ncol,nrow)
   var=''
   openr,u,file,/get_lun	
   for i=0,ncol-1 do begin
	for j=0,nrow-1 do begin
	readf,u,var
	var1=strsplit(var,' ',/ext)
	BT11[i,j]=float(var1[0])
	BT11_sm[i,j]=float(var1[1])	
	endfor
   endfor

  im=image(BT11[800:1000,800:1000],rgb_table=33,min_value=150,max_value=300,title='no smooth')
  im1=image(BT11_sm[800:1000,800:1000],rgb_table=33,min_value=150,max_value=300,title='smooth by gaussian_filter fortran')
  BT11_smgs=gauss_smooth(BT11,1,/edge_truncate)
  im2=image(BT11_smgs[800:1000,800:1000],rgb_table=33,min_value=150,max_value=300,title='smooth by gaussian_filter idl')

  
  stop  
end
