
pro test_deriv

   fname='test_deriv.txt'

   win_size=21
   BT11=fltarr(win_size) 
   first_deriv=fltarr(win_size)
   second_deriv=fltarr(win_size)
    x2=findgen(win_size)*1.0

   openr,u,fname,/get_Lun

	k=0
   var=' '
	while ~eof(u) do begin
		readf,u,var
		var1=float(strsplit(var,' ',/ext))
		if k mod 3 eq 0 then bt11=var1
		if k mod 3 eq 1 then first_deriv=var1
		if k mod 3 eq 2 then begin
		second_deriv=var1
    	pp=imsl_cssmooth(x2,bt11)
    	bt11_sm=imsl_spvalue(x2,pp)
		
   		p=plot(bt11,title='bt11'+string(k),yrange=[210,220])
   		p0=plot(bt11_sm,color='r',overplot=p)
   		p1=plot(first_deriv,title='first derive'+string(k),yrange=[-1,1])
   		p2=plot(deriv(bt11),overplot=p1,color='r')
   		p3=plot(second_deriv,title='second derive'+string(k),yrange=[-1,1])
  		p4=plot(deriv(deriv(bt11)),color='r',overplot=p3)

		endif
	k=k+1	
    endwhile
   free_lun,u


stop
end
