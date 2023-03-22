
pro test_ot_edge

  file='output_bt.txt'

  win_size=21
  Nray=16
  BT11=fltarr(win_size,Nray)

  openr,u,file,/get_Lun
  k=0
  var=' '

  
  while ~eof(u) do begin
       readf,u,var
       var1=float(strsplit(var,' ',/ext))
   	   BT11[*,k]=var1
		
 		if k eq 0 then begin
		  p=plot(bt11[*,k])
		endif else begin
		  p1=plot(bt11[*,k],overplot=p)
		endelse
	
		first_deriv=deriv(bt11[*,k])
	    second_deriv=deriv(first_deriv)
		ind=where(second_deriv le 0)
		print,ind[0],k
        k=k+1
 endwhile
 
 free_lun,u

 stop
end
