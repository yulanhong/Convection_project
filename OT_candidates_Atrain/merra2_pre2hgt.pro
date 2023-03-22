
pro merra2_pre2hgt,fname,h_troppb,tropt

   R=8.3143
   M=0.02896
   g=9.807

   	read_merra2,fname,lon,lat,PS,TS,time,troppb,troppt,troppv,tropt
 
	; pressure to altitude

;	C1=0-(R*tropt)/(M*g)
;	trophb=C1*alog(troppb/PS)/1000.
;	tropht=C1*alog(troppt/PS)/1000.
;	trophv=C1*alog(troppv/PS)/1000.

;	p=pn[Tb/(Tb+Lb*(h-hb)]^(g*M/(R*Lb))
	Lb=-0.0065 ;K/m
	h0=0.0

	C1=0-R*Lb/(g*M)

	C2=troppb/PS
	C3=C2^C1
	h_troppb=(TS*(C2^C1-1)/Lb+h0)/1000.0
	
	;plot 
	;for i=0,23 do begin

	;C2=troppt/PS
	;C3=C2^C1
	;h_troppt=(TS*(C2^C1-1)/Lb+h0)/1000.0
	;im2=image(reform(h_troppt[*,*,i]),rgb_table=33,title='Thermal estimate'+string(i+1)+'Z',$
;		min_value=5,max_value=20)	
;	ct2=colorbar(target=im2)
;
;	
;	C2=troppv/PS
;	C3=C2^C1
;	h_troppv=(TS*(C2^C1-1)/Lb+h0)/1000.0
;	im3=image(reform(h_troppv[*,*,i]),rgb_table=33,title='EPV estimate'+string(i+1)+'Z',min_value=5,max_value=20)	
;	ct3=colorbar(target=im3)

;	endfor ;end hour

end
