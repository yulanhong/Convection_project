
pro plot_fit_model

 ; fit from keeling
 c=-3.4755
 b1=0.2031
 b2=0.3488
 b3=0.5856
  
 Nn=100
 x1=findgen(Nn)*0.5-1
 x2=findgen(Nn)*0.25-1
 x3=findgen(Nn)*0.1-1

 fx=c+b1*x1+b2*x2+b3*x3
 z= 1.0/(1+exp(0.0-fx))

; reverse the function to obtain plot x1,x2,x3
 fz=0.0-alog((1.0-z)/z)
 x1_1=(fz-c-b2*x2-b3*x3)/b1
 x2_1=(fz-c-b1*x1-b3*x3)/b2
 x3_1=(fz-c-b1*x1-b2*x2)/b3

 p=plot3d(x1_1,x2_1,x3_1,'o',/sym_filled,$
	XRANGE=[-6, 6], YRANGE=[-6, 6], $

  ZRANGE=[-1.4, 1.4],$

  AXIS_STYLE=2, MARGIN=[0.2, 0.3, 0.1, 0], $

  XMINOR=0, YMINOR=0, ZMINOR=0, $

  DEPTH_CUE=[0, 2], /PERSPECTIVE, $

  RGB_TABLE=33,  $

  SHADOW_COLOR="deep sky blue", $

  XY_SHADOW=1, YZ_SHADOW=1, XZ_SHADOW=1, $

  XTITLE='x1', YTITLE='x2',ztitle='x3')
 stop

end
