pro read_modis_02,mod02fname,rad_062,rad_11,rad_67

  fid=hdf_sd_start(mod02fname,/read)

   sds_index=hdf_sd_nametoindex(fid,'EV_250_Aggr1km_RefSB')
   nid=hdf_sd_select(fid,sds_index)
   hdf_sd_getdata,nid,modis_250
   scale_ind=hdf_sd_attrfind(nid,'reflectance_scales')
   hdf_sd_attrinfo,nid,scale_ind,data=scale
   scale_ind=hdf_sd_attrfind(nid,'reflectance_offsets')
   hdf_sd_attrinfo,nid,scale_ind,data=offset
   rad_062=scale[0]*(modis_250[*,*,0]-offset[0])

   sds_index=hdf_sd_nametoindex(fid,'EV_500_Aggr1km_RefSB')
   nid=hdf_sd_select(fid,sds_index)
   hdf_sd_getdata,nid,modis_500
   scale_ind=hdf_sd_attrfind(nid,'radiance_scales')
   hdf_sd_attrinfo,nid,scale_ind,data=scale
   scale_ind=hdf_sd_attrfind(nid,'radiance_offsets')
   hdf_sd_attrinfo,nid,scale_ind,data=offset
   rad_21=scale[4]*(modis_500[*,*,4]-offset[4])

    sds_index=hdf_sd_nametoindex(fid,'EV_1KM_Emissive')
    nid=hdf_sd_select(fid,sds_index)
    hdf_sd_getdata,nid,modis_emis
    scale_ind=hdf_sd_attrfind(nid,'radiance_scales')
    hdf_sd_attrinfo,nid,scale_ind,data=scale
    scale_ind=hdf_sd_attrfind(nid,'radiance_offsets')
    hdf_sd_attrinfo,nid,scale_ind,data=offset
    rad_67=scale[6]*(modis_emis[*,*,6]-offset[6])
    rad_11=scale[10]*(modis_emis[*,*,10]-offset[10])
  	hdf_sd_end,fid

end
