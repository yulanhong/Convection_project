pro read_dardar,fname,fieldname,data
;+ to read dardar format data
;fname refers to the name of file; number is the number of field in the file

fid=hdf_sd_start(fname,/read)
;print,fid
sds_index = HDF_SD_NAMETOINDEX(fid,fieldname)
;print,sds_index
nid=hdf_sd_select(fid,sds_index)
;print,nid
hdf_sd_getdata,nid,data

hdf_sd_endaccess,nid

hdf_sd_end,fid

end
