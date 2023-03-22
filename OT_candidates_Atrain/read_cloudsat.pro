pro read_cloudsat,fname,swname,fieldname,data
;+ to read dardar format data
;fname refers to the name of file; number is the number of field in the file

fid=eos_sw_open(fname,/read)
if fid le 0 then print,fid,fname

sid = eos_sw_attach(fid,swname)
;print,sid
nid=eos_sw_readfield(sid,fieldname,data)
;print,nid

nid=eos_sw_detach(sid)
nid=eos_sw_close(fid)

end
