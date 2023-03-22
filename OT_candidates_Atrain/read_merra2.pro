pro read_merra2,fname,lon,lat,surf_ps,surf_ts,time,troppb,troppt,troppv,tropt

fid=ncdf_open(fname)
sid=ncdf_varid(fid,'lon')
ncdf_varget,fid,sid,lon

sid=ncdf_varid(fid,'lat')
ncdf_varget,fid,sid,lat

sid=ncdf_varid(fid,'PS')
ncdf_varget,fid,sid,surf_ps

sid=ncdf_varid(fid,'TS')
ncdf_varget,fid,sid,surf_ts

sid=ncdf_varid(fid,'time')
ncdf_varget,fid,sid,time


sid=ncdf_varid(fid,'TROPPB')
ncdf_varget,fid,sid,troppb

sid=ncdf_varid(fid,'TROPPT')
ncdf_varget,fid,sid,troppt

sid=ncdf_varid(fid,'TROPPV')
ncdf_varget,fid,sid,troppv


sid=ncdf_varid(fid,'TROPT')
ncdf_varget,fid,sid,tropt

end
