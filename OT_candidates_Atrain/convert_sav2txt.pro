; to convert sav file to txt 

pro convert_sav2txt

    fname=file_search('/data/keeling/a/yulanh/c/BW_backup/mydata/OT_modis/CCM/post_process_OT','*parallax_colocated_information.sav')


    nf=n_elements(fname)

    for fi=0,nf-1 do begin
        restore,fname[fi]

        wfname=strcompress(strmid(fname[fi],0,128)+'txt',/rem)

        openw,u,wfname,/get_lun

        Ns=n_elements(colocate_lat_parallax)
        if (n_elements(tptopmost) ne Ns) then begin
            tpa=tptopmost
            tptopmost=fltarr(Ns)
            tptopmost[0:n_elements(tpa)-1]=tpa    
        endif
        printf,u,'lon, lat, bt11, bt11sm, bt67, bt67sm, topmost_dbz, tropmost, troppopause, tropopause_t'
        for si=0, Ns-1 do begin
            printf,u,colocate_lon_parallax[si],colocate_lat_parallax[si],colocate_bt11[si],colocate_bt11[si],$
                colocate_bt67[si],colocate_bt67[si],topmost_dbz[si],tptopmost[si],tptropopause[si],$
                tptropopause_t[si],format='(10(f10.5,x))'
        endfor
        free_lun,u
        print,fname[fi]

    endfor


end

