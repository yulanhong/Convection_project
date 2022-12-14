#!/usr/bin/env python
# coding: utf-8

# In[ ]:


def radian_to_latlon(g16_data_file):
    from netCDF4 import Dataset
    import numpy as np
    
    # designate dataset
    g16nc = Dataset(g16_data_file, 'r')

    # GOES-R projection info and retrieving relevant constants
    proj_info = g16nc.variables['goes_imager_projection']
    lon_origin = proj_info.longitude_of_projection_origin
    H = proj_info.perspective_point_height+proj_info.semi_major_axis
    r_eq = proj_info.semi_major_axis
    r_pol = proj_info.semi_minor_axis

    # Data info
    lat_rad_1d = g16nc.variables['x'][:]
    lon_rad_1d = g16nc.variables['y'][:]
   
    # close file when finished
    g16nc.close()
    g16nc = None

    # create meshgrid filled with radian angles
    lat_rad,lon_rad = np.meshgrid(lat_rad_1d,lon_rad_1d)

    # lat/lon calc routine from satellite radian angle vectors

    lambda_0 = (lon_origin*np.pi)/180.0

    a_var = np.power(np.sin(lat_rad),2.0) + (np.power(np.cos(lat_rad),2.0)*(np.power(np.cos(lon_rad),2.0)+(((r_eq*r_eq)/(r_pol*r_pol))*np.power(np.sin(lon_rad),2.0))))
    b_var = -2.0*H*np.cos(lat_rad)*np.cos(lon_rad)
    c_var = (H**2.0)-(r_eq**2.0)

    r_s = (-1.0*b_var - np.sqrt((b_var**2)-(4.0*a_var*c_var)))/(2.0*a_var)

    s_x = r_s*np.cos(lat_rad)*np.cos(lon_rad)
    s_y = - r_s*np.sin(lat_rad)
    s_z = r_s*np.cos(lat_rad)*np.sin(lon_rad)

    lat = (180.0/np.pi)*(np.arctan(((r_eq*r_eq)/(r_pol*r_pol))*((s_z/np.sqrt(((H-s_x)*(H-s_x))+(s_y*s_y))))))
    lon = (lambda_0 - np.arctan(s_y/(H-s_x)))*(180.0/np.pi)

    # print test coordinates
    #print('{} N, {} W'.format(lat[250,250],abs(lon[250,250])))
    return lat,lon
#========== end of radian_to_latlon========================

def find_OT_edges(BT_wnd_radii,smBT_wnd_radii,BT_wvp_radii):
    from netCDF4 import Dataset
    import numpy as np
    import math

    OT_edge=np.zeros(Ndirection,dtype=np.uint8)
    OT_BTwnd_sum=np.zeros(Ndirection,'f')
    OT_BT2wnd_sum=np.zeros(Ndirection,'f')
    OT_BTwvp_sum=np.zeros(Ndirection,'f')
    ci_BTwnd_sum=np.zeros(Ndirection,'f')
    ci_BT2wnd_sum=np.zeros(Ndirection,'f')
    ci_BT_num=np.zeros(Ndirection,'i')
    
    for ri in range(Ndirection):
        first_deriv = np.gradient(smBT_wnd_radii[ri,:])
        second_deriv= np.gradient(first_deriv)
        ind=np.where(second_deriv < 0)[0]
       
        #plt.plot(smBT_wnd_radii[ri,:])
        #plt.show()
        #plt.plot(first_deriv)
        #plt.show()
        #plt.plot(second_deriv)
        #plt.show()
        
        if (len(ind) ==0): # if there is no negative second_derivative, using the minimum value 
            ind=np.where(min(second_deriv) == second_deriv)
            
        OT_edge[ri]=ind[0]+1
        OT_BTwnd_sum[ri]=sum(BT_wnd_radii[ri,0:OT_edge[ri]])
        OT_BT2wnd_sum[ri]=sum(BT_wnd_radii[ri,0:OT_edge[ri]]**2)
        OT_BTwvp_sum[ri]=sum(BT_wvp_radii[ri,0:OT_edge[ri]])
        
        tpBTci=BT_wnd_radii[ri,OT_edge[ri]:win_size+1]
        cind=np.where(tpBTci < cirrus_threshold)[0]
        ci_BTwnd_sum[ri] =sum(tpBTci[cind])
        ci_BT2wnd_sum[ri]=sum(tpBTci[cind]**2)
        ci_BT_num[ri]    = len(cind)
        
        
    OT_aveBTwnd = sum(OT_BTwnd_sum)/sum(OT_edge)
    OT_aveBTwvp = sum(OT_BTwvp_sum)/sum(OT_edge)
    ci_aveBTwnd = sum(ci_BTwnd_sum)/sum(ci_BT_num)
    OTci_aveBT2wnd = (sum(OT_BT2wnd_sum)+sum(ci_BT2wnd_sum))/(sum(OT_edge)+sum(ci_BT_num))
    OTci_aveBTwnd  = (sum(OT_BTwnd_sum)+sum(ci_BTwnd_sum))/(sum(OT_edge)+sum(ci_BT_num))
    OTci_stdBTwnd  = math.sqrt(OTci_aveBT2wnd-OTci_aveBTwnd**2)
  
    return OT_aveBTwnd,OT_aveBTwvp,ci_aveBTwnd,OTci_stdBTwnd,OT_edge
#=============== end of find_OT_edges====================
from csv import writer

def append_list_as_row(file_name, list_of_elem):
        # Open file in append mode
        with open(file_name, 'a+', newline='') as write_obj:
            # Create a writer object from csv module
            csv_writer = writer(write_obj)
            # Add contents of list as last row in the csv file
            csv_writer.writerow(list_of_elem)
    
#================= main ===========================================
def OT_main(G16wvp_file):
    
    from netCDF4 import Dataset
    from pyhdf.SD import SD, SDC
    import h5py
    import numpy as np
    import pandas as pd
    import math
    import csv
    from csv import writer
    import os, os.path
    import cartopy.crs as ccrs
    import cartopy.mpl.ticker as cticker
    import matplotlib.pyplot as plt
    from datetime import datetime, date, timedelta
    from scipy.ndimage import gaussian_filter
    import glob
    global win_size,Ndirection,cirrus_threshold

    #=========== these parameters are fixed ==========
    win_size=10
    Ndirection=8
    cirrus_threshold=260
    const =-3.2397
    coef_1=0.2075
    coef_2=0.3516
    coef_3=0.4996

    #============ change the directory for data acesses========================
    data_dir  ='/data/accp/a/snesbitt/goes-mds-tornado/'
    merra2_dir='/data/accp/a/snesbitt/geos_tt/' # change the the
    #merra2_dir='' # change the the
    write_dir ='/data/keeling/a/ecwolff3/GOES_OT/GOES_Test/Test_Output/New_Test1/'
    #=========================================
    
    scan_mode_flag='M6C14_G16_' # change the scan mode for window channel,which should be same as water vapor channel

    #11.2 um
    #G16wnd_file = 'OR_ABI-L1b-RadM2-M3C14_G16_s20183160350548_e201831603506_c201831603553.nc'
    G16wnd_file = glob.glob(data_dir + G16wvp_file[-77:-60] + scan_mode_flag + G16wvp_file[-50:-35]+'*')
    G16wnd_file = G16wnd_file[0]
    G16wnd_exist= os.path.exists(G16wnd_file)
    if G16wnd_exist == 'Fause':
        print('No window channel file for:'+G16wvp_file)
        exist()

    print(G16wvp_file,G16wnd_file)
    # for record result
    wfname =write_dir+G16wvp_file[-77:-60]+'OT_C0814'+G16wnd_file[-55:-2]+'h5'

    #=== process MERRA2 data file==
    year=G16wnd_file[-49:-45]
    day =G16wnd_file[-45:-42]
    time_utchh=G16wnd_file[-42:-40]
    time_utcmm=G16wnd_file[-40:-38]
    time_utc=int(time_utchh)+int(time_utcmm)/60.0
    #print(year,day,time_utchh,time_utcmm,time_utc)

    #==== get year and date=====
    start_date=date(int(year),1,1)
    convt_date=start_date+timedelta(days=int(day)-1)
    date_res  = convt_date.strftime("%m%d%y")
    mmdd=date_res[0:4]
    #merra2_file=merra2_dir+year+'/MERRA2_400.inst1_2d_asm_Nx.'+year+mmdd+'.nc4' # modify file name accordingly
    merra2_file=merra2_dir+year+mmdd+'_'+date_res[0:2]+'_'+'f'+time_utchh+'.nc'
    print(merra2_file)
    merra2_exist=os.path.exists(merra2_file)
    if merra2_exist == 'Fause':
        print('No MERRA2 file '+merra2_file)
        exist()
    #==== read MERRA-2 data ===
    merra2_id = Dataset(merra2_file,'r')
    merra2_tropopause_t=merra2_id.variables['__xarray_dataarray_variable__'][:]
    merra2_lat=merra2_id.variables['lat'][:]
    merra2_lon=merra2_id.variables['lon'][:]
    merra2_id.close()
    merra2_id = None

    #===== GOES-R radiance and BT
    g16nc = Dataset(G16wvp_file,'r')
    fk1_wvp=g16nc.variables['planck_fk1'][:]
    fk2_wvp=g16nc.variables['planck_fk2'][:]
    bc1_wvp=g16nc.variables['planck_bc1'][:]
    bc2_wvp=g16nc.variables['planck_bc2'][:]
    Rad_wvp=g16nc.variables['Rad'][:]
    g16nc.close()
    g16nc = None
    lat_wvp,lon_wvp=radian_to_latlon(G16wvp_file)

    g16nc = Dataset(G16wnd_file,'r')
    fk1_wnd=g16nc.variables['planck_fk1'][:]
    fk2_wnd=g16nc.variables['planck_fk2'][:]
    bc1_wnd=g16nc.variables['planck_bc1'][:]
    bc2_wnd=g16nc.variables['planck_bc2'][:]
    Rad_wnd=g16nc.variables['Rad'][:]
    g16nc.close()
    g16nc = None
    lat_wnd,lon_wnd=radian_to_latlon(G16wnd_file)

    #initialize the data array for recording results
    xdim=Rad_wnd.shape[0]
    ydim=Rad_wnd.shape[1]
    #OT_Area_circ  = np.zeros((xdim,ydim),'f')
    #OT_probability= np.zeros((xdim,ydim),'f')
    #OT_meanBTwnd  = np.zeros((xdim,ydim),'f')
    #OT_meanBTwvp  = np.zeros((xdim,ydim),'f')
    #ci_meanBTwnd  = np.zeros((xdim,ydim),'f')
    #OTci_sigmaBTwnd = np.zeros((xdim,ydim),'f')
    #BTD = np.zeros((xdim,ydim),'f') #new parameter
    #colocate_tropopause_t=np.zeros((xdim,ydim),'f')
    #OT_indices    = np.zeros((xdim,ydim),dtype=np.uint8)
    OT_edges_write= np.empty((0,8),dtype=np.uint8) 
    OT_index=1
    # CSV Update #
    OT_Area_circ  = np.empty((0),'f') 
    OT_probability= np.empty((0,1),dtype=np.uint8) 
    OT_meanBTwnd  = np.empty((0,1),dtype=np.uint8) 
    OT_meanBTwvp  = np.empty((0,1),dtype=np.uint8) 
    ci_meanBTwnd  = np.empty((0,1),dtype=np.uint8) 
    OTci_sigmaBTwnd = np.empty((0,1),dtype=np.uint8) 
    BTD = np.empty((0,1),dtype=np.uint8) #new parameter
    #colocate_tropopause_t=np.empty((0,1),dtype=np.uint8) 
    OT_indices    = np.empty((0,1),dtype=np.uint8)

    #==== convert to BT and smooth BT====
    BT_wvp=[fk2_wvp/(np.log((fk1_wvp/Rad_wvp)+1))-bc1_wvp]/bc2_wvp
    BT_wnd=[fk2_wnd/(np.log((fk1_wnd/Rad_wnd)+1))-bc1_wnd]/bc2_wnd
    BT_wvp=BT_wvp.reshape(xdim,ydim)
    BT_wnd=BT_wnd.reshape(xdim,ydim)
    #==== Gaussian filter to smooth BT field ===
    smooth_BT_wvp = gaussian_filter(BT_wvp,sigma=1)
    smooth_BT_wnd = gaussian_filter(BT_wnd,sigma=1)
    smooth_BT_wvp=smooth_BT_wvp.reshape(xdim,ydim)
    smooth_BT_wnd=smooth_BT_wnd.reshape(xdim,ydim)


    #print(merra2_tropopause_t.shape)
    #xdim=335
    #ydim=236

    #====== loop for OT search ========
    for xi in range(win_size,xdim-win_size):
        for yi in range(win_size,ydim-win_size):
            #print(xi,yi)
            tp_goeslon  = lon_wnd[xi,yi]
            tp_goeslat  = lat_wnd[xi,yi]
            # obtain MERRA tropopause temperature 
            merralon_scp= round((tp_goeslon+180)/0.625) 
            merralat_scp= round((tp_goeslat+90)/0.5)
            hour_scp = round(time_utc)
            if (merralon_scp > len(merra2_lon)):
                merralon_scp = len(merra2_lon)-1
            if (merralat_scp > len(merra2_lat)):
                merralat_scp = len(merra2_lat)-1
            if (hour_scp > 23):
                hour_scp=23
            tp_tropopause_t=merra2_tropopause_t[merralat_scp,merralon_scp]
            #colocate_tropopause_t[xi,yi]=tp_tropopause_t

            #===== start OT search ===
            if (abs(tp_goeslat) <= 25):
                BT_threshold = 200
            if (abs(tp_goeslat) > 25):
                BT_threshold = 230

            if ((BT_wnd[xi,yi] <= BT_threshold) & (BT_wnd[xi,yi] < tp_tropopause_t)):
                colupres = yi + win_size
                collowres= yi - win_size
                rowupres = xi + win_size
                rowlowres= xi - win_size
                tpbt_wnd_box=BT_wnd[rowlowres:rowupres,collowres:colupres]
                tpsmbt_wnd_box=smooth_BT_wnd[rowlowres:rowupres,collowres:colupres]


        #=== obtain BT in wv channle=====
                goesdiff= (abs(lat_wvp-tp_goeslat)+abs(lon_wvp-tp_goeslon))
                goesind = np.where(goesdiff == goesdiff.min())
                xi_wvp  = goesind[0][0]
                yi_wvp  = goesind[1][0]
                tpbt_wvp_box=BT_wvp[xi_wvp-win_size:xi_wvp+win_size,                                    yi_wvp-win_size:yi_wvp+win_size]

                #print(xi,yi,BT_wnd[xi,yi],np.min(tpbt_wnd_box))
                if (BT_wnd[xi,yi] == np.min(tpbt_wnd_box)): #only if the pixel is colder in the window, go to next step
                    # obtain 8 vectors in 8 directions
                    BT_wnd_radii=np.zeros((8,win_size),'f')
                    smBT_wnd_radii=np.zeros((8,win_size),'f')
                    BT_wvp_radii=np.zeros((8,win_size),'f')

                    BT_wnd_radii[0,:]=BT_wnd[xi,yi:yi+win_size]
                    BT_wnd_radii[1,:]=np.diagonal(BT_wnd[xi:xi+win_size,yi:yi+win_size])
                    BT_wnd_radii[2,:]=BT_wnd[xi:xi+win_size,yi]
                    BT_wnd_radii[3,:]=np.diagonal(np.fliplr(BT_wnd[xi:xi+win_size,yi-win_size+1:yi+1]).transpose())
                    BT_wnd_radii[4,:]=BT_wnd[xi,yi-win_size+1:yi+1][::-1]
                    BT_wnd_radii[5,:]=np.diagonal(BT_wnd[xi-win_size+1:xi+1,yi-win_size+1:yi+1])[::-1]
                    BT_wnd_radii[6,:]=BT_wnd[xi-win_size+1:xi+1,yi][::-1]
                    BT_wnd_radii[7,:]=np.diagonal(np.fliplr(BT_wnd[xi-win_size+1:xi+1,yi:yi+win_size]))[::-1]

                    smBT_wnd_radii[0,:]=smooth_BT_wnd[xi,yi:yi+win_size]
                    smBT_wnd_radii[1,:]=np.diagonal(smooth_BT_wnd[xi:xi+win_size,yi:yi+win_size])
                    smBT_wnd_radii[2,:]=smooth_BT_wnd[xi:xi+win_size,yi]
                    smBT_wnd_radii[3,:]=np.diagonal(np.fliplr(smooth_BT_wnd[xi:xi+win_size,yi-win_size+1:yi+1]).transpose())
                    smBT_wnd_radii[4,:]=smooth_BT_wnd[xi,yi-win_size+1:yi+1][::-1]
                    smBT_wnd_radii[5,:]=np.diagonal(smooth_BT_wnd[xi-win_size+1:xi+1,yi-win_size+1:yi+1])[::-1]
                    smBT_wnd_radii[6,:]=smooth_BT_wnd[xi-win_size+1:xi+1,yi][::-1]
                    smBT_wnd_radii[7,:]=np.diagonal(np.fliplr(smooth_BT_wnd[xi-win_size+1:xi+1,yi:yi+win_size]))[::-1]

                    BT_wvp_radii[0,:]=BT_wvp[xi_wvp,yi_wvp:yi_wvp+win_size]
                    BT_wvp_radii[1,:]=np.diagonal(BT_wvp[xi_wvp:xi_wvp+win_size,yi_wvp:yi_wvp+win_size])
                    BT_wvp_radii[2,:]=BT_wvp[xi_wvp:xi_wvp+win_size,yi_wvp]
                    BT_wvp_radii[3,:]=np.diagonal(np.fliplr(BT_wvp[xi_wvp:xi_wvp+win_size,yi_wvp-win_size+1:yi_wvp+1]).transpose())
                    BT_wvp_radii[4,:]=BT_wvp[xi_wvp,yi_wvp-win_size+1:yi_wvp+1][::-1]
                    BT_wvp_radii[5,:]=np.diagonal(BT_wvp[xi_wvp-win_size+1:xi_wvp+1,yi_wvp-win_size+1:yi_wvp+1])[::-1]
                    BT_wvp_radii[6,:]=BT_wvp[xi_wvp-win_size+1:xi_wvp+1,yi_wvp][::-1]
                    BT_wvp_radii[7,:]=np.diagonal(np.fliplr(BT_wvp[xi_wvp-win_size+1:xi_wvp+1,yi_wvp:yi_wvp+win_size]))[::-1]

                    #calculate the gradient and obtain OT edges, mean BTs 
                    OT_aveBTwnd,OT_aveBTwvp,ci_aveBTwnd,OTci_stdBTwnd,OT_edge =                     find_OT_edges(BT_wnd_radii,smBT_wnd_radii,BT_wvp_radii)

                    #==== to calculate probability and OT area ====
                    if ((OT_aveBTwvp > OT_aveBTwnd) & (ci_aveBTwnd > OT_aveBTwnd)):
                        radii_mean=2*np.mean(OT_edge)
                        tpot_area=math.pi*radii_mean**2

                        tpfx=const+coef_1*(ci_aveBTwnd-BT_wnd[xi,yi])+coef_2*(tp_tropopause_t-BT_wnd[xi,yi])+                        coef_3*(OT_aveBTwvp-OT_aveBTwnd)
                        tpot_prob=1.0/(1+math.exp(0.0-tpfx))

                        #= tpot_area
                        #OT_probability= tpot_prob
                        #OT_meanBTwnd  = OT_aveBTwnd
                        #OT_meanBTwvp  = OT_aveBTwvp
                        #ci_meanBTwnd  = ci_aveBTwnd
                        #OTci_sigmaBTwnd= OTci_stdBTwnd
                        #OT_indices = OT_index
                        #OT_index=OT_index +1
                        OT_edges_write=np.append(OT_edges_write,[OT_edge],axis=0)
                        #BTD = (ci_aveBTwnd-OT_aveBTwnd) #new parameter
                        print(tpot_area.shape,OT_Area_circ.shape)
                        OT_Area_circ  = np.append(OT_Area_circ,tpot_area,axis=0)
            
                        OT_probability= np.append(OT_probability,[tpot_prob],axis=0)
                        OT_meanBTwnd  = np.append(OT_meanBTwnd,[OT_aveBTwnd],axis=0)
                        OT_meanBTwvp  = np.append(OT_meanBTwvp,[OT_aveBTwvp],axis=0)
                        ci_meanBTwnd  = np.append(ci_meanBTwnd,[ci_aveBTwnd],axis=0)
                        OTci_sigmaBTwnd= np.append(OTci_sigmaBTwnd,[OTci_stdBTwnd],axis=0)
                        #OT_indices = np.append(OT_indices,[OT_index],axis=0)
                        BTD = np.append(BTD,[ci_aveBTwnd-OT_aveBTwnd],axis=0) #new parameter
                        OT_lat = np.append(OT_lat,[lat_wnd],axis=0)
                        OT_lon = np.append(OT_lon,[lon_wnd],axis=0)
                        tropo_temp = np.append(tropo_temp,[tp_tropopause_t],axis=0)
                        filename = np.append(filename,[G16wnd_file],axis=0)
                        

    #==== to record the results ======
    row_contents = [filename, OT_Area_circ, OT_probability, OT_lat, OT_lon, OT_meanBTwnd, OT_meanBTwvp, ci_meanBTwnd, OTci_sigmaBTwnd, OT_edges_write, tropo_temp, BTD]
    append_list_as_row('OT_output_test5.csv', row_contents)

