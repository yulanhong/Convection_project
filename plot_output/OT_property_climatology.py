# this script is to extract the OT information, recording lat, min BT11, avg. BT11, \
#avg. BT6.7, avg. CiBT, tropopause BT, H_sigma for OT-Ci area

def read_text(fname):
    fo=open(fname,'r') #create file object
    #headstr=fo.readline()
    data=fo.readlines()
    #print (data)
    fo.close()  # close object
    return data

import os 
import matplotlib as mpl
import matplotlib.pyplot as plt
from datetime import datetime, date, timedelta
import numpy as np
import pandas as pd
import glob
from pyhdf.SD import SD, SDC
import h5py
import statsmodels.api as sm
from scipy import stats
from math import floor

print('start',datetime.now())
planck_c1=1.191042e8
planck_c2=1.4387752e4

years=['2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013',\
       '2014','2015','2016','2017','2018','2019','2020','2021']
dir02='/data/keeling/a/yulanh/satellite/TerraDataArchive/MODIS/MOD021KM/'
dir03='/data/keeling/a/yulanh/satellite/TerraDataArchive/MODIS/MOD03/'
years=['2015']#,'2021']
#       '2014','2015','2016','2017','2018','2019','2020','2021']
dir02='/data/keeling/a/yulanh/f/MODIS/MYD021KM/'
dir03='/data/keeling/a/yulanh/f/MODIS/MYD03/'
#dir02='/data/gdi/e/MODIS/MYD021KM/'
#dir03='/data/gdi/e/MODIS/MYD03/'

Pthreshold=0.9
#years=['2000']
for year in years: 
    print(year)
    #fnames=glob.glob('/data/keeling/a/yulanh/c/OT_output/Terra/'+year+'/*.txt')
    fnames=glob.glob('/data/keeling/a/yulanh/c/OT_output/Aqua/'+year+'/*.txt')
    wfname=open('Aqua_OT_property_record_'+year+'.txt','w')
    wfname.write('lat, lon, ota, min_BT11, ave_BT11, ave_BT67, ave_ciBT11, tropoauseT, heterogeneity, dnflag, ct_lat, ct_lon, ct_min_BT11, time\n')
    #fnames=fnames[0:1]
    for fname in fnames:     
        #print(fname)
        data=read_text(fname)
        splitcol=data[0].split(' ')
        Ncol=len(splitcol)-splitcol.count('')
        Nrow=len(data)
        dataT=np.zeros((Nrow,Ncol),'f')
        for i in range(Nrow):
            splitcol=data[i].split(' ')
            k=0
            for j in range(len(splitcol)):
                if splitcol[j] != '' and splitcol[j] != '\n' :
                    dataT[i,k]=float(splitcol[j])
                    k=k+1
        proba = dataT[:,4]
        ind=np.where(proba >= Pthreshold)[0]
        Nnum=len(ind)
        if (Nnum > 0 ):
            #to correct lon and lat and bt
            yyyymmdd=fname[-16:-4]
            Julday=fname[-12:-9]
            mod02fname=glob.glob(dir02+year+'/'+Julday+'/MYD021KM.A'+yyyymmdd+'*')[0]
            mod03fname=glob.glob(dir03+year+'/'+Julday+'/MYD03.A'+yyyymmdd+'*')[0]
            
            hdf=SD(mod02fname,SDC.READ)
            emis_obj=hdf.select('EV_1KM_Emissive')
            for key, value in emis_obj.attributes().items():
                if key == 'radiance_offsets':
                    add_offset = value  
                if key == 'radiance_scales':
                    scale_factor = value
            emis11=scale_factor[10]*(emis_obj[10,:,:]-add_offset[10])
            bt11_2d=planck_c2/(11.03*np.log(planck_c1/(emis11*11.03**5.0)+1))
            
            hdf=SD(mod03fname)
            modlat=hdf.select('Latitude')
            modlat=modlat[:,:]
            modlon=hdf.select('Longitude')
            modlon=modlon[:,:]
            #print(modlon.shape)

            for ni in range(Nnum):
                i=ind[ni]
                yscp= int(dataT[i,0])-1
                xscp= int(dataT[i,1])-1
                #print(xscp,yscp,bt11_2d.shape,modlat[xscp,yscp],modlon[xscp,yscp],bt11_2d[xscp,yscp])
                lat = dataT[i,3]
                lon = dataT[i,2]
                ota = dataT[i,5]
                BT11_minOT=dataT[i,6]
                BT11_aveOT=dataT[i,7]
                BT67_minOT=dataT[i,8]
                BT67_aveOT=dataT[i,9]
                trop_T = dataT[i,10]
                BT11_aveci=dataT[i,11]
                hetero= dataT[i,30]/((BT11_aveOT+BT11_aveci)/2.0)
                
                SLT=float(fname[-8:-6])+dataT[i,2]/15.0 #solar local time
                dnflag=0
                if (SLT < 0) :
                    SLT=SLT+24

                if (SLT > 24) :
                    SLT=SLT-24

                if ((SLT >= 6) & (SLT < 18)):
                    dnflag=0
                else:
                    dnflag=1

                #to get correct tb and location
                lowscp=xscp-2
                upscp=xscp +3
               
                if lowscp < 0:
                    lowscp=0
                if upscp > bt11_2d.shape[0]:
                    upscp = bt11_2d.shape[0]
                    
                lowscp1=yscp-2
                upscp1=yscp +3
                if lowscp1 < 0:
                    lowscp1=0
                if upscp1 > bt11_2d.shape[1]:
                    upscp1 = bt11_2d.shape[1]
                    
                
                tpbt11_box=bt11_2d[lowscp:upscp,lowscp1:upscp1]
                tplat_box =modlat[lowscp:upscp,lowscp1:upscp1]
                tplon_box =modlon[lowscp:upscp,lowscp1:upscp1]
              
                if (np.min(tpbt11_box) == BT11_minOT):
                    ct_lon=lon
                    ct_lat=lat
                    ct_min_BT11=BT11_minOT
                if (np.min(tpbt11_box) != BT11_minOT):
                    minind=np.where(np.min(tpbt11_box) == tpbt11_box)
                    ct_lon=tplon_box[minind[0],minind[1]][0]
                    ct_lat=tplat_box[minind[0],minind[1]][0]
                    ct_min_BT11=np.min(tpbt11_box)
                
                    
                
                wfname.write(str(lat)+', '+str(lon)+', '+str(ota)+', '+str(BT11_minOT)+ ', '+str(BT11_aveOT)+', '+str(BT67_aveOT)+', '+\
                     str(BT11_aveci)+', '+str(trop_T)+', '+ str(hetero)+', '+str(dnflag)+', '+str(ct_lat)+', '+str(ct_lon)+', '+\
                             str(ct_min_BT11)+', '+yyyymmdd+'\n')
        
        
    wfname.close()
    
print('finish',datetime.now())

