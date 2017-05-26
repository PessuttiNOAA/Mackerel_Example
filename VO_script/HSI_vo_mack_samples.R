
# I have made Some Changes !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# This is an example Code to show the use of Github and SourceTree
# Code Version 2.0
# Code to Extract estimated habitat suitability from BTS locations 
require(raster)
require(ncdf4)
library(geosphere)
library(plyr)
#library(MASS)
require(lubridate)
library(maptools)

yrs<-1981:2015
#yrs<-2015
## some initial definitions
#includedstrata<-as.list(list(c(seq(1010,1120,by=10),1230,1250,seq(1610,1760,by=10),seq(3010,3610,by=10)),c(seq(1010,1120,by=10))))
#includedstrata<-as.list(list(c(seq(1010,1120,by=10),1230,1250,seq(1610,1760,by=10),seq(3010,3610,by=10))))

# ###########################
# setwd("C:/Users/jpessutt/Documents/Projects/CoopResearch/Manderson/Mackerel/Mackerel/") # at work
# mackStrat<-as.list(read.delim('Mack_present_strata.csv',sep=",",header=TRUE)) #pulls list from csv mack present strata
# includedstrata<-mackStrat
# ###############################

# From Keirsten-Most likely set of strata
includedstrata<-as.numeric(c('01010','01020','01030','01040','01050','01060','01070','01080','01090','01100','01110','01120','01130','01140','01150','01160',
  '01170','01180','01190','01200','01210','01220','01230','01240','01250','01260','01270','01280','01290','01300','01340','01351',
  '01360','01370','01380','01390','01400','01610','01620','01630','01640','01650','01660','01670','01680','01690','01700','01710',
  '01720','01730','01740','01750','01760','03020','03050','03080','03110','03140','03170','03200','03230','03260','03290','03320',
  '03350','03380','03410','03440','03450','03460','03560','03590','03600','03610','03640','03650','03660'))

btsStrata<-readShapePoly("C:/Users/jpessutt/Documents/Projects/CoopResearch/Manderson/Mackerel/Mackerel/GIS/BTS_STRATA_NAD83.shp") 
projlcc <-CRS("+proj=lcc +lon 0=90w +lat 1=15n +lat 2=65n")
proj4string(btsStrata) <- projlcc

### Temporary strata designation
#CoreOffshoreStrata<-c(seq(1010,1300,10),1340, seq(1360,1400,10),seq(1610,1760,10))
#CoreInshore73to12=c(3020, 3050, 3080 ,3110 ,3140 ,3170, 3200, 3230, 3260, 3290, 3320, 3350 ,3380, 3410 ,3440)
#includedstrata=c(CoreOffshoreStrata,CoreInshore73to12)

coast<-read.table(file="C:/Users/jpessutt/Documents/Projects/CoopResearch/Manderson/Mackerel/Mackerel/GIS/bigcoast.dat",header=FALSE, sep="") # at work  

#### bring in masking grid for depth
setwd("C://Users/jpessutt/Documents/Projects/CoopResearch/Manderson/Mackerel/Mackerel/Ralph/")
zmask<-raster('z_mask_1000.tif')


setwd("C:/Users/jpessutt/Documents/Projects/CoopResearch/Manderson/Mackerel/Mackerel/Old_Code/habitatcode") # at work
#setwd('/Users/JPessutti/Documents/NOAA/Projects/CoopResearch/Mackerel/habitatcode/') # at mac
#areafile<-'/Users/JPessutti/Documents/NOAA/Projects/CoopResearch/Mackerel/habitatcode/nefsc_strata_table.csv' #at mac
areafile<-"C:/Users/jpessutt/Documents/Projects/CoopResearch/Manderson/Mackerel/Mackerel/Old_Code/habitatcode/nefsc_strata_table.csv" # xls file with area for each strata - I only have NMFS-calculated areas for strata used in the butterfish assessment
rmsesheet<-'' # sheet in workbook that has RMSE for the season you need
debiasedname<-'http://aqua.smast.umassd.edu:8080/thredds/dodsC/bt_fall_db_monthly_oa/nwa_bt_fall_'; # name of debiased output up until year #
#debiasedname='/Users/palamara/Documents/squid_butterfish/bottomtemp/ROMSdebiased/roms_debiased_runs/nwa_bt_';
#parameterfile<-'/Users/palamara/Desktop/ScupFiles/bluefish/Bluefish_BA_Coeffs.xlsx';
#parameterfile<-'/Users/JPessutti/Documents/NOAA/Projects/CoopResearch/Mackerel/habitatcode/scup_BA_coeffs.xlsx';
parameterfile<-'/Users/JPessutti/Documents/NOAA/Projects/CoopResearch/Mackerel/habitatcode/scup_BA_coeffs.csv' # at mac
#parameterfile<-'C:/Users/jpessutt/Documents/Projects/CoopResearch/Manderson/Mackerel/Mackerel/Old_Code/habitatcode/scup_BA_coeffs.csv' # at work
# shallowbreak<-NULL
# if(!rmsesheet==''){
#     shallowbreak<-30; # how we define "shallow" for RMSE purposes (for this we used 30 m)
# }

season='spring'

trawlfiles<-as.list(c("MackerelNEFSCbt_01112017_w_maturity.csv"))
trawltypes<-as.list(c('nmfs'))

#typelabels<-as.list(c('nefsc_inshoretotal','fakes')) #_inner';'nefsc_inshore_outer';'neamap'};
typelabels<-as.list(c('nefsc_inshoretotal')) #_inner';'nefsc_inshore_outer';'neamap'};

# lon & lat bounds - pixels outside these bounds not considered in analysis
nLat=45
sLat=35
wLon=-78
eLon=-65



  #### bring in BTS dataset 
    trawldata<-read.delim(trawlfiles[[1]],sep=",",header=TRUE,row.names=NULL) #pulls in trawldata for each file, may only be one
    trawldata$ID<-paste(trawldata$CRUISE6,trawldata$STATION,sep="-")
    trawldata$date<-as.POSIXct(trawldata$EST_TOWDATE, format="%Y-%m-%d %H:%M:%S",tz="GMT") #formats date
    trawldata$date<-trawldata$date+5*60*60 #corrects from EST to GMT
    trawldata$date<-date(trawldata$date)
    towyear<-trawldata$YEAR
    towstrata<-trawldata$STRATUM
    towlon<-trawldata$LON
    towlat<-trawldata$LAT
    towseason<-trawldata$SEASON
    towid<-trawldata$ID
 

    ind<-which(towyear %in% yrs)# checks if towyear is in yrs
    indseason<-which(towseason==(toupper(season)))# same for season
    ind<-intersect(ind,indseason)#finding overlap between ind and indseason-overwrites first ind
    studytrawl<-trawldata[ind,] # subset by year and season
   
    

    towstrata<-studytrawl$STRATUM # identify stratum in dataset (used below)
    

#######################################   IF  limiting to only the strata we're including
#     if(!is.null(includedstrata)){      #may need to return statement to includedstrata[[i]] if additional lists are used.            
#         ind2<-which(towstrata %in% includedstrata)
#         yrtrawl<-yrtrawl[ind2,] # subset by year and season
#     }
   

###### Get model dates  
ralph<-"C:/Users/jpessutt/Documents/Projects/CoopResearch/Manderson/Mackerel/Mackerel/NWA-RD.HCSODA3R_bottomT_1980-2015.nc"
#you can add .html after netcdf connection to get data access page.

nc<-nc_open(ralph)

time_sec<-ncvar_get(nc,varid="ocean_time") #Updated for ncdf4 seconds since 1900-01-01 00:00:00
long_times<-as.POSIXct(time_sec, origin="1900-01-01 00:00:00", tz="GMT") #model times
long_dates<-date(long_times) # Date portion of model times
long_times_c<-as.character(long_times)


for (a in 1:length(yrs)){
    #a<-1
    # get year
    yr<-yrs[a]
    print(yr)
    
    setwd("C://Users/jpessutt/Documents/Projects/CoopResearch/Manderson/Mackerel/Mackerel/Ralph/")
    setwd(paste(as.character(yr),"/result",sep=""))
##############################################


    ###############################################################################################
    # identify which trawls fall in that year
    indtows<-which(studytrawl$YEAR==yr)
    yrtrawl<-studytrawl[indtows,] # make subset of studytrawls for just that year
    
    #Create placeholders for new calculations
    yrtrawl$kHSIv<-NA ##HSI value at sample point from valid, movement constrained grid
    yrtrawl$vCell<-NA #placeholder
    yrtrawl$off<-NA #placeholder
    yrtrawl$kHSIo<-NA #HSI value at sample point from original, non-movement constrained grid
    yrtrawl$oCell<-NA #placeholder
    yrtrawl$model<-NA #placeholder

    trawlDates<-unique(yrtrawl$date)# unique trawl dates within year


    for (i in trawlDates){  #steps through unique trawl dates within year
    #d<-trawlDates[1]
     modelDates<-which(long_dates==i) # finds index of model grid that matches focus trawl date
     print(modelDates)
     
     #movement Constrained
     vgrid<-raster(paste('valid_',yr,'_HSI_',modelDates,'.tif',sep="")) # brings in daily HSI-valid model
     #pgrid<-raster(paste('Patchmask_',yr,'_HSI_',modelDates,'.tif',sep="")) # brings in daily HSI-valid model
     # need to find value for non-movement constrained too
     setwd("C://Users/jpessutt/Documents/Projects/CoopResearch/Manderson/Mackerel/Mackerel/Ralph/")
     setwd(as.character(yr))
     ogrid<-raster(paste('HSI_',modelDates,'.tif',sep="")) # brings in daily HSI-temp model
     ogrid<-ogrid*zmask # applies depth mask
     
     #vgrid<-ogrid*pgrid # multiplies original by patch to get valid 
     
     setwd('result')
     #identifies tow samples on date d
     ind<-which(yrtrawl$date==i)
      
      for (s in ind){ # each sample on date d 
               
        slon<-yrtrawl$LON[s] # lon of sample k
        slat<-yrtrawl$LAT[s] #lat of sample k
        #strat<-yrtrawl$strata[s] #strata of sample k
        #skdens<-yrtrawl$kdens[s]  #strat area/count of samples for k sample strata
        
        spt<-cbind(slon,slat) ####creates tow point DataFrame
        #sHSI<-extract(vgrid,spt) # Extracts value
        
        ####### Movement constrained
        sCell<-extract(vgrid,spt,cellnumbers=TRUE) # Extracts value
        sHSI<-sCell[2]
        sCell<-sCell[1]
        print(sHSI)
        print(sCell)
        
        # what if point falls off model, find closest
        if (is.na(sHSI)){
          yrtrawl[s,30]<-1 # mark it as off
          vpoints<-rasterToPoints(vgrid) # converts grid to points
          pixcoor<-as.data.frame(vpoints[,c(1:2)])# gets lat lons of all cell values
          dist<-distm(spt, pixcoor, fun = distHaversine)  #asigns distance from point to all grid cells
          dmin<-which((dist==min(dist))) #finds index of point with minimum distance
          clpoint<-vpoints[dmin,]   #uses index to identify closest point
          cCell<-extract(vgrid,cbind(clpoint[1],clpoint[2]),cellnumbers=TRUE) # Extracts the Cell and the value
          
#           ##### Tests distance algo
#           test<-vgrid
#           test[sCell[1]]<-2 # make sCell a 2]
#           test[cCell[1]]<-2 # make sCell a 2
         
          sHSI<-cCell[2]
          sCell<-cCell[1]
        }
        
        yrtrawl[s,28]<-sHSI
        yrtrawl[s,29]<-sCell
        #numerator<-sHSI*skdens # numerator of equation for sample Kij
          
          ####### Non-movement constrained
          oHSI<-ogrid[sCell] # cell position will be same as above 
          oCell<-sCell
          print(oHSI)
          print(oCell)

# #Not needed-long way to do it 
#           oCell<-extract(ogrid,spt,cellnumbers=TRUE) # Extracts value
#           oHSI<-oCell[2]
#           oCell<-oCell[1]
#           print(oHSI)
#           print(oCell)
#           # what if point falls off model, find closest
#           if (is.na(oHSI)){
#             #yrtrawl[s,11]<-1 # mark it as off
#             opoints<-rasterToPoints(ogrid) # converts grid to points
#             pixcoor<-as.data.frame(opoints[,c(1:2)])# gets lat lons of all cell values
#             dist<-distm(spt, pixcoor, fun = distHaversine)  #asigns distance from point to all grid cells
#             dmin<-which((dist==min(dist))) #finds index of point with minimum distance
#             clpoint<-opoints[dmin,]   #uses index to identify closest point
#             ocCell<-extract(ogrid,cbind(clpoint[1],clpoint[2]),cellnumbers=TRUE) # Extracts the Cell and the value
#             
#             #           ##### Tests distance algo
#             #           test<-vgrid
#             #           test[sCell[1]]<-2 # make sCell a 2]
#             #           test[cCell[1]]<-2 # make sCell a 2
#             
#             oHSI<-ocCell[2]
#             oCell<-ocCell[1]
#           }
          
          yrtrawl[s,31]<-oHSI
          yrtrawl[s,32]<-oCell
          yrtrawl[s,33]<-modelDates

      } # end of s loop
      
            
    }

yrtrawl$dif<-yrtrawl$kHSIo-yrtrawl$kHSIv # identifies those affected by movement constraint
#strange<-subset(yrtrawl,yrtrawl$check==-1)
      ################################################################

yrsub<-yrtrawl[,c(26,28:34)]
Mack<-merge(studytrawl,yrsub,by='ID')

setwd("C:/Users/jpessutt/Documents/Projects/CoopResearch/Manderson/Mackerel/Mackerel/Ralph")
write.table(Mack,paste(yr,"_2vo_samples_allstrata.csv",sep=""),sep=",",col.names = TRUE,row.names = FALSE)  

}

#combine files
dflist<-list.files(pattern="_2vo_samples_allstrata.csv")
allSamples<-read.delim(dflist[1],sep=",",header=TRUE,row.names=NULL)
for (d in 2:length(dflist)){
  df<-read.delim(dflist[d],sep=",",header=TRUE,row.names=NULL)
  allSamples<-rbind(allSamples,df)
} 

write.table(allSamples,paste("All2vo_samples_allstrata.csv",sep=""),sep=",",col.names = TRUE,row.names = FALSE)  
############################################################################
#############################################################################
#############################################################################


#################################################################
#################################################################
#################################################################
