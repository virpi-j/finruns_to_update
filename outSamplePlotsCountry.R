#  rm(list=ls())
gc()
if(dev.interactive()) dev.off()

if(!exists("toFile")) toFile <- F
if(!exists("samplaus")) samplaus <- 0
if(!exists("landClassUnman")) landClassUnman <- NULL

library(dplyr)
library(ggplot2)

library(data.table)
print(toFile)
#vPREBAS <- "newVersion"
#vPREBAS <- "master"
if(!exists("nSegs")) nSegs <- 5000
if(!exists("fmi_from_allas")) fmi_from_allas <- F
if(!exists("save_fmi_data")) save_fmi_data <- F
if(!exists("delete_fmi_data")) delete_fmi_data <- T

library(tidyverse)
library(readxl)
regionNames <- c("Uusimaa", "Ahvenanmaa", "Keski-Pohjanmaa", "Pirkanmaa",
                 "Etela-Karjala", "Keski-Suomi", "Pohjois-Savo", 
                 "Lappi", "Kanta-Hame", "Pohjanmaa", "Varsinais-Suomi",
                 "Etela-pohjanmaa", "Paijat-Hame", "Satakunta", "Kymenlaakso",
                 "Kainuu", "Etela-Savo", "Pohjois-Karjala", "Pohjois-Pohjanmaa")
regionNames_fi <- c("Uusimaa", "Ahvenanmaa", "Keski-Pohjanmaa", "Pirkanmaa",
                    "Etelä-Karjala", "Keski-Suomi", "Pohjois-Savo", 
                    "Lappi", "Kanta-Häme", "Pohjanmaa", "Varsinais-Suomi",
                    "Etelä-Pohjanmaa", "Päijät-Häme", "Satakunta", "Kymenlaakso",
                    "Kainuu", "Etelä-Savo", "Pohjois-Karjala", "Pohjois-Pohjanmaa")
r_nos <- c(1, 21, 16, 6, 9, 13, 11, 19, 5, 15, 2, 14, 7, 4, 8, 18, 10, 12, 17) 
#regionNames <- fread("/scratch/project_2000994/PREBASruns/metadata/maakunta/maakunta_names.txt")
if(!exists("rids")) rids <- c(1,3:19)

NetSinks <- read_excel(path = "/users/vjunttil/finruns_to_update/LUKE_maak_nettonielu_kokeellinen.xlsx",  
                    sheet = "nettonielu", range = "A3:J24")
NetSinks <- NetSinks[-c(2),-c(2,3)]
NetSinks_per_ha <- read_excel(path = "/users/vjunttil/finruns_to_update/LUKE_maak_nettonielu_kokeellinen.xlsx",  
                       sheet = "nettonielu_per_ha", range = "A3:J24")
NetSinks_per_ha <- NetSinks_per_ha[-c(2),-c(2,3)]

## New Luke data
NetSinks_2025 <- read_excel(path = "/users/vjunttil/finruns_to_update/LUKE_maak_nettonielu_kokeellinen.xlsx",  
                            sheet = "nettonielu_2025", range = "A3:L22")
NetSinks_2025 <- NetSinks_2025[,-c(2,3)]
NetSinks_per_ha_2025 <- read_excel(path = "/users/vjunttil/finruns_to_update/LUKE_maak_nettonielu_kokeellinen.xlsx",  
                                   sheet = "nettonielu_per_ha_2025", 
                                   range = "A3:L22")
NetSinks_per_ha_2025 <- NetSinks_per_ha_2025[,-c(2,3)]

# min org from new LUke data
NetSinks_2025_min_org <- read_excel(path = "/users/vjunttil/finruns_to_update/LUKE_maak_nettonielu_kokeellinen.xlsx",  
                                    sheet = "nettonielu_2025_min_org", range = "A3:L98")
NetSinks_2025_min_org <- NetSinks_2025_min_org[,-c(3)]
NetSinks_2025_org <- NetSinks_2025_min <- NetSinks_2025
ri <- 1
for(ri in 1:nrow(NetSinks_2025)){
  nri <- which(NetSinks_2025_min_org[,1]==as.character(NetSinks_2025[ri,1]))
  NetSinks_2025_min[ri,-1] <- t(as.numeric(unlist(colSums(NetSinks_2025_min_org[c(nri,nri+2),-c(1:2)]))))
  NetSinks_2025_org[ri,-1] <- t(as.numeric(unlist(colSums(NetSinks_2025_min_org[c(nri+1,nri+3:4),-c(1:2)]))))
}

NetSinks_per_ha_2025_min_org <- read_excel(path = "/users/vjunttil/finruns_to_update/LUKE_maak_nettonielu_kokeellinen.xlsx",  
                                    sheet = "nettonielu_per_ha_2025_min_org", range = "A3:L98")
NetSinks_per_ha_2025_min_org <- NetSinks_per_ha_2025_min_org[,-c(3)]
NetSinks_per_ha_2025_org <- NetSinks_per_ha_2025_min <- NetSinks_per_ha_2025
ri <- 1
for(ri in 1:nrow(NetSinks_2025)){
  nri <- which(NetSinks_per_ha_2025_min_org[,1]==as.character(NetSinks_per_ha_2025[ri,1]))
  NetSinks_per_ha_2025_min[ri,-1] <- t(as.numeric(unlist(colSums(NetSinks_per_ha_2025_min_org[c(nri,nri+2),-c(1:2)]))))
  NetSinks_per_ha_2025_org[ri,-1] <- t(as.numeric(unlist(colSums(NetSinks_per_ha_2025_min_org[c(nri+1,nri+3:4),-c(1:2)]))))
}



##
V2015 <- read_excel(path = "/users/vjunttil/finruns_to_update/VMIstats.xlsx",  
                    sheet = "tilavuus", range = "B3:G25")
V2021 <- read_excel(path = "/users/vjunttil/finruns_to_update/VMIstats.xlsx",  
                    sheet = "tilavuus", range = "B26:G47")
W2015 <- read_excel(path = "/users/vjunttil/finruns_to_update/VMIstats.xlsx",  
                    sheet = "biomassa", range = "B3:V26")
W2021 <- read_excel(path = "/users/vjunttil/finruns_to_update/VMIstats.xlsx",  
                    sheet = "biomassa", range = "B27:V48")
gg2015 <- read_excel(path = "/users/vjunttil/finruns_to_update/VMIstats.xlsx",  
                    sheet = "kasvu", range = "B3:H25")
gg2021 <- read_excel(path = "/users/vjunttil/finruns_to_update/VMIstats.xlsx",  
                     sheet = "kasvu", range = "B26:H47")
landclass2015 <- read_excel(path = "/users/vjunttil/finruns_to_update/VMIstats.xlsx",  
                     sheet = "maaluokat", range = "B3:D25")
landclass2021 <- read_excel(path = "/users/vjunttil/finruns_to_update/VMIstats.xlsx",  
                            sheet = "maaluokat", range = "B26:D47")
ikaluokat2015 <- read_excel(path = "/users/vjunttil/finruns_to_update/VMIstats.xlsx",  
                            sheet = "ikaluokat_metsamaa", range = "B3:L25")
ikaluokat2021 <- read_excel(path = "/users/vjunttil/finruns_to_update/VMIstats.xlsx",  
                            sheet = "ikaluokat_metsamaa", range = "B26:L47")
lajiV2015 <- read_excel(path = "/users/vjunttil/finruns_to_update/VMIstats.xlsx",  
                            sheet = "tilavuus", range = "B3:H25")
lajiV2021 <- read_excel(path = "/users/vjunttil/finruns_to_update/VMIstats.xlsx",  
                        sheet = "tilavuus", range = "B26:H47")

# minDharvX = 999
setwd("/scratch/project_2000994/PREBASruns/PREBAStesting/")

#run_settings <- paste0("_clcutArFact",clcutArFact,
#                       "_addHarv",compHarvX,"_landClassX",range(landClassX)[1],
#                       "to",range(landClassX)[2],"_mortMod",mortMod)


meanRegion <- data.table()
# areasCountry <- data.table()
# areasProtectCountry <- data.table()
# dataCountry <- data.table()
# dataProtectCountry <- data.table()
#strangeSites <- NULL
#areaAllRegions <- NULL
outDir <- "/scratch/project_2000994/PREBASruns/PREBAStesting/RegionRuns/"
CSCrun <- T
station_id <- "finRuns"

g_to_kg <- 1000
ha_to_m2 <- 100^2
nYears <- 35

manualRun <- T
if(manualRun){
  RCP=0
  harvScen <- "Base"
  harvInten <- "Base"
  easyInit=FALSE; forceSaveInitSoil=F; cons10run = F
  procDrPeat=T; coeffPeat1=-240; coeffPeat2=70
  coefCH4 = 0.34; coefN20_1 = 0.23; coefN20_2 = 0.077#g m-2 y-1
  funPreb = "regionPrebas"
  initSoilCreStart=NULL; outModReStart=NULL; reStartYear=1
  sampleX=NULL; #P0currclim=NA; fT0=NA
  sampleID = 1; initAge=NA; disturbanceON <- NA; ingrowth <- F; TminTmax <- NA
}

results <- array(0,c(8,nYears,length(rids),2))
dimnames(results) <- list(c("grossgrowth","V","Vharvested", "NEE", "Wharvested", "CH4em", "N2Oem","NBE"),1:nYears,regionNames[rids],c("sum","ave"))

r_noi <- 1

if(!exists("ECMmod")) ECMmod <- 0
if(!exists("soilGridData")) soilGridData <- 0

#if(!toFile) rids <- rids[1:3]
fname <- paste0("results_agesample",samplaus,
                "_ECMmod",ECMmod,"_soilGridData",soilGridData,"_",rcps,"_HcModInit",HcMod_Init,"_fertupdt",fertUpdt)
if(toFile) pdf(paste0(outDir,fname,".pdf"))
if(!exists("FIGsOnly")) FIGsOnly <- F
if(!FIGsOnly){
  for(r_noi in 1:length(rids)){
    if(r_noi>1) noPrebasLoading <- T
    toMem <- ls()
    set.seed(1)
    r_no <- rids[r_noi]
    rname <- regionNames[r_no]
    rname_fi <- regionNames_fi[r_no]
    rnameid <- r_nos[r_no]
    mortMod <- 13
    landClassX <- 1:2
    if(!exists("compHarvX")) compHarvX <- 0
    if(!exists("thinFactX")) thinFactX <- 0.25
    #  print(paste("CompHarv =", compHarvX))
    source("~/finruns_to_update/settings.R")
    noPrebasLoading <- T # After the first installation, don't load prebas again
    
    areasLandClass2015 <- as.numeric(landclass2015[which(landclass2015[,1]==rname_fi),2:3])
    areasLandClass2021 <- as.numeric(landclass2021[which(landclass2021[,1]==rname_fi),2:3])
    Vstats <- as.numeric(c(V2015[which(V2015[,1]==rname_fi),ncol(V2015)],
                           V2021[which(V2021[,1]==rname_fi),ncol(V2015)]))*
      1e6/1000/as.numeric(c(sum(areasLandClass2015),sum(areasLandClass2021)))  
    
    ggstats <- as.numeric(c(gg2015[which(gg2015[,1]==rname_fi),ncol(gg2015)],
                            gg2021[which(gg2021[,1]==rname_fi),ncol(gg2015)]))
    wstats <- 0.5*as.numeric(c(W2015[which(W2015[,1]==rname_fi),ncol(W2015)],
                               W2021[which(W2021[,1]==rname_fi),ncol(W2015)]))*
      1e6/as.numeric(c(sum(areasLandClass2015),sum(areasLandClass2021)))  
    ageclassstats <- rbind(as.numeric(ikaluokat2015[which(ikaluokat2015[,1]==rname_fi),-1]),
                           as.numeric(ikaluokat2021[which(ikaluokat2021[,1]==rname_fi),-1]))
    ageclassstats <- ageclassstats[,-ncol(ageclassstats)]
    ageclassstats[1,] <- cumsum(ageclassstats[1,]/areasLandClass2015[1])
    ageclassstats[2,] <- cumsum(ageclassstats[2,]/areasLandClass2021[1])
    lajistats2015 <- as.numeric(V2015[which(V2015[,1]==rname_fi),2:5])
    lajistats2021 <- as.numeric(V2021[which(V2021[,1]==rname_fi),2:5])
    netsinksreg <- NetSinks[which(NetSinks[,1]==regionNames_fi[r_no]),-1]
    netsinksreg[which(netsinksreg=="..")] <- NA  
    netsinksreg <- as.numeric(netsinksreg)
    netsinksreg_per_ha <- NetSinks_per_ha[which(NetSinks_per_ha[,1]==regionNames_fi[r_no]),-1]
    netsinksreg_per_ha[which(netsinksreg_per_ha=="..")] <- NA  
    netsinksreg_per_ha <- as.numeric(netsinksreg_per_ha)
    
    netsinksreg2025 <- NetSinks_2025[which(NetSinks_2025[,1]==regionNames_fi[r_no]),-1]
    netsinksreg2025[which(netsinksreg2025=="..")] <- NA  
    netsinksreg2025 <- as.numeric(netsinksreg2025)
    netsinksreg_per_ha2025 <- NetSinks_per_ha_2025[which(NetSinks_per_ha_2025[,1]==regionNames_fi[r_no]),-1]
    netsinksreg_per_ha2025[which(netsinksreg_per_ha2025=="..")] <- NA  
    netsinksreg_per_ha2025 <- as.numeric(netsinksreg_per_ha2025)

    netsinksreg2025_min <- NetSinks_2025_min[which(NetSinks_2025_min[,1]==regionNames_fi[r_no]),-1]
    netsinksreg2025_min[which(netsinksreg2025_min=="..")] <- NA  
    netsinksreg2025_min <- as.numeric(netsinksreg2025_min)
    netsinksreg_per_ha2025_min <- NetSinks_per_ha_2025_min[which(NetSinks_per_ha_2025_min[,1]==regionNames_fi[r_no]),-1]
    netsinksreg_per_ha2025_min[which(netsinksreg_per_ha2025_min=="..")] <- NA  
    netsinksreg_per_ha2025_min <- as.numeric(netsinksreg_per_ha2025_min)
    
    netsinksreg2025_org <- NetSinks_2025_org[which(NetSinks_2025_org[,1]==regionNames_fi[r_no]),-1]
    netsinksreg2025_org[which(netsinksreg2025_org=="..")] <- NA  
    netsinksreg2025_org <- as.numeric(netsinksreg2025_org)
    netsinksreg_per_ha2025_org <- NetSinks_per_ha_2025_org[which(NetSinks_per_ha_2025_org[,1]==regionNames_fi[r_no]),-1]
    netsinksreg_per_ha2025_org[which(netsinksreg_per_ha2025_org=="..")] <- NA  
    netsinksreg_per_ha2025_org <- as.numeric(netsinksreg_per_ha2025_org)

    print(paste("Start running region",r_no,"/",rname))
  #  landclass <- c(sum(data.all$area[which(data.all$landclass==1)]),
  #                              sum(data.all$area[which(data.all$landclass==2)]))  
    
    
    if(TRUE){
      print("Get coordinates...")
      load(paste0("/scratch/project_2000994/PREBASruns/finRuns/input/maakunta/maakunta_",r_no,"_IDsTab.rdata"))
      data.IDs <- data.IDs[segID!=0]
      data.IDs$segID <- data.IDs$maakuntaID
      setkey(data.IDs,segID)
      setkey(data.all,segID)
      #setkey(data.IDs,maakuntaID)
      
      tabX <- merge(data.IDs,data.all)
      ntabX <- tabX[,.I[which.max(y)],by=segID]$V1
      data.all <- cbind(data.all, tabX[ntabX,c("x","y")])
      rm(list=c("tabX","ntabX","data.IDs"))
      gc()
      
      #set_thin_PROJ6_warnings(TRUE)
      xy <- data.all[,c("segID","x","y")]
      coordinates(xy) <- c("x","y")
      proj4string(xy) <- crsX
      #cord = SpatialPoints(xy, proj4string=CRS("+init=EPSG:3067"))
      location<-as.data.frame(spTransform(xy, CRS("+init=epsg:4326")))
      data.all$lon <- location$coords.x1#location$y
      data.all$lat <- location$coords.x2#location$y
      rm(list=c("xy","location"))
      gc()
      print("done.")
      
      #data.all <- cbind(data.all,data.IDs[match(data.all$segID, data.IDs$maakuntaID),4:5])
      finPeats <- raster("/scratch/project_2000994/MVMIsegments/segment-IDs/pseudopty.img")
      drPeatID <- 400 # ID = 400 for luke database; drained peatland
      print(drPeatID)
      undrPeatID <- 700  ### ID = 700 for luke database; undrained peatland
      if(FALSE){#file.exists(paste0("uncRuns/peatID_reg",r_no,".rdata"))){
        load(paste0("uncRuns/peatID_reg",r_no,".rdata"))
      } else {
        print("Extract peatIDs...")
        peatIDs <-extract(finPeats, cbind(data.all$x,data.all$y))
      }
      data.all[,peatID:=peatIDs]
      data.all$peatID[which(data.all$peatID==100)]<-0
      data.all$peatID[which(data.all$peatID==400)]<-1
      data.all$peatID[which(data.all$peatID==700)]<-2
      data.all <- data.all[which(data.all$peatID!=2),]
      rm(list=c("finPeats","peatIDs"))
      #print(unique(data.all$fert[which(data.all$landclass>1)]))
      #data.all$fert[which(data.all$landclass>1)] <- 9
      #print(unique(data.all$fert[which(data.all$landclass>1)]))
      gc()
    }
    areaRegion <- totArea <- sum(data.all$area,na.rm=T)
    
  
    if(samplaus==0){
      set.seed(1)
      dataS <- data.all[sample(1:nrow(data.all),nSegs,replace = F)]
    } else if(samplaus==1){
      if(newSample){
        par(mfrow=c(1,1))
        ## correction of data.all
        if(TRUE){
          data.all$decid <- data.all$birch+data.all$decid
          data.all$birch <- 0
          dataS <- data.all[sample(1:nrow(data.all),nSegs,replace = F),]
          #dataS$decid <- dataS$birch + dataS$decid
          #dataS$birch <- 0
          gc()
          print("region specific qq-correction of initial state data, start...")
          #load("~/finruns_to_update/quantile_data_2021.rdata")
          load(paste0("~/finruns_to_update/quantile_data_2021_landclass12_",
                      r_no,"_",regionNames_fi[r_no],".rdata"))
          if(max(data.all$ba)>max(qMSNFI[[1]]$x)) qMSNFI[[1]] <- rbind(qMSNFI[[1]],data.table(ecdf=1,x=max(data.all$ba)*1.05))
          if(max(data.all$decid)>max(qMSNFI[[2]]$x)) qMSNFI[[2]] <- rbind(qMSNFI[[2]],data.table(ecdf=1,x=max(data.all$decid)*1.05))
          if(max(data.all$pine)>max(qMSNFI[[3]]$x)) qMSNFI[[3]] <- rbind(qMSNFI[[3]],data.table(ecdf=1,x=max(data.all$pine)*1.05))
          if(max(data.all$spruce)>max(qMSNFI[[4]]$x)) qMSNFI[[4]] <- rbind(qMSNFI[[4]],data.table(ecdf=1,x=max(data.all$spruce)*1.05))
          if(max(data.all$age)>max(qMSNFI[[6]]$x)) qMSNFI[[6]] <- rbind(qMSNFI[[6]],data.table(ecdf=1,x=max(data.all$age)*1.05))
          if((max(data.all$h)/10)>max(qMSNFI[[7]]$x)) qMSNFI[[7]] <- rbind(qMSNFI[[7]],data.table(ecdf=1,x=max(data.all$h/10)*1.05))
          if(max(data.all$dbh)>max(qMSNFI[[8]]$x)) qMSNFI[[8]] <- rbind(qMSNFI[[8]],data.table(ecdf=1,x=max(data.all$dbh)*1.05))
          source("~/finruns_to_update/correction_function.R")
          ii <- 1
          bamax <- max(data.all$ba)
          dataS <- data.all
          dataCorr <- function(ii){
            if(ii%%1e5==0) print(paste(ii,"/",nrow(dataS)))
            dataSout<- array(c(min(bamax*1.1,correction_f(dataS$ba[ii],1)),
                               correction_f(dataS$decid[ii],2),
                               correction_f(dataS$pine[ii],3),
                               correction_f(dataS$spruce[ii],4),
                               correction_f(dataS$age[ii],6),
                               correction_f(dataS$h[ii]/10,7)*10,
                               correction_f(dataS$dbh[ii],8)),c(1,7))
            return(dataSout)
          }
          dataS <- apply(array(1:nrow(data.all),c(nrow(data.all),1)),1:2,dataCorr)
          print("done.")
          dataS <- data.table(t(dataS[,,1]))
          colnames(dataS) <- c("ba","decid","pine","spruce","age","h","dbh")
          dataS[which(dataS$age==0),c("ba","decid","pine","spruce","age","h","dbh")]<-0
          dataS[which(dataS$h==0),c("ba","decid","pine","spruce","age","h","dbh")]<-0
          if(FIGS){
            par(mfrow=c(3,2))
            ni <- sample(1:nrow(data.all),1000,replace = F)
            ni2 <- sample(1:nrow(dataS),1000,replace = F)
            plot(data.all$ba[ni],data.all$age[ni],pch=19,cex=0.2,
                 ylim=c(0,max(c(data.all$age[ni],dataS$age))),
                 xlim=c(0,max(c(data.all$ba[ni],dataS$ba))),
                 main=paste0(regionNames_fi[r_no],": black MSNFI, red MSNFIcorr"))
            points(dataS$ba[ni2],dataS$age[ni2],col="red",pch=19,cex=0.2)
            plot(data.all$ba[ni],data.all$pine[ni],pch=19,cex=0.2,
                 ylim=c(0,max(c(data.all$pine[ni],dataS$pine))),
                 xlim=c(0,max(c(data.all$ba[ni],dataS$ba))))
            points(dataS$ba[ni2],dataS$pine[ni2],col="red",pch=19,cex=0.2)
            plot(data.all$ba[ni],data.all$spruce[ni],pch=,cex=0.219,
                 ylim=c(0,max(c(data.all$spruce[ni],dataS$spruce))),
                 xlim=c(0,max(c(data.all$ba[ni],dataS$ba))))
            points(dataS$ba[ni2],dataS$spruce[ni2],col="red",pch=19,cex=0.2)
            plot(data.all$ba[ni],data.all$decid[ni],pch=19,cex=0.2,
                 ylim=c(0,max(c(data.all$decid[ni],dataS$decid))),
                 xlim=c(0,max(c(data.all$ba[ni],dataS$ba))))
            points(dataS$ba[ni2],dataS$decid[ni2],col="red",pch=19,cex=0.2)
            plot(data.all$ba[ni],data.all$dbh[ni],pch=19,cex=0.2,
                 ylim=c(0,max(c(data.all$dbh[ni],dataS$dbh))),
                 xlim=c(0,max(c(data.all$ba[ni],dataS$ba))),
                 main=paste0(regionNames_fi[r_no],": black MSNFI, red MSNFIcorr"))
            points(dataS$ba[ni2],dataS$dbh[ni2],col="red",pch=19,cex=0.2)
            plot(data.all$h[ni],data.all$dbh[ni],pch=19,cex=0.2,
                 ylim=c(0,max(c(data.all$dbh[ni],dataS$dbh))),
                 xlim=c(0,max(c(data.all$h[ni],dataS$h))))
            points(dataS$h[ni2],dataS$dbh[ni2],col="red",pch=19,cex=0.2)
          }
          data.all[,c("ba","decid","pine","spruce","age","h","dbh")] <- dataS
          rm("dataS")
          gc()
        }
        
        ##
        data.all$age <- round(data.all$age)
        sampleArea <- nSegs*median(data.all$area)*1.3
        sample_weight <- as.numeric(ikaluokat2015[which(ikaluokat2015[,1]==rname_fi),2:(ncol(ikaluokat2015)-1)])
        sample_weight_lc1 <- sample_weight/sum(sample_weight)
        #ikaid <- array(0,c(nrow(data.all),1))
        ages <- round(data.all$age)
        agelimits <- c(0,20,40,60,80,100,120,140,1e4)
        
        agelimitsii <- 0:max(150,max(data.all$age))
        agelimits[length(agelimits)] <- agelimitsii[length(agelimitsii)]
        pagelimitsii <- pagedata <- pagedata_lc1 <- pagesample <- pagesample_lc1 <- array(0,c(length(agelimitsii),1))
        pwages <- cumsum(sample_weight_lc1)
        n_lc1 <- which(data.all$landclass==1)
        n_lc2 <- which(data.all$landclass==2)
        ages <- ages[n_lc1]
        totArea_lc1 <- sum(data.all$area[n_lc1])
        ii <- 1
        for(ii in 1:length(agelimitsii)){
          if(agelimitsii[ii]%in%agelimits){
            iiprev <- which(agelimits==agelimitsii[ii])
            ageprev <- agelimitsii[ii]
            pagelimitsii[ii] <- pwages[iiprev]
          } else {
            pagelimitsii[ii] <- pwages[iiprev] + 
              (pwages[iiprev+1]-pwages[iiprev])/
              (agelimits[iiprev+1]-(ageprev))*(agelimitsii[ii]-(ageprev))
          }
          pagedata_lc1[ii] <- sum(data.all$area[n_lc1[which(data.all$age[n_lc1]<=agelimitsii[ii])]])/totArea_lc1
          pagedata[ii] <- sum(data.all$area[which(data.all$age<=agelimitsii[ii])])/totArea
        }
        par(mfrow=c(1,1))
        plot(agelimitsii, pagelimitsii, type="l", xlab="age", ylab="cumsum(area) quantiles")
        points(agelimits, pwages, pch=19)
        lines(agelimitsii,pagedata, col="red")
        lines(agelimitsii,pagedata_lc1, col="brown")
        
        ii <- 1
        nirandom <- NULL
        areashares <- array(0,c(length(agelimitsii),1))
        pareashares <- c(pagelimitsii[1],pagelimitsii[-1]-pagelimitsii[-length(pagelimitsii)])
        pareashares2 <- pareashares/sum(pareashares)*sampleArea
        #    pareashares2 <- pareashares/sum(pareashares)*totArea*nSegs/nrow(data.all)
        for(ii in 1:length(agelimitsii)){
          agei <- agelimitsii[ii]
          ni <- which(ages==agei)
          #print(paste("age",agei,":",length(ni)))
          if(length(ni)==0){
            ni <- which(abs(ages-ages[which.min((ages-agei)^2)[1]])<=4)
          }
          while(areashares[ii]<=1*pareashares2[ii]){
            nii <- ni[sample(1:length(ni),1)]
            nirandom <- c(nirandom,nii)
            areashares[ii] <- areashares[ii]+data.all$area[n_lc1[nii]]
          }
          if(FALSE){
            while(areashares[ii]<=pagelimitsii[ii]){
              nirandom <- c(nirandom,ni[sample(1:length(ni),1)])
              areashares[ii] <- sum(data.all$area[n_lc1[nirandom]])/sampleArea
              #areashares[ii] <- areashares[ii]+data.all$area[n_lc1[nii]]
            }}
        }
        nirandom <- n_lc1[nirandom]
        ii<-1
        for(ii in 1:length(agelimitsii)){
          pagesample_lc1[ii] <- sum(data.all$area[nirandom[
            which(data.all$age[nirandom]<=agelimitsii[ii])]])/sum(data.all$area[nirandom])
        }
        
        # add landclass 2 segments
        nLC2 <- round(length(n_lc2)/nrow(data.all)*length(nirandom))
        nirandom <- c(nirandom,n_lc2[sample(1:length(n_lc2),nLC2,replace = T)])
        set.seed(1)
        print(paste("Sampled segments",length(nirandom),"versus nSegs =",nSegs))
        if(length(nirandom)>nSegs){ 
          nirandom <- nirandom[sample(1:length(nirandom),nSegs,replace=F)]
        } else {
          nirandom <- nirandom[sample(1:length(nirandom),nSegs,replace=T)]
        }
        dataS <- data.all[nirandom,]
        for(ii in 1:length(agelimitsii)){
          pagesample[ii] <- sum(dataS$area[which(dataS$age<=agelimitsii[ii])])/sum(dataS$area)
        }
        lines(agelimitsii, pagesample_lc1, col="pink",lwd=2)
        lines(agelimitsii, pagesample, col="green")
        legend(x = "topleft", box.col = "black", 
               lty = c(NA,1,1,1,1,1),
               pch = c(19,NA,NA,NA,NA,NA),
               lwd = c(1,1,1,1,1,2),
               col = c("black","black","red","brown","green","pink"),
               #bg ="yellow", box.lwd = 2 , #title="EQUATIONS",  
               legend=c("VMIstats_lc1","VMIstats lin.line_lc1", "MVMI",
                        "MVMI_lc1","sample","sample_lc1"))  
        
        if(FALSE){
          for(ij in 1:length(agelimits)){
            if(ij == 1){ ikaid[which(ages==agelimits[1])] <- ij
            } else if(ij==length(agelimits)){
              ikaid[which(ages>(agelimits[ij-1]+1))] <- ij
            } else {
              ikaid[which(ages>(agelimits[ij-1]+10) & ages<=agelimits[ij])]<-ij  
            }
          }
          ikaid[which(data.all$landclass==2)] <- 0
          ikaid <- ikaid + 1
          
          w2 <- areasLandClass2015/sum(areasLandClass2015)
          sample_weight <- c(w2[2],sample_weight_lc1*w2[1]) # prob for all landclass 2, probs for landclass 1 by age
          
          nirandom <- NULL
          for(id in 1:length(unique(ikaid))){
            ni <- which(ikaid==id)
            ni <- ni[sample(1:length(ni),nSegs,replace = T)]
            ni <- ni[which(cumsum(data.all$area[ni])<= sample_weight[id]*sampleArea)]
            nirandom <- c(nirandom,ni)
          }
          dataS <- data.all[nirandom[sample(1:length(nirandom),nSegs,replace=F)],]
        }
        print("save sample")
        save(dataS,file=paste0("/scratch/project_2000994/PREBASruns/PREBAStesting/RegionRuns/dataS_",
                               r_no,"_",nSegs,".rdata"))
      } else {
        print("Load old sample data")
        load(file=paste0("/scratch/project_2000994/PREBASruns/PREBAStesting/RegionRuns/dataS_",
                               r_no,"_",nSegs,".rdata"))
      }
    } else {
      data.all$decid <- data.all$birch+data.all$decid
      data.all$birch <- 0
      set.seed(1)
      dataS <- data.all[sample(1:nrow(data.all),nSegs,replace = F),]
      #dataS$decid <- dataS$birch + dataS$decid
      #dataS$birch <- 0
      gc()
      #dataSorig <- dataS
      if(samplaus==2){
        print("qq-correction of initial state data, start...")
        #load("~/finruns_to_update/quantile_data_2021.rdata")
        load("~/finruns_to_update/quantile_data_2021_landclass12.rdata")
        if(NFIlocal){
          VMIages <- as.numeric(ikaluokat2015[which(ikaluokat2015[,1]==rname_fi),2:(ncol(ikaluokat2015)-1)])
          VMIxs <- c(0,1,20,40,60,80,100,120,140,max(qFC[[6]]$x),max(qFC[[6]]$x)*1.01)
          eVMI <- cumsum(VMIages)/sum(VMIages)
          eVMI <- c(eVMI[1]-.0001,eVMI,1)
          qFC[[6]] <- data.table(ecdf=eVMI,x=VMIxs)
          ex <- sort(unique(round(data.all$age)))
          ex <- c(ex,max(ex)*1.01)
          eMSNFI <- ex*0
          exi <- 1
          totA <- sum(data.all$area)
          for(exi in 1:length(ex)){
            eMSNFI[exi] <- sum(data.all$area[which(data.all$age<=ex[exi])])/totA
          }
          qMSNFI[[6]] <- data.table(ecdf=eMSNFI,x=ex)
          if(TEST){
            ex <- sort(unique(round(data.all$ba)))
            ex <- c(ex,max(ex)*1.01)
            eMSNFI <- ex*0
            for(exi in 1:length(ex)){
              eMSNFI[exi] <- sum(data.all$area[which(data.all$ba<=ex[exi])])/totA
            }
            qMSNFI[[1]] <- data.table(ecdf=eMSNFI,x=ex)
            ##
            ex <- sort(unique(round(data.all$decid)))
            ex <- c(ex,max(ex)*1.01)
            eMSNFI <- ex*0
            for(exi in 1:length(ex)){
              eMSNFI[exi] <- sum(data.all$area[which(data.all$decid<=ex[exi])])/totA
            }
            qMSNFI[[2]] <- data.table(ecdf=eMSNFI,x=ex)
            ##
            ex <- sort(unique(round(data.all$pine)))
            ex <- c(ex,max(ex)*1.01)
            eMSNFI <- ex*0
            for(exi in 1:length(ex)){
              eMSNFI[exi] <- sum(data.all$area[which(data.all$pine<=ex[exi])])/totA
            }
            qMSNFI[[3]] <- data.table(ecdf=eMSNFI,x=ex)
            ##
            ex <- sort(unique(round(data.all$spruce)))
            ex <- c(ex,max(ex)*1.01)
            eMSNFI <- ex*0
            for(exi in 1:length(ex)){
              eMSNFI[exi] <- sum(data.all$area[which(data.all$spruce<=ex[exi])])/totA
            }
            qMSNFI[[4]] <- data.table(ecdf=eMSNFI,x=ex)
            ##
            ex <- sort(unique(round(data.all$h)))
            ex <- c(ex,max(ex)*1.01)
            eMSNFI <- ex*0
            for(exi in 1:length(ex)){
              eMSNFI[exi] <- sum(data.all$area[which(data.all$h<=ex[exi])])/totA
            }
            qMSNFI[[7]] <- data.table(ecdf=eMSNFI,x=ex)
            ##
            ex <- sort(unique(round(data.all$dbh)))
            ex <- c(ex,max(ex)*1.01)
            eMSNFI <- ex*0
            for(exi in 1:length(ex)){
              eMSNFI[exi] <- sum(data.all$area[which(data.all$dbh<=ex[exi])])/totA
            }
            qMSNFI[[8]] <- data.table(ecdf=eMSNFI,x=ex)
            
          }
        }
        source("~/finruns_to_update/correction_function.R")
        ii <- 1
        for(ii in 1:nSegs){
          dataS[ii,"ba"] <- min(max(data.all$ba)*1.1,correction_f(dataS$ba[ii],1))
          dataS[ii,"decid"] <- correction_f(dataS$decid[ii],2)
          dataS[ii,"pine"] <- correction_f(dataS$pine[ii],3)
          dataS[ii,"spruce"] <- correction_f(dataS$spruce[ii],4)
          dataS[ii,"age"] <- correction_f(dataS$age[ii],6)
          dataS[ii,"h"] <- correction_f(dataS$h[ii]/10,7)*10
          dataS[ii,"dbh"] <- correction_f(dataS$dbh[ii],8)  
        }
        print("done.")
        dataS[which(dataS$age==0),c("ba","decid","pine","spruce","age","h","dbh")]<-0
        dataS[which(dataS$h==0),c("ba","decid","pine","spruce","age","h","dbh")]<-0
        if(FALSE){
          par(mfrow=c(2,2))
          ni <- sample(1:nrow(data.all),1000,replace = F)
          ni2 <- sample(1:nrow(dataS),1000,replace = F)
          plot(data.all$ba[ni],data.all$age[ni],pch=19,cex=0.2,
               ylim=c(0,max(c(data.all$age[ni],dataS$age))),
               xlim=c(0,max(c(data.all$ba[ni],dataS$ba))))
          points(dataS$ba[ni2],dataS$age[ni2],col="red",pch=19,cex=0.2)
          plot(data.all$ba[ni],data.all$pine[ni],pch=19,cex=0.2,
               ylim=c(0,max(c(data.all$pine[ni],dataS$pine))),
               xlim=c(0,max(c(data.all$ba[ni],dataS$ba))))
          points(dataS$ba[ni2],dataS$pine[ni2],col="red",pch=19,cex=0.2)
          plot(data.all$ba[ni],data.all$spruce[ni],pch=,cex=0.219,
               ylim=c(0,max(c(data.all$spruce[ni],dataS$spruce))),
               xlim=c(0,max(c(data.all$ba[ni],dataS$ba))))
          points(dataS$ba[ni2],dataS$spruce[ni2],col="red",pch=19,cex=0.2)
          plot(data.all$h[ni],data.all$dbh[ni],pch=19,cex=0.2,
               ylim=c(0,max(c(data.all$dbh[ni],dataS$dbh))),
               xlim=c(0,max(c(data.all$h[ni],dataS$h))))
          points(dataS$h[ni2],dataS$dbh[ni2],col="red",pch=19,cex=0.2)
        }
      
      } else if(samplaus==3){
        print("region specific qq-correction of initial state data, start...")
        #load("~/finruns_to_update/quantile_data_2021.rdata")
        load(paste0("~/finruns_to_update/quantile_data_2021_landclass12_",
        r_no,"_",regionNames_fi[r_no],".rdata"))
        if(max(data.all$ba)>max(qMSNFI[[1]]$x)) qMSNFI[[1]] <- rbind(qMSNFI[[1]],data.table(ecdf=1,x=max(data.all$ba)*1.05))
        if(max(data.all$decid)>max(qMSNFI[[2]]$x)) qMSNFI[[2]] <- rbind(qMSNFI[[2]],data.table(ecdf=1,x=max(data.all$decid)*1.05))
        if(max(data.all$pine)>max(qMSNFI[[3]]$x)) qMSNFI[[3]] <- rbind(qMSNFI[[3]],data.table(ecdf=1,x=max(data.all$pine)*1.05))
        if(max(data.all$spruce)>max(qMSNFI[[4]]$x)) qMSNFI[[4]] <- rbind(qMSNFI[[4]],data.table(ecdf=1,x=max(data.all$spruce)*1.05))
        if(max(data.all$age)>max(qMSNFI[[6]]$x)) qMSNFI[[6]] <- rbind(qMSNFI[[6]],data.table(ecdf=1,x=max(data.all$age)*1.05))
        if((max(data.all$h)/10)>max(qMSNFI[[7]]$x)) qMSNFI[[7]] <- rbind(qMSNFI[[7]],data.table(ecdf=1,x=max(data.all$h/10)*1.05))
        if(max(data.all$dbh)>max(qMSNFI[[8]]$x)) qMSNFI[[8]] <- rbind(qMSNFI[[8]],data.table(ecdf=1,x=max(data.all$dbh)*1.05))
        if(NFIlocal){
          VMIages <- as.numeric(ikaluokat2015[which(ikaluokat2015[,1]==rname_fi),2:(ncol(ikaluokat2015)-1)])
          VMIxs <- c(0,1,20,40,60,80,100,120,140,max(qFC[[6]]$x),max(qFC[[6]]$x)*1.01)
          eVMI <- cumsum(VMIages)/sum(VMIages)
          eVMI <- c(eVMI[1]-.0001,eVMI,1)
          qFCNFI <- data.table(ecdf=eVMI,x=VMIxs)
          if(!TEST) qFC[[6]] <- qFCNFI
          ex <- sort(unique(round(data.all$age)))
          ex <- c(ex,max(ex)*1.01)
          eMSNFI <- ex*0
          exi <- 1
          totA <- sum(data.all$area)
          for(exi in 1:length(ex)){
            eMSNFI[exi] <- sum(data.all$area[which(data.all$age<=ex[exi])])/totA
          }
          qMSNFI_NFI <- data.table(ecdf=eMSNFI,x=ex)
          if(!TEST) qMSNFI[[6]] <- qMSNFI_NFI
          if(TEST){
            ageS <- dataS$age
            source("~/finruns_to_update/correction_function.R")
            for(ii in 1:nSegs){
              ageS[ii] <- correction_f(dataS$age[ii],6,ecdfx = qMSNFI_NFI,ecdfz = qFCNFI)
            }
            if(FALSE){
              ex <- sort(unique(round(data.all$ba)))
              ex <- c(ex,max(ex)*1.01)
              eMSNFI <- ex*0
              for(exi in 1:length(ex)){
                eMSNFI[exi] <- sum(data.all$area[which(data.all$ba<=ex[exi])])/totA
              }
              qMSNFI[[1]] <- data.table(ecdf=eMSNFI,x=ex)
              ##
              ex <- sort(unique(round(data.all$decid)))
              ex <- c(ex,max(ex)*1.01)
              eMSNFI <- ex*0
              for(exi in 1:length(ex)){
                eMSNFI[exi] <- sum(data.all$area[which(data.all$decid<=ex[exi])])/totA
              }
              qMSNFI[[2]] <- data.table(ecdf=eMSNFI,x=ex)
              ##
              ex <- sort(unique(round(data.all$pine)))
              ex <- c(ex,max(ex)*1.01)
              eMSNFI <- ex*0
              for(exi in 1:length(ex)){
                eMSNFI[exi] <- sum(data.all$area[which(data.all$pine<=ex[exi])])/totA
              }
              qMSNFI[[3]] <- data.table(ecdf=eMSNFI,x=ex)
              ##
              ex <- sort(unique(round(data.all$spruce)))
              ex <- c(ex,max(ex)*1.01)
              eMSNFI <- ex*0
              for(exi in 1:length(ex)){
                eMSNFI[exi] <- sum(data.all$area[which(data.all$spruce<=ex[exi])])/totA
              }
              qMSNFI[[4]] <- data.table(ecdf=eMSNFI,x=ex)
              ##
              ex <- sort(unique(round(data.all$h)))
              ex <- c(ex,max(ex)*1.01)
              eMSNFI <- ex*0
              for(exi in 1:length(ex)){
                eMSNFI[exi] <- sum(data.all$area[which(data.all$h<=ex[exi])])/totA
              }
              qMSNFI[[7]] <- data.table(ecdf=eMSNFI,x=ex)
              ##
              ex <- sort(unique(round(data.all$dbh)))
              ex <- c(ex,max(ex)*1.01)
              eMSNFI <- ex*0
              for(exi in 1:length(ex)){
                eMSNFI[exi] <- sum(data.all$area[which(data.all$dbh<=ex[exi])])/totA
              }
              qMSNFI[[8]] <- data.table(ecdf=eMSNFI,x=ex)
            }
          }
        }
        source("~/finruns_to_update/correction_function.R")
        ii <- 1
        for(ii in 1:nSegs){
          dataS[ii,"ba"] <- min(max(data.all$ba)*1.1,correction_f(dataS$ba[ii],1))
          dataS[ii,"decid"] <- correction_f(dataS$decid[ii],2)
          dataS[ii,"pine"] <- correction_f(dataS$pine[ii],3)
          dataS[ii,"spruce"] <- correction_f(dataS$spruce[ii],4)
          dataS[ii,"age"] <- correction_f(dataS$age[ii],6)
          dataS[ii,"h"] <- correction_f(dataS$h[ii]/10,7)*10
          dataS[ii,"dbh"] <- correction_f(dataS$dbh[ii],8)  
        }
        print("done.")
        if(NFIlocal & TEST){
          print("start NFI age hist based sampling...")
          dataS2 <- dataS
          for(ii in 1:nSegs){
            ntmp <- which(ageS[ii]==dataS$age)
            if(length(ntmp)<1) ntmp <- which.min((ageS[ii]-dataS$age)^2)[1]
            if(length(ntmp)>1) ntmp <- sample(ntmp,1)
            dataS2[ii,] <- dataS[ntmp,]
          }
          dataS <- dataS2 
          rm(list=c("dataS2","ntmp"))
          gc()
          print("done.")
        }
        dataS[which(dataS$age==0),c("ba","decid","pine","spruce","age","h","dbh")]<-0
        dataS[which(dataS$h==0),c("ba","decid","pine","spruce","age","h","dbh")]<-0
        if(FIGS){
          par(mfrow=c(3,2))
          ni <- sample(1:nrow(data.all),1000,replace = F)
          ni2 <- sample(1:nrow(dataS),1000,replace = F)
          #ni2 <- c(ni2,18089)
          plot(data.all$ba[ni],data.all$age[ni],pch=19,cex=0.2,
               ylim=c(0,max(c(data.all$age[ni],dataS$age))),
               xlim=c(0,max(c(data.all$ba[ni],dataS$ba))),
               main=paste0(regionNames_fi[r_no],": black MSNFI, red MSNFIcorr"))
          points(dataS$ba[ni2],dataS$age[ni2],col="red",pch=19,cex=0.2)
          plot(data.all$ba[ni],data.all$pine[ni],pch=19,cex=0.2,
               ylim=c(0,max(c(data.all$pine[ni],dataS$pine))),
               xlim=c(0,max(c(data.all$ba[ni],dataS$ba))))
          points(dataS$ba[ni2],dataS$pine[ni2],col="red",pch=19,cex=0.2)
          plot(data.all$ba[ni],data.all$spruce[ni],pch=,cex=0.219,
               ylim=c(0,max(c(data.all$spruce[ni],dataS$spruce))),
               xlim=c(0,max(c(data.all$ba[ni],dataS$ba))))
          points(dataS$ba[ni2],dataS$spruce[ni2],col="red",pch=19,cex=0.2)
          plot(data.all$ba[ni],data.all$decid[ni],pch=19,cex=0.2,
               ylim=c(0,max(c(data.all$decid[ni],dataS$decid))),
               xlim=c(0,max(c(data.all$ba[ni],dataS$ba))))
          points(dataS$ba[ni2],dataS$decid[ni2],col="red",pch=19,cex=0.2)
          plot(data.all$ba[ni],data.all$dbh[ni],pch=19,cex=0.2,
               ylim=c(0,max(c(data.all$dbh[ni],dataS$dbh))),
               xlim=c(0,max(c(data.all$ba[ni],dataS$ba))),
               main=paste0(regionNames_fi[r_no],": black MSNFI, red MSNFIcorr"))
          points(dataS$ba[ni2],dataS$dbh[ni2],col="red",pch=19,cex=0.2)
          plot(data.all$h[ni],data.all$dbh[ni],pch=19,cex=0.2,
               ylim=c(0,max(c(data.all$dbh[ni],dataS$dbh))),
               xlim=c(0,max(c(data.all$h[ni],dataS$h))))
          points(dataS$h[ni2],dataS$dbh[ni2],col="red",pch=19,cex=0.2)
        }
      }
      #rm("data.all")
      gc()
    }
    
    # new fert/sitetypes
    if(fertUpdt){
    ###load the fittet probit models to estimate the Site fertility class
      load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/step.probit.rdata"))
      dataSample <- data.table(st=dataS$fert,
                               H=dataS$h,
                               D=dataS$dbh,
                               BAtot=dataS$ba,
                               BApPer=dataS$pine)
      modX <- step.probit[["all"]]
      ###run model -> returns the probability for each site type so you can sample using that probability
      ###model inputs:
      # st = site type
      # H = average height
      # D = average dbh
      #BAtot = total basal area
      #BApPer = % of pine basal area
      probs <- predict(modX,type='p',dataSample)
      str <- matrix(0,nSegs,1)
      for(ri in 1:nSegs){
        str[ri] <- sample(1:5,1,prob = probs[ri,])
      }
      a <- hist(dataS$fert,1:11-.5,plot=F)
      b <- hist(str,a$breaks,plot=F)
      barplot(rbind(a$counts,b$counts),beside=T, names.arg=a$mids,
              main=paste("region",r_no,"black=original, gray=updated"),
              xlab="fertility")
      dataS[,fert:=str]
      dataS$fert[which(dataS$landclass>1)] <- 5
    }
    
    print(paste("NAs in init state?", any(is.na(dataS))))
    print(paste("fmi_from_allas =",fmi_from_allas))
    # fmi data from allas
    if(fmi_from_allas & save_fmi_data){
      toMemFmi <- ls()
      source("~/finruns_to_update/0.5_get_fmi_from_allas.R")
      repo <- "ForModLabUHel/fmi.weather.finland"
      file_path <- "r/init_setup.R"
      branch <- "main"
      # Get init functions from github
      init_funs <- fetch_file_from_github(repo, file_path, branch)
      eval(parse(text = init_funs))
      rm(init_funs, file_path, repo)
      # SET PARAMETERS
      if(!exists("resolution")) resolution <- 1 # Resolution in km (1, 5 or 9)
      years <- c(2015:2024) # For which years to extract (1961:2023 are full years)
      save_path <- paste0(getwd()) # Where to save the extracted data.table as .rdata
      repo_url <- "https://github.com/ForModLabUHel/fmi.weather.finland.git" # Project repository to use
      format_to_prebas <- T # TRUE for Prebas format, FALSE for raw data. Default is TRUE.
      
      req_coords_dt <- data.table(
        id = 1:nrow(dataS),
        E = dataS$x,
        N = dataS$y
      )
      req_coords <- as.matrix(req_coords_dt[, c("E", "N")]) # The coords are passed as a matrix
      
      # Set parameters
      params <- list(req_coords = req_coords, resolution = resolution, years = years)
      
      # Combine arguments
      setup_and_run_args <- c(params, list(save_path = save_path, repo_url = repo_url, format_to_prebas = format_to_prebas))
      
      # RUN
      result <- do.call(setup_and_run, setup_and_run_args)
      
      # clear memory
      rm(list = setdiff(ls(), toMemFmi))
      gc()
      
      # Change file name
      workdir <- getwd()
      #rcps <- "CurrClim_fmi"
      fmi_vars_PREBAS_file <-  paste0("fmi_vars_PREBAS_dataS_",r_noi,".rdata")
      climID_lookup_file <- paste0("climID_lookup_dataS_",r_noi,".rdata")
      
      file.rename(list.files(path=workdir, pattern="fmi_vars_", all.files=FALSE,full.names=FALSE)[1],
                  fmi_vars_PREBAS_file)
      file.rename(list.files(path=workdir, pattern="climID_lookup_", all.files=FALSE,full.names=FALSE)[1],
                  climID_lookup_file)
    } else if(fmi_from_allas & !save_fmi_data){
      workdir <- getwd()
      #rcps <- "CurrClim_fmi"
      fmi_vars_PREBAS_file <-  paste0("fmi_vars_PREBAS_dataS_",r_noi,".rdata")
      climID_lookup_file <- paste0("climID_lookup_dataS_",r_noi,".rdata")
    }
    
    if(!exists("soilInfo_new")) soilInfo_new <- T
    MANUALRUN <- T
    if(MANUALRUN){
      easyInit=FALSE; forceSaveInitSoil=F; cons10run = F; coeffPeat1=-240; coeffPeat2=70; coefCH4 = 0.34; coefN20_1 = 0.23; coefN20_2 = 0.077; climScen = 0; clcut=1;  funPreb = regionPrebas; ingrowth = F; initSoilCreStart=NULL; outModReStart=NULL; reStartYear=1; climdata=NULL; sampleX=dataS; disturbanceON=NA; TminTmax=NA
      deltaID <- 1; outType <- "TestRun"; harvScen="Base"; harvInten="Base"; climScen=0  
      procDrPeat=T; forceSaveInitSoil=F; sampleX = dataS  
    }
    P0currclim=NA 
    fT0=NA
    scale_cc_area <- 1
    startingYear <- 2015
    endingYear <- 2050
    nYears <- endingYear-startingYear
    source("~/finruns_to_update/functions.R")
    ageHarvPriorX <- ageHarvPriorX1 # 120
    HcFactor<-HcFactor1
    print(paste("Fraction of >140 year forests:",1-mean(ageclassstats[,8])))
    if(mean(ageclassstats[,8])<0.975){ # if more old forests...     
      HcFactor <- HcFactor2
      ageHarvPriorX <- ageHarvPriorX2 # 160
    }
    if(!exists("climFIG")) climFIG <-F
    if(climFIG | save_fmi_data | !fmi_from_allas){
      #if(ECMmod==1 & nSegs==10000) load(file=paste0("/scratch/project_2000994/PREBASruns/PREBAStesting/RegionRuns/InitVals/Ninfo_station",r_no,".rdata"))
      print("Run runModel")
      out <- runModel(1,sampleID=1, outType = "testRun", rcps = "CurrClim", climScen = 0,#RCP=0,
                      harvScen="Base", harvInten="Base", procDrPeat=T, 
                      ECMmod = 0, initilizeSoil = T,
                      P0currclim = P0currclim,
                      fT0 = fT0,
                      soilGridData = soilGridData,
                      soilInfo_new = soilInfo_new,
                      thinFactX= thinFactX, landClassUnman = landClassUnman,
                      compHarvX = compHarvX, ageHarvPriorX = ageHarvPriorX,
                      forceSaveInitSoil=F, sampleX = dataS, HcMod_Init = HcMod_Init)
      if(ECMmod==1){
        nlc1 <- which(dataS$landclass==1)
        if(ba_scaling){
          cS <- scalesample*(Vstats[1]/(sum(apply(out$region$multiOut[nlc1,1,"V",,1],1,sum)*dataS$area[nlc1])/sum(dataS$area[nlc1])))^(1/3)
          print(paste("Scaling coefficient",cS))
          cS <- cS*(1+(dataS$lat-61)*.01)
          print(range(cS))
          dataS[,ba:=ba*cS^2]
          dataS[,dbh:=dbh*cS]
          dataS[,h:=h*cS]
          if(samplaus!=1) dataS[,age:=age*cS]
          ages <- apply(out$region$multiOut[,1,"age",,1],1,mean)
          scale_cc_area <- 1.5*(clcutAr[1]*sum(dataS$area)/totArea)/sum((dataS$area)[which(ages<1)])
          print(paste("Scale cc area",scale_cc_area))
        }
        print("estimate P0CurrClim")
        P0currclim <- (out$region$P0y[,nYears,1])
        fT0 <- (fTfun(out$region$weatherYasso[,nYears,1],
                              out$region$weatherYasso[,nYears,2],
                              out$region$weatherYasso[,nYears,3]))
        print("Run with ECMmod.")
        out <- runModel(1,sampleID=1, outType = "testRun", rcps = "CurrClim", climScen = 0,#RCP=0,
                        harvScen="Base", harvInten="Base", procDrPeat=T, 
                        ECMmod = ECMmod,
                        initilizeSoil=F,
                        P0currclim = P0currclim,
                        fT0 = fT0,
                        soilGridData = soilGridData,
                        soilInfo_new = soilInfo_new,
                        thinFactX= thinFactX, landClassUnman = landClassUnman,
                        compHarvX = compHarvX,ageHarvPriorX = ageHarvPriorX,
                        forceSaveInitSoil=F, sampleX = dataS, HcMod_Init = HcMod_Init)
      }
      NEP_yasso1 <- apply(out$region$multiOut[,,"NEP/SMI[layer_1]",,1],1:2,sum)
      # COmpare climate variables with new FMI weather data
      clim1 <- out$clim
      timei1 <- (1:dim(out$region$multiOut)[2])+2015
      #plot(timei, NEP_yasso,ylim=c(0,250),type="l",main="Currclim",ylab="NEPmin")
      out_currclim <- out
    }    
    print(paste("Sample area:",sum(dataS$area)))
    if(HarvScen!="Base" | fmi_from_allas){
      workdir <- paste0(getwd(),"/")  
      startingYear <- 2015
      endingYear <- 2024
      nYears <- endingYear-startingYear
      out <- runModel(1,sampleID=1, outType = "testRun", rcps = rcps, climScen = 0,#RCP=0,
                      harvScen=harvScen, harvInten=HarvInten, procDrPeat=T, 
                      thinFactX= thinFactX, landClassUnman = landClassUnman,
                      ECMmod = ECMmod,
                      P0currclim = P0currclim,
                      fT0 = fT0,
                      soilGridData = soilGridData,
                      compHarvX = compHarvX,ageHarvPriorX = ageHarvPriorX,
                      forceSaveInitSoil=F, sampleX = dataS, 
                      HcMod_Init = HcMod_Init)
      out_currclim_fmi <- out
      clim2 <-out$clim
      
      #NansRun <- any(is.na(out$region$multiOut[,,"NEP/SMI[layer_1]",,1]))
      #print(paste("Any NaNs in NEP?", NansRun))
      NEP_yasso <- apply(out$region$multiOut[,,"NEP/SMI[layer_1]",,1],1:2,sum)
      #if(NansRun) NEP_yasso <- NEP_yasso[which(!is.na(rowSums(NEP_yasso))),]     

      timei2 <- (1:dim(out$region$multiOut)[2])+2015
      if(climFIG | (TRUE & (save_fmi_data | !fmi_from_allas))){
        par(mfrow=c(3,2))
        plotID <- 101
        id_currclim_1 <- which(clim1$id==dataS$CurrClimID[plotID])
        id_currclimfmi_1 <- which(clim2$id==out$region$siteInfo[plotID,"climID"])
        for(ij in 1:(length(clim1)-1)){
          ylims  <- c(min(min(clim1[[ij]]),min(clim2[[ij]])),
                      max(max(clim1[[ij]]),max(clim2[[ij]])))
          if(ij%in%c(1,3,4)) ylims[1] <-  0
          plot(clim2[[ij]][id_currclimfmi_1,1:(9*365)],ylab=names(clim1)[ij],
               ylim =ylims, col="red",main="black: CurrClim, red: Currclimfmi",pch=19,cex=.2)
          points(clim1[[ij]][id_currclim_1,1:(9*365)],pch=19,col="black",cex=0.2)
        }
        ylims  <- c(0,1.5*max(c(colMeans(NEP_yasso1),colMeans(NEP_yasso))))
        plot(timei1, NEP_yasso1[plotID,],type="l",ylim=ylims,main="black: Currclim, red: Currclimfmi",ylab="NEPmin")
        lines(timei1,colMeans(NEP_yasso1),col="blue",lwd=4)      
        lines(timei2,colMeans(NEP_yasso),col="red",lwd=4)      
        lines(timei1,NEP_yasso1[plotID,],col="blue")      
        lines(timei2,NEP_yasso[plotID,],col="red")
        for(ij in 1:(length(clim1)-1)){
          ylims  <- c(min(min(clim1[[ij]][id_currclim_1,1:(365)]),
                          min(clim2[[ij]][id_currclimfmi_1,1:(1*365)])),
                      max(max(clim1[[ij]][id_currclim_1,1:(1*365)]),
                          max(clim2[[ij]][id_currclimfmi_1,1:(1*365)])))
          if(ij%in%c(1,3,4)) ylims[1] <-  0
          plot(clim2[[ij]][id_currclimfmi_1,1:(1*365)],
               clim1[[ij]][id_currclim_1,1:(1*365)],
               ylab=paste("Currclim"),
               xlab=paste("Currclim_fmi"),
               ylim =ylims, xlim = ylims,
               main=names(clim1)[ij],pch=19)
        }
        analyzeClim <- function(climx, ir = 1, vname){
            outx <- array(0,dim = c(12,4),
                          dimnames = list(c(1:12),paste0(vname,c("_overzerodays","_sum","_meanofall","_meanofoverzerodays"))))
          dds <- c(0,as.vector(cumsum(days_in_month(1:12))))
          if(is.null(dim(climx))) climx <- array(climx,c(1,length(climx)))
          for(ik in 1:12){
            ddss <- (dds[ik]+1):dds[ik+1]
            outx[ik,1] <- sum(as.numeric(climx[ir,ddss]>0))          
            outx[ik,2] <- sum(climx[ir,ddss])    
            outx[ik,3] <- mean(climx[ir,ddss])    
            outx[ik,4] <- mean((climx[ir,ddss])[(climx[ir,ddss]>0)])
          }
          return(outx)
        }
        climStats <- list()
        for(ij in 1:(length(clim1)-2)){
          outxx <- array(0,dim = c(12,4,4),
                        dimnames = list(c(1:12),paste0(names(clim1)[ij],c("_overzerodays","_sum","_meanofall","_meanofoverzerodays")),
                                        c("currclim_i","currclimfmi_i","currclim_all","currclimfmi_all")))
          outxx[,,1] <- analyzeClim(clim1[[ij]], ir=id_currclim_1, names(clim1)[ij])  
          outxx[,,2] <- analyzeClim(clim2[[ij]], ir=id_currclimfmi_1, names(clim2)[ij])  
          outxx[,,3] <- analyzeClim(colMeans(clim1[[ij]]), ir=1, names(clim1)[ij])  
          outxx[,,4] <- analyzeClim(colMeans(clim2[[ij]]), ir=1, names(clim2)[ij])  
          climStats[[ij]] <- outxx
          names(climStats)[ij] <- names(clim1)[ij]
          par(mfrow=c(2,2))
          ik <- 1
          for(ik in 1:4){
            if(ik==1){ 
              barplot(t(climStats[[ij]][,ik,]),
                      beside=T,
                      legend=dimnames(climStats[[ij]])[[3]],
                      main=dimnames(climStats[[ij]])[[2]][ik])
            } else {
              barplot(t(climStats[[ij]][,ik,]),
                      beside=T,
                      main=dimnames(climStats[[ij]])[[2]][ik])
              
            }
          }
        }
      }
      if(exists("NEP_yasso1")) rm("NEP_yasso1") 
      NEP_yasso <- colMeans(NEP_yasso)
    }
    #lapply(sampleIDs, 
    #       function(jx) { 
    #         runModelAdapt(1,sampleID=jx, outType = outType, rcps = "CurrClim_fmi",
    #                       harvScen="Base", harvInten="Base",
    #                       forceSaveInitSoil=T)
    # out <- runModelAdapt(1,sampleID=1, outType = "testRun", rcps = rcps,
    #                       harvScen="NoHarv", harvInten="NoHarv", climScen=0, procDrPeat=T,
    #                       forceSaveInitSoil=F)
    gc()

    NEP_yasso <- out$region$multiOut[,,"NEP/SMI[layer_1]",,1]
    NansRun <- any(is.na(NEP_yasso))
    #print(paste("Any NaNs in NEP?", NansRun))
    niNa <- NA
    if(NansRun){
      niNa <- which(is.na(rowSums(apply(NEP_yasso,1:2,sum))))
    #  NEP_yasso <- NEP_yasso[-niNa,,]
    }   
    out$region <- peat_regression_model_multiSite(out$region,which(dataS$peatID==1)) 
    
    output <- out$region$multiOut
    areas <- dataS$area
    timei <- (1:dim(output)[2])+2015
    ti <- 1
    ikaluokat <- array(0,c(nYears,9))
    agelimits <- c(0,20,40,60,80,100,120,140,1e4)
    for(ti in 1:nYears){
      #    ages <- apply(output[,ti,"age",,1]*output[,ti,"BA",,1],1,sum)/apply(output[,ti,"BA",,1],1,sum)
      ages <- output[,ti,"age",1,1]
      
      ages[is.na(ages)] <- 0 
      ages <- ages[which(dataS$landclass==1)]
      areas1 <- areas[which(dataS$landclass==1)]
      for(ij in 1:length(agelimits)){
        if(ij == 1){ 
          ikaluokat[ti,ij] <- sum(areas1[which(ages==agelimits[1])])
        } else if(ij==length(agelimits)){
          ikaluokat[ti,ij] <- sum(areas1[which(ages>(agelimits[ij-1]+1))])
        } else {
          ikaluokat[ti,ij] <- sum(areas1[which(ages>(agelimits[ij-1]+1) & ages<=agelimits[ij])])  
        }
      }
      ikaluokat[ti,] <- round(ikaluokat[ti,]/sum(ikaluokat[ti,]),3)
      
    }
    par(mfrow=c(1,1))
    datagroups <- c("a: 0","b: 1-20","c: 21-40","d: 41-60","e: 61-80","f:81-100","g: 101-120","h: 121-140","i: 140-")
    data <- data.frame(time=rep(timei,each=ncol(ikaluokat)), 
                       shares = c(t(ikaluokat)), classes =rep(datagroups,length(timei)))
    a1 <- ggplot(data, aes(x=time, y=shares, fill=classes)) + 
      geom_area(alpha=0.6, linewidth=.5, colour="white") + 
      #scale_fill_viridis(discrete = T) +
      #ylim(0,600) + 
      theme_gray(base_size = 10) + 
      ggtitle(paste("Age class area share, Region",r_no, rname_fi))
    data2 <- data.frame(time=rep(c(2015,2021), each=ncol(ikaluokat)),
                        shares = c(1-t(ageclassstats)), 
                        classes = rep(datagroups, 2))
    a2 <- geom_point(data = data2, 
                     mapping = aes(x = time, y = shares, colour = classes))
    print(a1+a2)
    #totArea <- sum(data.all$area)
    #areaRegion <- totArea <- sum(data.all$area,na.rm=T)
    sortVar <- c("landclass","peatID","cons")
    outresults <- areatable <- data.table()
    sortid <- 1
    areatable <- cbind(areatable, areaTot = totArea)
    for(sortid in 1:nsorts){
      if(sortid==1){
        n_lc1 <- which(dataS$landclass==1)
        n_lc2 <- which(dataS$landclass==2)
        sortVarnams <- c("forest","poorly_productive")
        sortTotAreas <- c(sum(data.all$area[which(data.all$landclass==1)]),
                          sum(data.all$area[which(data.all$landclass==2)]))
        areatable <- cbind(areatable, data.table(area_forest=sortTotAreas[1],
                                                 area_poorly_productive=sortTotAreas[2]))
      } else if(sortid==2) {
        n_lc1 <- which(dataS$peatID==0)
        n_lc2 <- which(dataS$peatID==1)
        sortVarnams <- c("min","ditched_org")
        sortTotAreas <- c(sum(data.all$area[which(data.all$peatID==0)]),
                          sum(data.all$area[which(data.all$peatID==1)]))
        areatable <- cbind(areatable, data.table(area_min=sortTotAreas[1],
                                                 area_ditched_org=sortTotAreas[2]))
        #  n_lc3 <- which(dataS$peatID==2)
        #  sortVarnams <- c("min","ditched_org","natural_org")
        #  sortTotAreas <- c(sum(data.all$area[which(data.all$peatID==0)]),
        #                    sum(data.all$area[which(data.all$peatID==1)]),
        #                    sum(data.all$area[which(data.all$peatID==2)]))
      } else if(sortid==3) {
        n_lc1 <- which(dataS$cons==0)
        n_lc2 <- which(dataS$cons==1)
        sortVarnams <- c("managed","cons")
        sortTotAreas <- c(sum(data.all$area[which(data.all$cons==0)]),
                          sum(data.all$area[which(data.all$cons==1)]))
        areatable <- cbind(areatable, data.table(area_managed=sortTotAreas[1],
                                                 area_cons=sortTotAreas[2]))
      }    
      
      #areas1 <- areas[n_lc1]
      #areas2 <- areas[n_lc2]
      #if(sortid==3) areas3 <- areas[n_lc3]
      varis <- c("wf_STKG","NEP_yasso","V","age","Wtot","BA","grossGrowth","NEP/SMI[layer_1]",
                 "Wharvested","Vharvested","VroundWood","Venergywood",
                 "Vmort","CH4em","N2Oem")
      variNams <- c("wf_STKG","NEP_yasso","V","age","Wtot","BA","grossGrowth","NEP",
                    "Wharvested","Vharvested","VroundWood","Venergywood",
                    "Vmort","CH4em","N2Oem")
      #  outresults <- outresultsSum <- data.table()
      ij <- 1
      for(ij in 1:length(varis)){
        if(varis[ij]=="Wtot"){ 
          tmp <- apply(output[,,c(24,25,31,32,33),,1],c(1,2,4),sum)
        } else if(varis[ij]=="NEP_yasso"){
          tmp <- NEP_yasso
        } else if(varis[ij]=="Wharvested"){
          tmp <- output[,,"WroundWood",,1]+out$region$multiEnergyWood[,,,2]
        } else if(varis[ij]=="Vharvested"){
          tmp <- output[,,"VroundWood",,1]+out$region$multiEnergyWood[,,,1]
        }  else if(varis[ij]=="Venergywood"){
          tmp <- out$region$multiEnergyWood[,,,1]
        }  else if(varis[ij]=="CH4em"){
          tmp <- out$region$CH4emisDrPeat_kgyear
        }  else if(varis[ij]=="N2Oem"){
          tmp <- out$region$N2OemisDrPeat_kgyear
        } else {
          ijid <- which(varNames==varis[ij])
          if(length(ijid)==0){
            ijid <- which(str_detect(varNames,"grossGrowth"))[1]
            print(paste0("'",varis[ij],"' replaced with '",varNames[ijid],"'"))      
          }
          tmp <- output[,,ijid,,1]
        }
        if(varis[ij]%in%c("wf_STKG","NEP_yasso","V","Wtot","BA","grossGrowth","NEP/SMI[layer_1]","Wharvested","Vharvested","VroundWood","Venergywood","Vmort","CH4em","N2Oem")){ # sums
          if(varis[ij]%in%c("CH4em","N2Oem")){
            outres <- sum(tmp*areas)/sum(areas)
          } else {
            if(varis[ij]%in%c("NEP_yasso","NEP/SMI[layer_1]") & ! is.na(niNa)[1]){
              outres <- colSums(apply(tmp[-niNa,,],1:2,sum)*areas[-niNa])/sum(areas[-niNa])
            } else {
              outres <- colSums(apply(tmp,1:2,sum)*areas)/sum(areas)
            }
          }
          #assign(varis[ij],outres)
          if(sortid==1){
            outresults <- cbind(outresults, outres)
            colnames(outresults)[ncol(outresults)] <- variNams[ij]
            #outresultsSum <- cbind(outresultsSum, outres*totArea)
            #colnames(outresultsSum)[ncol(outresultsSum)] <- variNams[ij]
          }        
          ik <- 1
          for(ik in 1:length(sortVarnams)){
            if(ik==1) ni <- n_lc1
            if(ik==2) ni <- n_lc2
            if(ik==3) ni <- n_lc3
            if(varis[ij]%in%c("CH4em","N2Oem")){
              outres <- sum(tmp[ni]*areas[ni])/sum(areas[ni])
            } else {
              if(varis[ij]%in%c("NEP_yasso","NEP/SMI[layer_1]") & !is.na(niNa)[1]){
                ni <- setdiff(ni,niNa)
                outres <- colSums(apply(tmp[ni,,],1:2,sum)*areas[ni])/sum(areas[ni])
              } else {
                outres <- colSums(apply(tmp[ni,,],1:2,sum)*areas[ni])/sum(areas[ni])
              }
            }
            outresults <- cbind(outresults, outres)
            colnames(outresults)[ncol(outresults)] <- paste0(variNams[ij],"_",sortVarnams[ik])
            #outresultsSum <- cbind(outresultsSum, outres*sortTotAreas[ik])
            #colnames(outresultsSum)[ncol(outresultsSum)] <- paste0(variNams[ij],"_",sortVarnams[ik])
            #assign(paste0(varis[ij],"_lc",ik),outres)
            if(sortid==1 & ik==1 & varis[ij]=="grossGrowth"){
              stypes <- out$region$siteInfo[,"siteType"]
              for(stp in 1:5){
                ni <- which(stypes==stp)
                if(length(ni)>1){
                  gg0 <- colSums(apply(tmp[ni,,],c(1:2),sum)*areas[ni])/sum(areas[ni])
                } else { gg0 <- rowSums(apply(tmp[ni,,],c(1:2),sum))}
                assign(paste0("gg",stp), gg0)
                rm("gg0")
              }
            }
          }
        }
        if(varis[ij]%in%c("age")){ # mean
          outres <- colSums(apply(tmp,1:2,mean)*areas)/sum(areas)
          #assign(varis[ij],outres)
          if(sortid==1){
            outresults <- cbind(outresults, outres)
            colnames(outresults)[ncol(outresults)] <- variNams[ij]
            #outresultsSum <- cbind(outresultsSum, outres)
            #  colnames(outresultsSum)[ncol(outresultsSum)] <- variNams[ij]
          }
          ik <- 1
          for(ik in 1:length(sortVarnams)){
            if(ik==1) ni <- n_lc1
            if(ik==2) ni <- n_lc2
            if(ik==3) ni <- n_lc3
            outres <- colSums(apply(tmp[ni,,],1:2,mean)*areas[ni])/sum(areas[ni])
            outresults <- cbind(outresults, outres)
            colnames(outresults)[ncol(outresults)] <- paste0(variNams[ij],"_",sortVarnams[ik])
            # outresultsSum <- cbind(outresultsSum, outres)
            #colnames(outresultsSum)[ncol(outresultsSum)] <- paste0(variNams[ij],"_",sortVarnams[ik])
            #assign(paste0(varis[ij],"_lc",ik),outres)
          }
        }
      }
      
      ij <- which(colnames(outresults)=="NEP")
      if(sortid==1){
        outresults <-cbind(outresults, -outresults$NEP*ha_to_m2/g_to_kg)
        colnames(outresults)[ncol(outresults)] <- "NEE"
        outresults <-cbind(outresults, outresults$NEE*totArea)
        colnames(outresults)[ncol(outresults)] <- "NEEsum"
      }
      ik <- 1
      for(ik in 1:length(sortVarnams)){
        ikj <- which(colnames(outresults)==paste0("NEP_",sortVarnams[ik]))
        print(colnames(outresults)[ikj])
        outresults <- cbind(outresults, -outresults[,..ikj]*ha_to_m2/g_to_kg)
        colnames(outresults)[ncol(outresults)] <- paste0("NEE_",sortVarnams[ik])
        ikj <- which(colnames(outresults)==paste0("NEE_",sortVarnams[ik]))
        outresults <- cbind(outresults, outresults[,..ikj]*sortTotAreas[ik])
        colnames(outresults)[ncol(outresults)] <- paste0("NEEsum_",sortVarnams[ik])
      }
      
      if(sortid==1){
        outresults <-cbind(outresults, 44/12*(outresults$NEE+outresults$Wharvested)+
                             298*outresults$N2Oem + 25*outresults$CH4em)
        colnames(outresults)[ncol(outresults)] <- "NBE"
        outresults <-cbind(outresults, outresults$NBE*totArea)
        colnames(outresults)[ncol(outresults)] <- "NBEsum"
      }
      ik <- 1
      for(ik in 1:length(sortVarnams)){
        ik0 <- which(colnames(outresults)==paste0("NEE_",sortVarnams[ik]))
        ik1 <- which(colnames(outresults)==paste0("Wharvested_",sortVarnams[ik]))
        ik2 <- which(colnames(outresults)==paste0("CH4em_",sortVarnams[ik]))
        ik3 <- which(colnames(outresults)==paste0("N2Oem_",sortVarnams[ik]))
        outresults <- cbind(outresults,44/12*(outresults[,..ik0]+outresults[,..ik1])+
                              298*outresults[,..ik2] + 25*outresults[,..ik3])
        colnames(outresults)[ncol(outresults)] <- paste0("NBE_",sortVarnams[ik])
        ikj <- which(colnames(outresults)==paste0("NBE_",sortVarnams[ik]))
        outresults <- cbind(outresults, outresults[,..ikj]*sortTotAreas[ik])
        colnames(outresults)[ncol(outresults)] <- paste0("NBEsum_",sortVarnams[ik])
      }
      #NBE <- 44/12*(NEE + Wharvested) + 298*N2Oem + 25*CH4em
      
      KUVA <- T
      if(KUVA){
        timei <- 2015+1:nrow(outresults)
        par(mfrow=c(3,2))
        ij <- which(colnames(outresults)=="grossGrowth")
        tmp <- unlist(outresults[,..ij])
        plot(timei, tmp, type="l",main=paste("Region",r_no,rname), 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylim = c(0,10), ylab = "grossgrowth, m3/ha",lwd=3)
        points(c(2015,2021),ggstats,pch=19,col="red")
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- ij + ik
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        lines(timei,gg1,col="gray",lwd=0.5)
        lines(timei,gg2,col="yellow",lwd=0.5)
        lines(timei,gg3,col="orange",lwd=0.5)
        lines(timei,gg4,col="brown",lwd=0.5)
        lines(timei,gg5,col="magenta",lwd=0.5)
        legend("bottomright",c(paste0("all ",round(totArea/1000),"kha"),
                               paste0(sortVarnams," ", round(sortTotAreas/1000),"kha")),
               pch=rep(1,length(sortVarnams)+1),cex=0.7,
               bty = "n",
               col=c("black",colorsi[1:length(sortVarnams)]))
        
        # NEP
        ij <- which(colnames(outresults)=="NEP_yasso")
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("NEP_yasso_",sortVarnams),colnames(outresults)))
        ij3 <- c(ij2,match(c("NEP",paste0("NEP_",sortVarnams)),colnames(outresults)))
        ymax <- max(0,max(outresults[,..ij3],na.rm = T))
        ymin <- min(0,min(outresults[,..ij3],na.rm = T))
        plot(timei, tmp, type="l",main=paste("Region",r_no,rname), 
             xlim = c(timei[1]-1,timei[length(timei)]),
             xlab = "time, dotted=NEPyasso, solid=NEP (incl.ditched peatlands)",
             ylab = "NEP, g/m2", ylim = c(ymin,ymax),
             lwd=3, lty=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- which(paste0("NEP_yasso_",sortVarnams[ik])==colnames(outresults))
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik],lty=3)
          }
        }
        
        ij <- which(colnames(outresults)=="NEP")
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("NEP_",sortVarnams),colnames(outresults)))
        ymax <- max(0,max(outresults[,..ij2]))
        ymin <- min(0,min(outresults[,..ij2]))
        lines(timei, tmp, lwd=3, col="black")
        #plot(timei, tmp, type="l",main=paste("Region",r_no,rname), 
        #     xlim = c(timei[1]-1,timei[length(timei)]),
        #     ylab = "NEP, g/m2", ylim = c(ymin,ymax),
        #     lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- which(paste0("NEP_",sortVarnams[ik])==colnames(outresults))
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        lines(timei,0*timei, col="black")
        
        # V
        ij <- which(colnames(outresults)=="V")
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("V_",sortVarnams),colnames(outresults)))
        ymax <- max(Vstats,max(outresults[,..ij2]))
        ymin <- min(outresults[,..ij2])
        plot(timei, tmp, type="l",main=paste("Region",r_no,rname), 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylab = "V, m3/ha", ylim = c(0,ymax),
             lwd=3)
        points(c(2015,2021),Vstats,pch=19,col="red")
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- which(paste0("V_",sortVarnams[ik])==colnames(outresults))
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        
        # wf_STKG
        ij <- which(colnames(outresults)=="wf_STKG")
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("wf_STKG_",sortVarnams),colnames(outresults)))
        ymax <- max(outresults[,..ij2])
        ymin <- min(outresults[,..ij2])
        plot(timei, tmp, type="l",main=paste("Region",r_no,rname), 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylab = "Foliage biomass, kgC/ha", ylim = c(0,ymax),
             lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- which(paste0("wf_STKG_",sortVarnams[ik])==colnames(outresults))
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        
        # age
        ij <- which(colnames(outresults)=="age")[1]
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("age_",sortVarnams),colnames(outresults)))
        ymax <- max(outresults[,..ij2])
        ymin <- min(outresults[,..ij2])
        plot(timei, tmp, type="l",main=paste("Region",r_no,rname), 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylab = "age, years", ylim = c(0,ymax),
             lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- which(paste0("age_",sortVarnams[ik])==colnames(outresults))
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        
        # BA
        ij <- which(colnames(outresults)=="BA")
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("BA_",sortVarnams),colnames(outresults)))
        ymax <- max(outresults[,..ij2])
        ymin <- min(outresults[,..ij2])
        plot(timei, tmp, type="l",main=paste("Region",r_no,rname), 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylab = "BA m2/ha", ylim = c(0,ymax),
             lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- ij2[ik+1]
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        
        # soil depth
        xi <- sort(unique(out$region$siteInfo[,"soildepth"]))
        ah <- array(0,c(length(sortVarnams),length(xi)))
        ii <- 1
        ik <- 1
        for(ik in 1:length(sortVarnams)){
          if(ik==1) ni <- n_lc1
          if(ik==2) ni <- n_lc2
          if(ik==3) ni <- n_lc3
          Ah <- sum(dataS$area[ni])
          for(ii in 1:length(xi)){
            ah[ik,ii] <- sum(dataS$area[ni[which(out$region$siteInfo[ni,"soildepth"]==xi[ii])]])
          }
          ah[ik,] <- round(ah[ik,]/Ah,4)*100
        } 
        barplot(ah, names.arg = xi, beside=T, col=c("blue","green"),
                main="soil depth in sample",ylab="% of area", xlab="soil_depth",
                legend.text=c(sortVarnams))
        
        if(TRUE){
          # fertility histograms
          xi <- 1:10
          ah <- array(0,c(length(sortVarnams),length(xi)))
          ii <- 1
          ik <- 1
          for(ik in 1:length(sortVarnams)){
            if(ik==1) ni <- n_lc1
            if(ik==2) ni <- n_lc2
            if(ik==3) ni <- n_lc3
            Ah <- sum(dataS$area[ni])
            for(ii in 1:length(xi)){
              ah[ik,ii] <- sum(dataS$area[ni[which(dataS$fert[ni]==ii)]])
            }
            ah[ik,] <- round(ah[ik,]/Ah,4)*100
          } 
          barplot(ah, names.arg = xi, beside=T, col=c("blue","green"),
                  main="fertility in sample",ylab="% of area", xlab="fert",
                  legend.text=c(sortVarnams))
        }
        if(FALSE){
          # Wtot
          ij <- which(colnames(outresults)=="Wtot")
          tmp <- unlist(outresults[,..ij])
          ij2 <- c(ij,match(paste0("Wtot_",sortVarnams),colnames(outresults)))
          ymax <- max(outresults[,..ij2])
          ymin <- min(outresults[,..ij2])
          plot(timei, tmp, type="l",main=paste("Region",r_no,rname), 
               xlim = c(timei[1]-1,timei[length(timei)]),
               ylab = "Wtot, kg C/ha", ylim = c(0,ymax),
               lwd=3)
          colorsi <- c("blue","green","pink")
          for(ik in 1:length(sortVarnams)){
            ijk <- ij2[ik+1]
            tmp <- unlist(outresults[,..ijk])
            if(length(tmp)>1){
              lines(timei, tmp,col=colorsi[ik])
            }
          }
        }
        # Vharvested
        ij <- which(colnames(outresults)=="Vharvested")
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("Vharvested_",sortVarnams),colnames(outresults)))
        ymax <- max(outresults[,..ij2])
        ymin <- min(outresults[,..ij2])
        plot(timei, tmp, type="l",main=paste("Region",r_no,rname), 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylab = "Vharv, m3/ha", ylim = c(0,ymax),
             lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- ij2[1 + ik]
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        points(timei[1:nYears],rowSums(HarvLimMaak[1:nYears,])/totArea*1000,col="red")
        legend("bottomright",c(paste0("all ",round(totArea/1000),"kha"),
                               paste0(sortVarnams," ", round(sortTotAreas/1000),"kha")),
               pch=rep(1,length(sortVarnams)+1),cex=0.7,
               bty = "n",
               col=c("black",colorsi[1:length(sortVarnams)]))
        
        
        CO2eq_C <- 44/12 
        if(FALSE){
        # Wharvested
        ij <- which(colnames(outresults)=="Wharvested")
        tmp <- unlist(outresults[,..ij])*CO2eq_C
        ij2 <- c(ij,match(paste0("Wharvested_",sortVarnams),colnames(outresults)))
        ymax <- max(outresults[,..ij2])*CO2eq_C
        ymin <- min(outresults[,..ij2])*CO2eq_C
        plot(timei, tmp, type="l",main=paste("Region",r_no,rname), 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylab = "Wharv, kg CO2eq/ha", ylim = c(0,ymax),
             lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- ij2[1 + ik]
          tmp <- unlist(outresults[,..ijk])*CO2eq_C
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        }
        # NEE
        ij <- which(colnames(outresults)=="NEE")
        tmp <- unlist(outresults[,..ij])*CO2eq_C
        ij2 <- c(ij,match(paste0("NEE_",sortVarnams),colnames(outresults)))
        ymax <- max(0,max(outresults[,..ij2])*CO2eq_C)
        ymin <- min(0,min(outresults[,..ij2])*CO2eq_C)
        plot(timei, tmp, type="l",main=paste("Region",r_no,rname), 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylab = "NEE, kg CO2eq/ha", ylim = c(ymin,ymax),
             lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- ij2[1 + ik]
          tmp <- unlist(outresults[,..ijk])*CO2eq_C
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        lines(timei,0*timei, col="black")
        
        
        if(TRUE){
          # NBE
          ij <- which(colnames(outresults)=="NBE")
          tmp <- unlist(outresults[,..ij])
          ij2 <- c(ij,match(paste0("NBE_",sortVarnams),colnames(outresults)))
          if(!is.na(netsinksreg[1])){ 
            netsinksreg_per_ha <-netsinksreg*1e9/totArea
            ymax <- 1.05*max(0,max(max(netsinksreg_per_ha,na.rm = T),max(outresults[,..ij2])))
            ymin <- 1.05*min(0,min(min(netsinksreg_per_ha,na.rm = T),min(outresults[,..ij2])))
          } else {
            ymax <- max(0,max(outresults[,..ij2]))
            ymin <- min(0,min(outresults[,..ij2]))
            ymax <- max(ymax, 1000*1.05*c(max(netsinksreg_per_ha2025),max(netsinksreg_per_ha2025_min),max(netsinksreg_per_ha2025_org)))
            ymin <- min(ymin, 1000*1.05*c(min(netsinksreg_per_ha2025),min(netsinksreg_per_ha2025_min),min(netsinksreg_per_ha2025_org)))
            
          }
          plot(timei, tmp, type="l",main=paste("Region",r_no,rname), 
               xlim = c(timei[1]-1,timei[length(timei)]),
               ylab="NBE, kg CO2eq/ha", ylim = c(ymin,ymax),
               lwd=3)
          points(2015:2021, netsinksreg_per_ha, pch=19,col="red")
          points(2015:2023, netsinksreg_per_ha2025*1000, pch=19,col="purple")
          if(sortid==2){
            points(2015:2023, netsinksreg_per_ha2025_min*1000, pch=1, col="blue")
            points(2015:2023, netsinksreg_per_ha2025_org*1000, pch=1, col="green")
          }
          
          colorsi <- c("blue","green","pink")
          for(ik in 1:length(sortVarnams)){
            ijk <- ij2[1 + ik]
            tmp <- unlist(outresults[,..ijk])
            if(length(tmp)>1){
              lines(timei, tmp,col=colorsi[ik])
            }
          }
          lines(c(timei[1],timei[length(timei)]),c(0,0),col="black")
          
          # NBEsum
          ij <- which(colnames(outresults)=="NBEsum")
          tmp <- unlist(outresults[,..ij])
          ij2 <- c(ij,match(paste0("NBEsum_",sortVarnams),colnames(outresults)))
          if(!is.na(netsinksreg[1])){          
            ymax <- 1.05*max(0,max(max(netsinksreg*1e9,na.rm = T),max(outresults[,..ij2])))
            ymin <- 1.05*min(0,min(min(netsinksreg*1e9,na.rm = T),min(outresults[,..ij2])))
          } else {
            ymax <- max(0,max(outresults[,..ij2]))
            ymin <- min(0,min(outresults[,..ij2]))
          }
          ymax <- max(ymax, 1e9*1.05*c(max(netsinksreg2025),max(netsinksreg2025_min),max(netsinksreg2025_org)))
          ymin <- min(ymin, 1e9*1.05*c(min(netsinksreg2025),min(netsinksreg2025_min),min(netsinksreg2025_org)))
          plot(timei, tmp/1e6, type="l",main=paste("Region",r_no,rname), 
               xlim = c(timei[1]-1,timei[length(timei)]),
               ylab="NBEsum, million kg CO2eq", ylim = c(ymin,ymax)/1e6,
               lwd=3)
          points(2015:2021,netsinksreg*1e9/1e6,pch=19,col="red")
          points(2015:2023,netsinksreg2025*1e9/1e6,pch=19,col="purple")
          if(sortid==2){
            points(2015:2023, netsinksreg2025_min*1000, pch=1, col="blue")
            points(2015:2023, netsinksreg2025_org*1000, pch=1, col="green")
          }
          colorsi <- c("blue","green","pink")
          for(ik in 1:length(sortVarnams)){
            ijk <- ij2[1 + ik]
            tmp <- unlist(outresults[,..ijk])
            if(length(tmp)>1){
              lines(timei, tmp/1e6,col=colorsi[ik])
            }
          }
          lines(c(timei[1],timei[length(timei)]),c(0,0),col="black")
          
        }
      }
    }
    
    #
    #if(toFile) 
      save(outresults, areatable, 
                    file = paste0(outDir,fname,"_rno",r_noi,".rdata"))  
    if(fmi_from_allas & delete_fmi_data){
      file.remove(paste0(workdir,fmi_vars_PREBAS_file))
      file.remove(paste0(workdir,climID_lookup_file))
    }
    rm(list=setdiff(ls(), toMem))
    gc()
    
  }
} 


regionIDs <- as.numeric(c("1","21","16","06","09","13","11","19","05",
                          "15","02","14","07","04","08","18","10","12","17"))
regs <- c("Uu","Ah","KP","Pi","EK","KS","PS","La","KH","Po","VS",
          "EP","PH","Sa","Ky","Ka","ES","PK","PP")
regs <- paste(regionIDs,regionNames_fi)
regs[match(c(1:2,4:19),r_nos)]
r_nois <- r_nois0 <- match(c(1:2,4:19),r_nos)

r_nois[-1] <- r_nois0[-1]-1

load(paste0(outDir,fname,"_rno",r_noi,".rdata"))
#load(paste0(outDir,"results_agesample",samplaus,NFIlocal,"_compHarv",compHarvX,"ageHarvPrior",ageHarvPriorX,"_rno",1,"_",rcps,".rdata"))  
timei <- 2015+1:nrow(outresults)
outresults_all <- array(0,dim=c(dim(outresults)[1],dim(outresults)[2],length(rids)),
                        dimnames = list(c(2015+1:dim(outresults)[1]),
                                        colnames(outresults),
                                        regionNames[rids]))
validation_all <- array(NA,dim=c(2,2*2+9*2,length(rids)),
                        dimnames = list(c("NFI","sim"),
                                        c("gg2015","gg2021","V2015","V2021",
                                          paste0("NBEtot",2015:2023),
                                          paste0("NBEave",2015:2023)),
                                        regionNames[rids]))
r_noi_id <- 3
for(r_noi_id in 1:length(r_nois0)){#1:length(rids)){
  toMem <- ls()
  set.seed(1)
  r_noi <- r_nois0[r_noi_id]
  r_noi_file <- r_nois[r_noi_id]
  #r_no <- rids[r_noi]
  r_no <- r_noi#s0[r_noi]
  rname <- regionNames[r_no]
  rname_fi <- regionNames_fi[r_no]
  print(rname)
  rnameid <- r_nos[r_no]
  mortMod <- 13
  landClassX <- 1:2
  if(!exists("compHarvX")) compHarvX <- 0
  if(!exists("thinFactX")) thinFactX <- 0.25
  #  print(paste("CompHarv =", compHarvX))
  noPrebasLoading <- T
  source("~/finruns_to_update/settings.R")
  
  areasLandClass2015 <- as.numeric(landclass2015[which(landclass2015[,1]==rname_fi),2:3])
  areasLandClass2021 <- as.numeric(landclass2021[which(landclass2021[,1]==rname_fi),2:3])
  Vstats <- as.numeric(c(V2015[which(V2015[,1]==rname_fi),ncol(V2015)],
                         V2021[which(V2021[,1]==rname_fi),ncol(V2015)]))*
    1e6/1000/as.numeric(c(sum(areasLandClass2015),sum(areasLandClass2021)))  
  
  ggstats <- as.numeric(c(gg2015[which(gg2015[,1]==rname_fi),ncol(gg2015)],
                          gg2021[which(gg2021[,1]==rname_fi),ncol(gg2015)]))
  wstats <- 0.5*as.numeric(c(W2015[which(W2015[,1]==rname_fi),ncol(W2015)],
                             W2021[which(W2021[,1]==rname_fi),ncol(W2015)]))*
    1e6/as.numeric(c(sum(areasLandClass2015),sum(areasLandClass2021)))  
  ageclassstats <- rbind(as.numeric(ikaluokat2015[which(ikaluokat2015[,1]==rname_fi),-1]),
                         as.numeric(ikaluokat2021[which(ikaluokat2021[,1]==rname_fi),-1]))
  ageclassstats <- ageclassstats[,-ncol(ageclassstats)]
  ageclassstats[1,] <- cumsum(ageclassstats[1,]/areasLandClass2015[1])
  ageclassstats[2,] <- cumsum(ageclassstats[2,]/areasLandClass2021[1])
  lajistats2015 <- as.numeric(V2015[which(V2015[,1]==rname_fi),2:5])
  lajistats2021 <- as.numeric(V2021[which(V2021[,1]==rname_fi),2:5])
  netsinksreg <- NetSinks[which(NetSinks[,1]==regionNames_fi[r_no]),-1]
  netsinksreg[which(netsinksreg=="..")] <- NA  
  netsinksreg <- as.numeric(netsinksreg)
  netsinksreg_per_ha <- NetSinks_per_ha[which(NetSinks_per_ha[,1]==regionNames_fi[r_no]),-1]
  netsinksreg_per_ha[which(netsinksreg_per_ha=="..")] <- NA  
  netsinksreg_per_ha <- as.numeric(netsinksreg_per_ha)
  netsinksreg2025 <- NetSinks_2025[which(NetSinks_2025[,1]==regionNames_fi[r_no]),-1]
  netsinksreg2025[which(netsinksreg2025=="..")] <- NA  
  netsinksreg2025 <- as.numeric(netsinksreg2025)
  netsinksreg_per_ha2025 <- NetSinks_per_ha_2025[which(NetSinks_per_ha_2025[,1]==regionNames_fi[r_no]),-1]
  netsinksreg_per_ha2025[which(netsinksreg_per_ha2025=="..")] <- NA  
  netsinksreg_per_ha2025 <- as.numeric(netsinksreg_per_ha2025)
  
  print(paste("Start figs of region",r_no,"/",rname))
  #    main = regs[r_noi][r_noi,] <- c(sum(data.all$area[which(data.all$landclass==1)]),
  #                                sum(data.all$area[which(data.all$landclass==2)]))  
  #areaRegion <- totArea <- sum(data.all$area,na.rm=T)
  
  gc()
  
  sortVar <- c("landclass","peatID","cons")
  ri <- max(1,r_noi-1)
  dimnames(outresults_all)[[3]][ri]
  load(paste0(outDir,fname,"_rno",r_noi_file,".rdata"))
  #load(paste0(outDir,"results_agesample",samplaus,NFIlocal,"_compHarv",compHarvX,"ageHarvPrior",ageHarvPriorX,"_rno",ri,"_",rcps,".rdata"))  
  outresults_all[,,ri] <- array(unlist(outresults),dim(outresults))
  totArea <- areatable$areaTot
  netsinksreg_per_ha <-netsinksreg*1e9/totArea
  #validation_all[1,5:11,ri] <- netsinksreg*1e3 
  validation_all[1,5:13,ri] <- netsinksreg2025*1e3 
  validation_all[2,6:13,ri] <- outresults$NBEsum[which(timei%in%2016:2023)]/1e6
  validation_all[1,1:2,ri] <- ggstats
  validation_all[2,1:2,ri] <- outresults$grossGrowth_forest[c(1,which(timei==2021))]
  validation_all[1,3:4,ri] <- Vstats
  validation_all[2,3:4,ri] <- outresults$V_forest[c(1,which(timei==2021))]
  #validation_all[1,12:18,ri] <- netsinksreg_per_ha 
  validation_all[1,14:22,ri] <- netsinksreg_per_ha2025*1000 
  validation_all[2,15:22,ri] <- outresults$NBE[which(timei%in%2016:2023)]
  
  if(FIGsOnly){
    sortid <- 1
    for(sortid in 1:nsorts){
      if(sortid==1){
        sortVarnams <- c("forest","poorly_productive")
      } else if(sortid==2) {
        sortVarnams <- c("min","ditched_org")
      } else if(sortid==3) {
        sortVarnams <- c("managed","cons")
      }    
      
      KUVA <- T
      if(KUVA){
        timei <- 2015+1:nrow(outresults)
        par(mfrow=c(3,2))
        ij <- which(colnames(outresults)=="grossGrowth")
        tmp <- unlist(outresults[,..ij])
        plot(timei, tmp, type="l",main=paste("Region",r_no,regionNames_fi[r_no]), 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylim = c(0,9), ylab = "grossgrowth, m3/ha",lwd=3)
        points(c(2015,2021),ggstats,pch=19,col="red")
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- ij + ik
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        legend("bottomright",c(paste0("all "),
                               paste0(sortVarnams)),
               pch=rep(1,length(sortVarnams)+1),cex=0.7,
               bty = "n",
               col=c("black",colorsi[1:length(sortVarnams)]))
        
        # NEP
        ij <- which(colnames(outresults)=="NEP_yasso")
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("NEP_yasso_",sortVarnams),colnames(outresults)))
        ij3 <- c(ij2,match(c("NEP",paste0("NEP_",sortVarnams)),colnames(outresults)))
        ymax <- max(0,max(outresults[,..ij3]))
        ymin <- min(0,min(outresults[,..ij3]))
        plot(timei, tmp, type="l",main = regs[r_noi], 
             xlim = c(timei[1]-1,timei[length(timei)]),
             xlab = "time, dotted=NEPyasso, solid=NEP (incl.ditched peatlands)",
             ylab = "NEP, g/m2", ylim = c(ymin,ymax),
             lwd=3, lty=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- which(paste0("NEP_yasso_",sortVarnams[ik])==colnames(outresults))
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik],lty=3)
          }
        }
        
        ij <- which(colnames(outresults)=="NEP")
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("NEP_",sortVarnams),colnames(outresults)))
        ymax <- max(0,max(outresults[,..ij2]))
        ymin <- min(0,min(outresults[,..ij2]))
        lines(timei, tmp, lwd=3, col="black")
        #plot(timei, tmp, type="l",main=paste("Region",r_no,rname), 
        #     xlim = c(timei[1]-1,timei[length(timei)]),
        #     ylab = "NEP, g/m2", ylim = c(ymin,ymax),
        #     lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- which(paste0("NEP_",sortVarnams[ik])==colnames(outresults))
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        lines(timei,0*timei, col="black")
        
        # V
        ij <- which(colnames(outresults)=="V")
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("V_",sortVarnams),colnames(outresults)))
        ymax <- max(Vstats,max(outresults[,..ij2]))
        ymin <- min(outresults[,..ij2])
        plot(timei, tmp, type="l",main = regs[r_noi], 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylab = "V, m3/ha", ylim = c(0,ymax),
             lwd=3)
        points(c(2015,2021),Vstats,pch=19,col="red")
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- which(paste0("V_",sortVarnams[ik])==colnames(outresults))
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        
        # wf_STKG
        if(any(colnames(outresults)=="wf_STKG")){
        ij <- which(colnames(outresults)=="wf_STKG")
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("wf_STKG_",sortVarnams),colnames(outresults)))
        ymax <- max(outresults[,..ij2])
        ymin <- min(outresults[,..ij2])
        plot(timei, tmp, type="l",main=paste("Region",r_no,rname), 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylab = "Foliage biomass, kgC/ha", ylim = c(0,ymax),
             lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- which(paste0("wf_STKG_",sortVarnams[ik])==colnames(outresults))
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        }
        
        # age
        ij <- which(colnames(outresults)=="age")[1]
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("age_",sortVarnams),colnames(outresults)))
        ymax <- max(outresults[,..ij2])
        ymin <- min(outresults[,..ij2])
        plot(timei, tmp, type="l",main = regs[r_noi], 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylab = "age, years", ylim = c(0,ymax),
             lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- which(paste0("age_",sortVarnams[ik])==colnames(outresults))
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        
        # BA
        ij <- which(colnames(outresults)=="BA")
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("BA_",sortVarnams),colnames(outresults)))
        ymax <- max(outresults[,..ij2])
        ymin <- min(outresults[,..ij2])
        plot(timei, tmp, type="l",main = regs[r_noi], 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylab = "BA m2/ha", ylim = c(0,ymax),
             lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- ij2[ik+1]
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        
        # Wtot
        ij <- which(colnames(outresults)=="Wtot")
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("Wtot_",sortVarnams),colnames(outresults)))
        ymax <- max(outresults[,..ij2])
        ymin <- min(outresults[,..ij2])
        plot(timei, tmp, type="l",main = regs[r_noi], 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylab = "Wtot, kg C/ha", ylim = c(0,ymax),
             lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- ij2[ik+1]
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        
        # Vharvested
        ij <- which(colnames(outresults)=="Vharvested")
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("Vharvested_",sortVarnams),colnames(outresults)))
        ymax <- max(outresults[,..ij2])
        ymin <- min(outresults[,..ij2])
        plot(timei, tmp, type="l",main = regs[r_noi], 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylab = "Vharv, m3/ha", ylim = c(0,ymax),
             lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- ij2[1 + ik]
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        points(timei[1:nYears],rowSums(HarvLimMaak[1:nYears,])/totArea*1000,col="red")
        legend("bottomright",c(paste0("all "),
                               paste0(sortVarnams)),
               pch=rep(1,length(sortVarnams)+1),cex=0.7,
               bty = "n",
               col=c("black",colorsi[1:length(sortVarnams)]))
        
        
        # Wharvested
        ij <- which(colnames(outresults)=="Wharvested")
        CO2eq_C <- 44/12 
        tmp <- unlist(outresults[,..ij])*CO2eq_C
        ij2 <- c(ij,match(paste0("Wharvested_",sortVarnams),colnames(outresults)))
        ymax <- max(outresults[,..ij2])*CO2eq_C
        ymin <- min(outresults[,..ij2])*CO2eq_C
        plot(timei, tmp, type="l",main = regs[r_noi], 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylab = "Wharv, kg CO2eq/ha", ylim = c(0,ymax),
             lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- ij2[1 + ik]
          tmp <- unlist(outresults[,..ijk])*CO2eq_C
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        
        # NEE
        ij <- which(colnames(outresults)=="NEE")
        tmp <- unlist(outresults[,..ij])*CO2eq_C
        ij2 <- c(ij,match(paste0("NEE_",sortVarnams),colnames(outresults)))
        ymax <- max(0,max(outresults[,..ij2])*CO2eq_C)
        ymin <- min(0,min(outresults[,..ij2])*CO2eq_C)
        plot(timei, tmp, type="l",main = regs[r_noi], 
             xlim = c(timei[1]-1,timei[length(timei)]),
             ylab = "NEE, kg CO2eq/ha", ylim = c(ymin,ymax),
             lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- ij2[1 + ik]
          tmp <- unlist(outresults[,..ijk])*CO2eq_C
          if(length(tmp)>1){
            lines(timei, tmp,col=colorsi[ik])
          }
        }
        lines(timei,0*timei, col="black")
        
        
        if(TRUE){
          # NBE
          ij <- which(colnames(outresults)=="NBE")
          tmp <- unlist(outresults[,..ij])
          ij2 <- c(ij,match(paste0("NBE_",sortVarnams),colnames(outresults)))
          if(!is.na(netsinksreg[1])){ 
            netsinksreg_per_ha <-netsinksreg*1e9/totArea
            ymax <- 1.05*max(0,max(max(netsinksreg_per_ha,na.rm = T),max(outresults[,..ij2])))
            ymin <- 1.05*min(0,min(min(netsinksreg_per_ha,na.rm = T),min(outresults[,..ij2])))
          } else {
            ymax <- max(0,max(outresults[,..ij2]))
            ymin <- min(0,min(outresults[,..ij2]))
          }
          plot(timei, tmp, type="l",main = regs[r_noi], 
               xlim = c(timei[1]-1,timei[length(timei)]),
               ylab="NBE, kg CO2eq/ha", ylim = c(ymin,ymax),
               lwd=3)
          points(2015:2021, netsinksreg_per_ha, pch=19,col="red")
          points(2015:2023, netsinksreg_per_ha2025*1000, pch=19,col="purple")
          colorsi <- c("blue","green","pink")
          for(ik in 1:length(sortVarnams)){
            ijk <- ij2[1 + ik]
            tmp <- unlist(outresults[,..ijk])
            if(length(tmp)>1){
              lines(timei, tmp,col=colorsi[ik])
            }
          }
          lines(c(timei[1],timei[length(timei)]),c(0,0),col="black")
          
          # NBEsum
          ij <- which(colnames(outresults)=="NBEsum")
          tmp <- unlist(outresults[,..ij])
          ij2 <- c(ij,match(paste0("NBEsum_",sortVarnams),colnames(outresults)))
          if(!is.na(netsinksreg[1])){          
            ymax <- 1.05*max(0,max(max(netsinksreg*1e9,na.rm = T),max(outresults[,..ij2])))
            ymin <- 1.05*min(0,min(min(netsinksreg*1e9,na.rm = T),min(outresults[,..ij2])))
          } else {
            ymax <- max(0,max(outresults[,..ij2]))
            ymin <- min(0,min(outresults[,..ij2]))
          }
          plot(timei, tmp/1e6, type="l",main = regs[r_noi], 
               xlim = c(timei[1]-1,timei[length(timei)]),
               ylab="NBEsum, million kg CO2eq", ylim = c(ymin,ymax)/1e6,
               lwd=3)
          points(2015:2021,netsinksreg*1e9/1e6,pch=19,col="red")
          points(2015:2023,netsinksreg2025*1e9/1e6,pch=19,col="purple")
          colorsi <- c("blue","green","pink")
          for(ik in 1:length(sortVarnams)){
            ijk <- ij2[1 + ik]
            tmp <- unlist(outresults[,..ijk])
            if(length(tmp)>1){
              lines(timei, tmp/1e6,col=colorsi[ik])
            }
          }
          lines(c(timei[1],timei[length(timei)]),c(0,0),col="black")
          
        }
      }
      
    }
    rm(list=setdiff(ls(),toMem))
    gc()
  }
}

if(toFile){
  r_noi <- 1
  outresults_wholecountry <- areatable_wholecountry <- data.frame()
  for(r_noi in 1:length(rids)){
    toMem <- ls()
    set.seed(1)
    r_no <- rids[r_noi]
    rname <- regionNames[r_no]
    rname_fi <- regionNames_fi[r_no]
    rnameid <- r_nos[r_no]
    load(file = paste0(outDir,fname,"_rno",r_noi,".rdata"))
    #load(file = paste0(outDir,"results_agesample",samplaus,NFIlocal,"_compHarv",compHarvX,"ageHarvPrior",ageHarvPriorX,"_rno",r_noi,"_",rcps,".rdata"))  
    
    if(r_noi==1){
      ij <- which(grepl("NBEsum",colnames(outresults)))
      cnames <- colnames(outresults)[ij]
      outresults_wholecountry <- outresults[,..ij]
      areatable_wholecountry <- data.table(r_no, rname, areatable)
    } else {
      outresults_wholecountry <- outresults_wholecountry + outresults[,..cnames]
      #outresults[,..ij]
      areatable_wholecountry <- rbind(areatable_wholecountry,
                                      data.table(r_no, rname, areatable))
    }
    rm(list=setdiff(ls(), c(toMem,"cnames")))
    gc()
    
  }    
  
  #if(toFile) 
  save(outresults_wholecountry, outresults_all, validation_all, areatable_wholecountry,
       file = paste0(outDir,fname,"_wholeCountry",".rdata"))
       #file = paste0(outDir,"results_agesample",samplaus,NFIlocal,"_compHarv",compHarvX,"ageHarvPrior",ageHarvPriorX,"_wholeCountry_",rcps,".rdata"))  
  par(mfrow=c(3,1))
  sortid <- 1
  cnames <- colnames(outresults_wholecountry)
  timei <- 2015+1:nrow(outresults_wholecountry)
  for(sortid in 1:nsorts){
    if(sortid==1){
      sortVarnams <- c("forest","poorly_productive")
    } else if(sortid==2) {
      sortVarnams <- c("min","ditched_org")#,"natural_org")
    } else if(sortid==3) {
      sortVarnams <- c("managed","cons")
    }    
    ij <- c(1,which(cnames%in%paste0("NBEsum_",sortVarnams)))
    ymax <- max(outresults_wholecountry[,..ij])
    ymin <- min(outresults_wholecountry[,..ij])
    plot(timei, outresults_wholecountry$NBEsum/1e6, type="l",main="Whole country", 
         ylab="NBEsum, million kg CO2eq", ylim = c(ymin,ymax)/1e6,
         #xlim <- c(2015,2025),
         lwd=3)
    colorsi <- c("blue","green","pink")
    ik <- 1
    for(ik in 1:length(sortVarnams)){
      ijk <- ij[1 + ik]
      tmp <- unlist(outresults_wholecountry[,..ijk])
      if(length(tmp)>1){
        lines(timei, tmp/1e6,col=colorsi[ik])
      }
    }
    lines(c(timei[1],timei[length(timei)]),c(0,0),col="black")
    legend("bottomright",c("all",sortVarnams),
           bty = "n",
           pch=rep(1,length(sortVarnams)+1),cex=0.7,
           col=c("black",colorsi[1:length(sortVarnams)]))
    
  }
}


#if(FIGsOnly){
regionIDs <- as.numeric(c("1","21","16","06","09","13","11","19","05",
                          "15","02","14","07","04","08","18","10","12","17"))
regs <- c("Uu","Ah","KP","Pi","EK","KS","PS","La","KH","Po","VS",
          "EP","PH","Sa","Ky","Ka","ES","PK","PP")
regs <- paste(regionIDs,regs)
regs[match(c(1:2,4:19),r_nos)]
r_nois0 <- match(c(1:2,4:19),r_nos)
r_nois[-1] <- r_nois0[-1]-1
par(mfrow=c(2,1))
barplot(validation_all[1:2,1,r_nois],names.arg=regs[r_nois0],
        beside=T, legend.text =c("NFI","sim"), main="grossgrowth 2015/2016")
barplot(validation_all[1:2,2,r_nois],names.arg=regs[r_nois0],
        beside=T, main="grossgrowth 2021")
barplot(validation_all[1:2,3,r_nois],names.arg=regs[r_nois0],
        beside=T, main="volume 2015/2016")
barplot(validation_all[1:2,4,r_nois],names.arg=regs[r_nois0],
        beside=T, main="volume 2021")

ylims <- c(min(validation_all[,14:22,],na.rm = T),
           max(validation_all[,14:22,],na.rm = T))
par(mfrow=c(3,3))  
for(ii in r_nois){
  plot(2015:2023, 
       validation_all[2,14:22,ii], type="l",
       ylim = ylims, col="blue",
       main=dimnames(validation_all)[[3]][ii],
       xlab="year, blue=sim, red=NFI",ylab="NBEave")
  points(2015:2023, validation_all[1,14:22,ii], pch=19,col="red")
  lines(c(2015,2023),c(0,0),col="black")
}

if(TRUE){
  ylims <- c(min(validation_all[,5:13,],na.rm = T),
             max(validation_all[,5:13,],na.rm = T))
  par(mfrow=c(3,3))  
  for(ii in r_nois){
    plot(2015:2023, 
         validation_all[2,5:13,ii], type="l",
         ylim = ylims, col="blue",
         main=dimnames(validation_all)[[3]][ii],
         xlab="year",ylab="NBEsum")
    points(2015:2023, validation_all[1,5:13,ii], pch=19,col="red")
    lines(c(2015,2023),c(0,0),col="black")
  }
}

if(toFile) dev.off()
