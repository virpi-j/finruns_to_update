rm(list=ls())
gc()
if(dev.interactive()) dev.off()
library(dplyr)
library(ggplot2)
library(data.table)

# minDharvX = 999
# landClassX = 1
# mortMod=3
# regions <- 1:19 #c(8, 3,9,10,13,14,15,19)
setwd("/scratch/project_2000994/PREBASruns/PREBAStesting/")

#run_settings <- paste0("_clcutArFact",clcutArFact,
#                       "_addHarv",compHarvX,"_landClassX",range(landClassX)[1],
#                       "to",range(landClassX)[2],"_mortMod",mortMod)
regionNames <- c("Uusimaa", "Ahvenanmaa", "Keski-Pohjanmaa", "Pirkanmaa",
              "Etela-Karjala", "Keski-Suomi", "Pohjois-Savo", 
              "Lappi", "Kanta-Hame", "Pohjanmaa", "Varsinais-Suomi",
              "Etela-pohjanmaa", "Paijat-Hame", "Satakunta", "Kymenlaakso",
              "Kainuu", "Etela-Savo", "Pohjois-Karjala", "Pohjois-Pohjanmaa")
r_nos <- c(1, 21, 16, 6, 9, 13, 11, 19, 5, 15, 2, 14, 7, 4, 8, 18, 10, 12, 17) 
#regionNames <- fread("/scratch/project_2000994/PREBASruns/metadata/maakunta/maakunta_names.txt")
# outDyr <- "outSampleHcF1.2_cons10run"
vPREBAS <- "newVersion"

meanRegion <- data.table()
# areasCountry <- data.table()
# areasProtectCountry <- data.table()
# dataCountry <- data.table()
# dataProtectCountry <- data.table()
strangeSites <- NULL
areaAllRegions <- NULL
outDir <- "/scratch/project_2000994/PREBASruns/PREBAStesting/RegionRuns/"
CSCrun <- T
nSegs <- 20000
station_id <- 1

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
  landClassUnman=NULL; compHarvX = 0; funPreb = "regionPrebas"
  initSoilCreStart=NULL; outModReStart=NULL; reStartYear=1
  sampleX=NULL; P0currclim=NA; fT0=NA
  sampleID = 1; initAge=NA; disturbanceON <- NA; ingrowth <- F; TminTmax <- NA
}

rids <- c(1,3:19)
results <- array(0,c(7,nYears,length(rids),2))
dimnames(results) <- list(paste0("var",1:dim(results)[1]),1:nYears,regionNames[rids],c("sum","ave"))
r_no <- 3
for(r_noi in 1:length(rids)){
  toMem <- ls()

  r_no <- rids[r_noi]
  rname <- regionNames[r_no]
  rnameid <- r_nos[r_no]
  source("~/finruns_to_update/settings.R")
  #devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
  #source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
  
  areaRegion <- sum(data.all$area,na.rm=T)
  areaAllRegions <- c(areaAllRegions,areaRegion)
  
  totArea <- sum(data.all$area)
  dataS <- data.all[sample(1:nrow(data.all),nSegs,replace = F),]
  #load(paste0(outDir,"/r_no",r_no,run_settings,".rdata"))
  
  if(TRUE){
    load(paste0("/scratch/project_2000994/PREBASruns/finRuns/input/maakunta/maakunta_",r_no,"_IDsTab.rdata"))
    data.IDs <- data.IDs[segID!=0]
    data.IDs$segID <- data.IDs$maakuntaID
    setkey(data.IDs,segID)
    setkey(dataS,segID)
    #setkey(data.IDs,maakuntaID)
    
    tabX <- merge(data.IDs,dataS)
    ntabX <- tabX[,.I[which.max(y)],by=segID]$V1
    dataS <- cbind(dataS, tabX[ntabX,c("x","y")])
    
    set_thin_PROJ6_warnings(TRUE)
    xy <- dataS[,c("segID","x","y")]
    coordinates(xy) <- c("x","y")
    proj4string(xy) <- crsX
    #cord = SpatialPoints(xy, proj4string=CRS("+init=EPSG:3067"))
    location<-as.data.frame(spTransform(xy, CRS("+init=epsg:4326")))
    dataS$lat <- location$y
  }

  #plot(dataS$x, dataS$y, pch=21)
  ops <- list(dataS)
  source("~/adaptFirst_runs/functions.R")

  deltaID <- 1; outType <- "TestRun"; rcps = "CurrClim"; harvScen="Base"; harvInten="Base"; climScen=0  
  out <- runModelAdapt(1,sampleID=1, outType = "testRun", rcps = "CurrClim",
                harvScen="Base", harvInten="Base", climScen=0, procDrPeat=T,
                forceSaveInitSoil=F)
  
  #lapply(sampleIDs, 
  #       function(jx) { 
  #         runModelAdapt(1,sampleID=jx, outType = outType, rcps = "CurrClim_fmi",
  #                       harvScen="Base", harvInten="Base",
  #                       forceSaveInitSoil=T)
  gc()

  output <- out$region$multiOut
  areas <- dataS$area
  time <- (1:dim(output)[2])+2015
  
  V <- colSums(apply(output[,,"V",,1],1:2,sum)*areas)
  grossgrowth <- colSums(apply(output[,,"grossGrowth",,1],1:2,sum))
  NEE <- -colSums(apply(output[,,"NEP/SMI[layer_1]",,1],1:2,sum)*areas*ha_to_m2/g_to_kg)
  Wharvested <- colSums(apply(output[,,"WroundWood",,1],1:2,sum)+apply(out$region$multiEnergyWood[,,,2],1:2,sum))
  CH4em <- rep(sum(out$region$CH4emisDrPeat_kgyear*areas),nYears)  
  N2Oem <- rep(sum(out$region$N2OemisDrPeat_kgyear*areas),nYears)  
  
  NBE <- NEE + Wharvested
  Vharvested <- colSums(apply(output[,,"VroundWood",,1],1:2,sum)+apply(out$region$multiEnergyWood[,,,1],1:2,sum))
  KUVA <- T
  if(KUVA){
    par(mfrow=c(2,2))
    plot(time, grossgrowth/sum(areas), type="l",main=paste("Region",r_no))
    plot(time, V/sum(areas), type="l",main=paste("Region",r_no))
    plot(time, Vharvested/sum(areas)*totArea, type="l",main=paste("Region",r_no))
    plot(time, NBE/sum(areas), type="l",main=paste("Region",r_no))
    lines(c(time[1],time[length(time)]),c(0,0),col="black")
  }
  results_ave <- rbind(grossgrowth,V,Vharvested, NEE, Wharvested, CH4em, N2Oem)/sum(areas)
  results[,,r_noi,1] <- results_ave*totArea
  results[,,r_noi,2] <- results_ave
  dimnames(results)[1] <- c("grossgrowth","V","Vharvested", "NEE", "Wharvested", "CH4em", "N2Oem")
  save(results, file = paste0(outDir,"results.rdata"))  
  rm(list=setdiff(ls(), toMem))
  gc()
}

break
countryArea <- sum(areaAllRegions)

meanScenNorm <- meanRegion
meanScenNorm[, vars] <- 
  meanRegion[ ,lapply(.SD, `*`, area/countryArea), .SDcols = vars]

meanCountry <- meanScenNorm[ ,lapply(.SD, sum), .SDcols = vars,by=.(harScen,year,harvInten)]

meanCountry$CbalState=0
meanCountry[year %in% 2:max(meanCountry$year)]$CbalState=
  -(meanCountry[year %in% 2:max(meanCountry$year),
                (WtotTrees+soilC+GVw+Wdb)] -
      meanCountry[year %in% 1:(max(meanCountry$year)-1),
                  (WtotTrees+soilC+GVw+Wdb)])*44/12/1e9*
  countryArea

meanCountry[,CbalFluxes:=(-NEP*10+WenergyWood+WroundWood)*
              44/12*countryArea/1e9]
meanCountry[year ==1]$CbalState=NA

save(meanCountry,meanRegion,countryArea,
     file = paste0(outDyr,"/country",
                   run_settings,".rdata"))

# dataPlot <- meanCountry[harScen!="adaptTapio"]
dataPlot <- meanCountry
pdf(paste0(outDyr,"/plots/plots_country.pdf"))
for(varX in vars){
  # i=i+1
  print(ggplot(dataPlot)+
          # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
          geom_line(aes(x = year+ 2015, y = get(varX), color = harScen,linetype=harvInten)) + 
          xlab("year") + ylab(varX))
  # print(ggplot(meanCountry)+
  #         # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
  #         geom_line(aes(x = year+ 2016, y = get(varX)*countryArea/1e6,
  #                       color = harScen)) + 
  #         xlab("year") + ylab(varX))
}
print(ggplot(dataPlot)+
        # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
        # geom_line(aes(x = year+ 2016, y = CbalFluxes, color = harScen,linetype=harvInten)) + 
        geom_line(aes(x = year+ 2015, y = CbalState, color = harScen,linetype=harvInten)) +
        xlab("year") + ylab("C balance (State)"))
print(ggplot(dataPlot)+
        # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
        geom_line(aes(x = year+ 2015, y = CbalFluxes, color = harScen,linetype=harvInten)) +
        # geom_line(aes(x = year+ 2016, y = CbalState, color = harScen,linetype=harvInten)) +
        xlab("year") + ylab("C balance (Fluxes)"))
dev.off()


scens <- unique(meanRegion$harScen)
meanRegion$region <- as.factor(meanRegion$region)
meanRegion$regIDs <- meanRegion$region
meanRegion$region <- NULL
setkey(regionNames,regIDs)
setkey(meanRegion,regIDs)
regionNames$regIDs <- as.factor(regionNames$regIDs)
meanRegion <- merge(meanRegion,regionNames)

pdf(paste0(outDyr,"/plots/plots_ScenariosCountry.pdf"))
for(varX in vars){
  for(scenX in scens){
    # i=i+1
    print(ggplot(meanCountry[harScen==scenX])+
            # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
            geom_line(aes(x = year+ 2015, y = get(varX), color = harvInten)) + 
            xlab("year") + ylab(varX)+ ggtitle(scenX))
  }
}
dev.off()

# for(r_no in 1:19){
# pdf(paste0("outSample/plots/plots_Scenarios_",
#            regionNames[r_no]$regNames,".pdf"))
# for(varX in vars){
#   for(scenX in scens){
#     # i=i+1
#     print(ggplot(meanRegion[harScen==scenX & regIDs==r_no])+
#             # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
#             geom_line(aes(x = year+ 2016, y = get(varX), color = harvInten)) + 
#             xlab("year") + ylab(varX)+ ggtitle(scenX))
#   }
# }
# dev.off()
# }
# 

write.csv(meanCountry,file=paste0(outDyr,"/plots/MeanCountryAllRuns.csv"))
write.csv(meanCountry[harScen=="Base" & harvInten=="Base"],
          file=paste0(outDyr,"/plots/MeanCountryBaseRuns.csv"))
write.csv(meanRegion,file=paste0(outDyr,"/plots/MeanRegionAllRuns.csv"))
write.csv(meanRegion[harScen=="Base" & harvInten=="Base"],
          file=paste0(outDyr,"/plots/MeanRegionBaseRuns.csv"))


#####compare runs +10% cons areas vs. actual situation
dat1 <- fread(file="outSampleHcF1.2/plots/MeanCountryAllRuns.csv")
dat1$cons = "actual"

dat2 <- fread(file="outSampleHcF1.2_cons10run/plots/MeanCountryAllRuns.csv")
dat2$cons = "+10%"
datAll <- rbind(dat1,dat2)
names(dat2)

scens2 <- scens#[-which(scens%in%c("adaptTapio","NoHarv"))]
pdf(paste0(outDyr,"/plots/plots_ScenariosCountry2.pdf"))
for(varX in vars){
  for(scenX in scens2){
    # i=i+1
    print(ggplot(datAll[harScen==scenX])+
            # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
            geom_line(aes(x = year+ 2015, y = get(varX), color = cons,linetype=harvInten)) +
            xlab("year") + ylab(varX)+ ggtitle(scenX))
  }
}
dev.off()
