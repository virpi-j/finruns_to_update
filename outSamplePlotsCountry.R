#  rm(list=ls())
gc()
if(dev.interactive()) dev.off()

if(!exists("toFile")) toFile <- F
if(!exists("samplaus")) samplaus <- F

library(dplyr)
library(ggplot2)
library(data.table)
print(toFile)
vPREBAS <- "newVersion"
#vPREBAS <- "master"
if(!exists("nSegs")) nSegs <- 5000
if(!exists("fmi_from_allas")) fmi_from_allas <- F

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
rids <- c(1,3:19)

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
#lajiV2015 <- read_excel(path = "/users/vjunttil/finruns_to_update/VMIstats.xlsx",  
#                            sheet = "lajitilavuudet", range = "B3:H25")
#lajiV2021 <- read_excel(path = "/users/vjunttil/finruns_to_update/VMIstats.xlsx",  
#                        sheet = "lajitilavuudet", range = "B26:H47")

landclassMSNFI <- array(0,c(length(rids),2),dimnames = list(regionNames_fi[rids],c("metsämaa","kitumaa")))
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
  landClassUnman=NULL; funPreb = "regionPrebas"
  initSoilCreStart=NULL; outModReStart=NULL; reStartYear=1
  sampleX=NULL; P0currclim=NA; fT0=NA
  sampleID = 1; initAge=NA; disturbanceON <- NA; ingrowth <- F; TminTmax <- NA
}

results <- array(0,c(8,nYears,length(rids),2))
dimnames(results) <- list(c("grossgrowth","V","Vharvested", "NEE", "Wharvested", "CH4em", "N2Oem","NBE"),1:nYears,regionNames[rids],c("sum","ave"))

r_noi <- 7
if(!toFile) rids <- rids[1:3]
if(toFile) pdf(paste0(outDir,"results_agesample",samplaus,"compHarv",compHarvX,"ageHarvPrior",ageHarvPriorX,".pdf"))
for(r_noi in 1:length(rids)){
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
  
  print(paste("Start running region",r_no,"/",rname))
  landclassMSNFI[r_noi,] <- c(sum(data.all$area[which(data.all$landclass==1)]),
                              sum(data.all$area[which(data.all$landclass==2)]))  
  areaRegion <- totArea <- sum(data.all$area,na.rm=T)
  
  if(samplaus){
    sampleArea <- nSegs*median(data.all$area)*1
    sample_weight <- as.numeric(ikaluokat2015[which(ikaluokat2015[,1]==rname_fi),2:(ncol(ikaluokat2015)-1)])
    sample_weight_lc1 <- sample_weight/sum(sample_weight)
    #ikaid <- array(0,c(nrow(data.all),1))
    ages <- round(data.all$age)
    agelimits <- c(0,20,40,60,80,100,120,140,1e4)
    
    agelimitsii <- 0:max(150,max(data.all$age))
    agelimits[length(agelimits)] <- agelimitsii[length(agelimitsii)]
    pagelimitsii <- pagedata <- pagesample <- array(0,c(length(agelimitsii),1))
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
        if(FALSE){
          pagedataprev <- sum(data.all$area[which(data.all$age[n_lc1]<=agelimits[iiprev])])/totArea_lc1
          pagedatanext <- sum(data.all$area[which(data.all$age[n_lc1]<=agelimits[iiprev+1])])/totArea_lc1
          pagedataii <- sum(data.all$area[which(data.all$age[n_lc1]<=agelimitsii[ii])])/totArea_lc1        
          pageii <- pwages[iiprev] + 
            (pwages[iiprev+1]-pwages[iiprev])/
            (agelimits[iiprev+1]-(ageprev))*(agelimitsii[ii]-(ageprev))
          pagelimitsii[ii] <- pwages[iiprev] + 
            (pagedataii - pagedataprev)/
            (pagedatanext - pagedataprev)*(pageii - pwages[iiprev])
        }
        if(TRUE){
        pagelimitsii[ii] <- pwages[iiprev] + 
          (pwages[iiprev+1]-pwages[iiprev])/
          (agelimits[iiprev+1]-(ageprev))*(agelimitsii[ii]-(ageprev))
        }
      }
      pagedata[ii] <- sum(data.all$area[which(data.all$age[n_lc1]<=agelimitsii[ii])])/totArea_lc1
    }
    plot(agelimitsii, pagelimitsii, type="l", xlab="age", ylab="cumsum(area) quantiles")
    points(agelimits, pwages, pch=19)
    lines(agelimitsii,pagedata, col="red")
    #ri <- runif(nSegs)
    ii <- 1
    nirandom <- NULL
    areashares <- array(0,c(length(agelimitsii),1))
    pareashares <- c(pagelimitsii[1],pagelimitsii[-1]-pagelimitsii[-length(pagelimitsii)])
    pareashares2 <- pareashares/sum(pareashares)*sampleArea
#    pareashares2 <- pareashares/sum(pareashares)*totArea*nSegs/nrow(data.all)
    for(ii in 1:length(agelimitsii)){
      agei <- agelimitsii[ii]
      ni <- which(ages==agei)
      if(length(ni)==0){
        ni <- which(ages==ages[which.min((data.all$age[n_lc1]-agei)^2)[1]])
      }
      while(areashares[ii]<=1*pareashares2[ii]){
        nii <- ni[sample(1:length(ni),1)]
        nirandom <- c(nirandom,nii)
        areashares[ii] <- areashares[ii]+data.all$area[n_lc1[nii]]
      }
    }
    nirandom <- n_lc1[nirandom]
    # add landclass 2 segments
    nLC2 <- round(length(n_lc2)/nrow(data.all)*length(nirandom))
    nirandom <- c(nirandom,n_lc2[sample(1:length(n_lc2),nLC2,replace = T)])
    if(FALSE){
    for(ii in 1:length(ri)){
      agei <- which(pagelimitsii<=ri[ii])
      if(length(agei)>0){      
        agei <- agelimitsii[agei[length(agei)]]
      } else {
        agei <- 0
      }
      ni <- which(ages==agei)
      if(length(ni)==0){
        ni <- which(ages==ages[which.min((data.all$age-agei)^2)[1]])
      }
      if(pareashares2[agei+1]<=areashares[agei+1]){
        probsi <- 1/data.all$area[ni]
        ni <- ni[sample(1:length(ni),1,prob = probsi/sum(probsi))]
      } else {
        ni <- ni[sample(1:length(ni),1)]
      }
      areashares[agei+1] <- areashares[agei+1]+ data.all$area[ni]
      if(is.na(ni)) break
       print(data.all$age[ni])
      nirandom <- c(nirandom,ni)
    }}
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
    lines(agelimitsii, pagesample, col="green")
    legend(x = "topleft", box.col = "black", 
           lty = c(NA,1,1,1),
           pch = c(19,NA,NA,NA),
           col = c("black","black","red","green"),
           #bg ="yellow", box.lwd = 2 , #title="EQUATIONS",  
           legend=c("VMIstats","VMIstats lin.line", "MVMI","sample"))  
    
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
  } else {
    dataS <- data.all[sample(1:nrow(data.all),nSegs,replace = F),]
  }
  
  #lajistats2015
  #round(colSums(dataS[,c("pine","spruce","birch","decid")])/sum(dataS$area)*sum(data.all$area)/1e6)
  dataS$decid <- dataS$birch + dataS$decid
  dataS$birch <- 0

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
    
    #set_thin_PROJ6_warnings(TRUE)
    xy <- dataS[,c("segID","x","y")]
    coordinates(xy) <- c("x","y")
    proj4string(xy) <- crsX
    #cord = SpatialPoints(xy, proj4string=CRS("+init=EPSG:3067"))
    location<-as.data.frame(spTransform(xy, CRS("+init=epsg:4326")))
    dataS$lat <- location$coords.x2#location$y
  }
  rcps <- "CurrClim"
  print(fmi_from_allas)
  # fmi data from allas
  if(fmi_from_allas){
    toMemFmi <- ls()
    source("0.5_get_fmi_from_allas.R")
    repo <- "ForModLabUHel/fmi.weather.finland"
    file_path <- "r/init_setup.R"
    branch <- "main"
    # Get init functions from github
    init_funs <- fetch_file_from_github(repo, file_path, branch)
    eval(parse(text = init_funs))
    rm(init_funs, file_path, repo)
    # SET PARAMETERS
    resolution <- 1 # Resolution in km (1, 5 or 9)
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
    
    # Change file name
    file.rename(list.files(path=workdir, pattern="fmi_vars_", all.files=FALSE,full.names=FALSE)[1],
                "fmi_vars_PREBAS.rdata")
    file.rename(list.files(path=workdir, pattern="climID_lookup_", all.files=FALSE,full.names=FALSE)[1],
                "climID_lookup.rdata")
    rm(list = setdiff(ls(), toMemFmi))
    gc()
    rcps <- "CurrClim_fmi"
    
  }
  
  source("~/finruns_to_update/functions.R")
  
  deltaID <- 1; outType <- "TestRun"; harvScen="Base"; harvInten="Base"; climScen=0  
  out <- runModel(1,sampleID=1, outType = "testRun", rcps = rcps, RCP=0,
                harvScen="Base", harvInten="Base", procDrPeat=T, 
                thinFactX= thinFactX,
                compHarvX = compHarvX,ageHarvPriorX = ageHarvPriorX,
                forceSaveInitSoil=F, sampleX = dataS)
  print(sum(dataS$area))
  #lapply(sampleIDs, 
  #       function(jx) { 
  #         runModelAdapt(1,sampleID=jx, outType = outType, rcps = "CurrClim_fmi",
  #                       harvScen="Base", harvInten="Base",
  #                       forceSaveInitSoil=T)
# out <- runModelAdapt(1,sampleID=1, outType = "testRun", rcps = rcps,
#                       harvScen="NoHarv", harvInten="NoHarv", climScen=0, procDrPeat=T,
#                       forceSaveInitSoil=F)
  gc()

  output <- out$region$multiOut
  areas <- dataS$area
  time <- (1:dim(output)[2])+2015
  
  
  n_lc1 <- which(dataS$landclass==1)
  n_lc2 <- which(dataS$landclass==2)
  areas1 <- areas[n_lc1]
  areas2 <- areas[n_lc2]
  
  ti <- 1
  ikaluokat <- array(0,c(nYears,9))
  agelimits <- c(0,20,40,60,80,100,120,140,1e4)
  for(ti in 1:nYears){
#    ages <- apply(output[,ti,"age",,1]*output[,ti,"BA",,1],1,sum)/apply(output[,ti,"BA",,1],1,sum)
    ages <- output[,ti,"age",1,1]
    
    ages[is.na(ages)] <- 0 
    ages <- ages[n_lc1]
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
  
  
  V <- colSums(apply(output[,,"V",,1],1:2,sum)*areas)/sum(areas)
  Vlc1 <- colSums(apply(output[n_lc1,,"V",,1],1:2,sum)*areas1)/sum(areas1)
  Vlc2 <- colSums(apply(output[n_lc2,,"V",,1],1:2,sum)*areas2)/sum(areas2)
  
  Wtot <-   colSums(apply(output[,,c(24,25,31,32,33),,1],1:2,sum)*areas)/sum(areas)
  Wtotlc1 <-   colSums(apply(output[n_lc1,,c(24,25,31,32,33),,1],1:2,sum)*areas1)/sum(areas1)
  Wtotlc2 <-   colSums(apply(output[n_lc2,,c(24,25,31,32,33),,1],1:2,sum)*areas2)/sum(areas2)
  
  BA <- colSums(apply(output[,,"BA",,1],1:2,sum)*areas)/sum(areas)
  BAlc1 <- colSums(apply(output[n_lc1,,"BA",,1],1:2,sum)*areas1)/sum(areas1)
  BAlc2 <- colSums(apply(output[n_lc2,,"BA",,1],1:2,sum)*areas2)/sum(areas2)
  
  grossgrowth <- colSums(apply(output[,,43,,1],1:2,sum)*areas)/sum(areas)#grossgrowth
  grossgrowthlc1 <- colSums(apply(output[n_lc1,,43,,1],1:2,sum)*areas1)/sum(areas1)#grossgrowth
  grossgrowthlc2 <- colSums(apply(output[n_lc2,,43,,1],1:2,sum)*areas2)/sum(areas2)#grossgrowth
  
  NEP <- colSums(apply(output[,,"NEP/SMI[layer_1]",,1],1:2,sum)*areas)/sum(areas)
  NEPlc1 <- colSums(apply(output[n_lc1,,"NEP/SMI[layer_1]",,1],1:2,sum)*areas1)/sum(areas1)
  NEPlc2 <- colSums(apply(output[n_lc2,,"NEP/SMI[layer_1]",,1],1:2,sum)*areas2)/sum(areas2)
  
  NEE <- -NEP*ha_to_m2/g_to_kg
  NEElc1 <- -NEPlc1*ha_to_m2/g_to_kg
  NEElc2 <- -NEPlc2*ha_to_m2/g_to_kg
  #NEE <- -colSums(apply(output[,,"NEP/SMI[layer_1]",,1],1:2,sum)*areas*ha_to_m2/g_to_kg)/sum(areas)
  Wharvested <- colSums(apply(output[,,"WroundWood",,1],1:2,sum)*areas+
                          apply(out$region$multiEnergyWood[,,,2],1:2,sum)*areas)/sum(areas)
  Wharvestedlc1 <- colSums(apply(output[n_lc1,,"WroundWood",,1],1:2,sum)*areas1+
                          apply(out$region$multiEnergyWood[n_lc1,,,2],1:2,sum)*areas1)/sum(areas1)
  Wharvestedlc2 <- colSums(apply(output[n_lc2,,"WroundWood",,1],1:2,sum)*areas2+
                          apply(out$region$multiEnergyWood[n_lc2,,,2],1:2,sum)*areas2)/sum(areas2)
  
  CH4em <- sum(out$region$CH4emisDrPeat_kgyear*areas)/sum(areas)  
  N2Oem <- sum(out$region$N2OemisDrPeat_kgyear*areas)/sum(areas)  
  
  CH4emlc1 <- sum(out$region$CH4emisDrPeat_kgyear[n_lc1]*areas1)/sum(areas1)  
  N2Oemlc1 <- sum(out$region$N2OemisDrPeat_kgyear[n_lc1]*areas1)/sum(areas1)  
  CH4emlc2 <- sum(out$region$CH4emisDrPeat_kgyear[n_lc2]*areas2)/sum(areas2)  
  N2Oemlc2 <- sum(out$region$N2OemisDrPeat_kgyear[n_lc2]*areas2)/sum(areas2)  
  
  NBE <- NEE + Wharvested
  NBElc1 <- NEElc1 + Wharvestedlc1
  NBElc2 <- NEElc2 + Wharvestedlc2
  
  VroundWood <- colSums(apply(output[,,"VroundWood",,1],1:2,sum)*areas)/sum(areas)
  VroundWoodlc1 <- colSums(apply(output[n_lc1,,"VroundWood",,1],1:2,sum)*areas1)/sum(areas1)
  VroundWoodlc2 <- colSums(apply(output[n_lc2,,"VroundWood",,1],1:2,sum)*areas2)/sum(areas2)
  
  VenergyWood <- colSums(apply(out$region$multiEnergyWood[,,,1],1:2,sum)*areas)/sum(areas)
  VenergyWoodlc1 <- colSums(apply(out$region$multiEnergyWood[n_lc1,,,1],1:2,sum)*areas1)/sum(areas1)
  VenergyWoodlc2 <- colSums(apply(out$region$multiEnergyWood[n_lc2,,,1],1:2,sum)*areas2)/sum(areas2)
  
  Vharvested <- VroundWood+VenergyWood
  Vharvestedlc1 <- VroundWoodlc1+VenergyWoodlc1
  Vharvestedlc2 <- VroundWoodlc2+VenergyWoodlc2
  KUVA <- T
  if(KUVA){
    par(mfrow=c(3,2))
    plot(time, grossgrowth, type="l",main=paste("Region",r_no,rname), 
         ylim = c(0,8))
    points(c(2015,2021),ggstats,pch=19,col="red")
    lines(time, grossgrowthlc1,col="blue")
    if(length(n_lc2)>1) lines(time, grossgrowthlc2,col="green")
    #    plot(time, BA, type="l",main=paste("Region",r_no,rname))
    plot(time, NEP, type="l",main=paste("Region",r_no),
         ylim=c(min(NEP),max(NEP)))
    lines(time, NEPlc1, col="blue")
    if(length(n_lc2)>1) lines(time, NEPlc2, col="green")

    plot(time, V, type="l",main=paste("Region",r_no), ylim = c(0,160))
    points(c(2015,2021),Vstats,pch=19,col="red")
    lines(time, Vlc1, col="blue")
    if(length(n_lc2)>1) lines(time, Vlc2, col="green")
    
    plot(time, Wtot, type="l",main=paste("Region",r_no), 
         ylim = c(0,60000))
    points(c(2015,2021),wstats,pch=19,col="red")
    lines(time, Wtotlc1, col="blue")
    if(length(n_lc2)>1) lines(time, Wtotlc2, col="green")
    
    plot(time, Vharvested, type="l",main=paste("Region",r_no), ylim=c(0,max(Vharvested)))
    points(time[1:nYears],rowSums(HarvLimMaak[1:nYears,])/totArea*1000,col="red")
    lines(time, Vharvestedlc1, col="blue")
    if(length(n_lc2)>1)lines(time, Vharvestedlc2, col="green")
    
    plot(time, NBE, type="l",ylim = 1.05*c(min(0,min(NBE)),max(max(NBE),0)),main=paste("Region",r_no))
    lines(c(time[1],time[length(time)]),c(0,0),col="black")
    lines(time, NBElc1, col="blue")
    if(length(n_lc2)>1) lines(time, NBElc2, col="green")
    
    par(mfrow=c(1,1))
    datagroups <- c("a: 0","b: 1-20","c: 21-40","d: 41-60","e: 61-80","f:81-100","g: 101-120","h: 121-140","i: 140-")
    data <- data.frame(time=rep(time,each=ncol(ikaluokat)), 
                       shares = c(t(ikaluokat)), classes =rep(datagroups,length(time)))
    a1 <- ggplot(data, aes(x=time, y=shares, fill=classes)) + 
      geom_area(alpha=0.6, size=.5, colour="white") + 
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
    
  }
  results_ave <- rbind(grossgrowth,V,Vharvested, NEE, Wharvested, CH4em, N2Oem, NBE)/sum(areas)
  results[,,r_noi,1] <- results_ave*totArea
  results[,,r_noi,2] <- results_ave
  if(toFile) save(results, landclassMSNFI, ikaluokat, 
                  file = paste0(outDir,"results_agesample",samplaus,"compHarv",compHarvX,"ageHarvPrior",ageHarvPriorX,".rdata"))  
 # rm(list=setdiff(ls(), toMem))
  gc()
}
if(toFile) dev.off()
