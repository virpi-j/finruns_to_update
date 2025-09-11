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
if(exists("rids")) rids <- c(1,3:19)

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
#if(!toFile) rids <- rids[1:3]
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
  
  if(TRUE){
    load(paste0("/scratch/project_2000994/PREBASruns/finRuns/input/maakunta/maakunta_",r_no,"_IDsTab.rdata"))
    data.IDs <- data.IDs[segID!=0]
    data.IDs$segID <- data.IDs$maakuntaID
    setkey(data.IDs,segID)
    setkey(data.all,segID)
    #setkey(data.IDs,maakuntaID)
    
    tabX <- merge(data.IDs,data.all)
    ntabX <- tabX[,.I[which.max(y)],by=segID]$V1
    data.all <- cbind(data.all, tabX[ntabX,c("x","y")])
    
    #set_thin_PROJ6_warnings(TRUE)
    xy <- data.all[,c("segID","x","y")]
    coordinates(xy) <- c("x","y")
    proj4string(xy) <- crsX
    #cord = SpatialPoints(xy, proj4string=CRS("+init=EPSG:3067"))
    location<-as.data.frame(spTransform(xy, CRS("+init=epsg:4326")))
    data.all$lat <- location$coords.x2#location$y
    
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
      #peatIDs <-extract(finPeats, cbind(data.all$x,data.all$y))
      #print("Save peatIDs.")
      #save(peatIDs, file=paste0("uncRuns/peatID_reg",r_no,".rdata"))
    }
    #data.all[,peatID:=peatIDs]
    data.all[,peatID:=peatIDs]
    data.all$peatID[which(data.all$peatID==100)]<-0
    data.all$peatID[which(data.all$peatID==400)]<-1
    data.all$peatID[which(data.all$peatID==700)]<-2
    data.all <- data.all[which(data.all$peatID!=2),]
    rm(list=c("finPeats","peatIDs","data.IDs","tabX","ntabX","xy"))
    gc()
  }
  if(samplaus){
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
  } else {
    dataS <- data.all[sample(1:nrow(data.all),nSegs,replace = F),]
    #rm("data.all")
    gc()
  }
  
  #lajistats2015
  #round(colSums(dataS[,c("pine","spruce","birch","decid")])/sum(dataS$area)*sum(data.all$area)/1e6)
  dataS$decid <- dataS$birch + dataS$decid
  dataS$birch <- 0

  rcps <- "CurrClim"
  print(fmi_from_allas)
  # fmi data from allas
  if(fmi_from_allas){
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
    
    # clear memory
    rm(list = setdiff(ls(), toMemFmi))
    gc()
    
    # Change file name
    workdir <- getwd()
    rcps <- "CurrClim_fmi"
    fmi_vars_PREBAS_file <-  paste0("fmi_vars_PREBAS_dataS_",r_noi,".rdata")
    climID_lookup_file <- paste0("climID_lookup_dataS_",r_noi,".rdata")
    
    file.rename(list.files(path=workdir, pattern="fmi_vars_", all.files=FALSE,full.names=FALSE)[1],
                fmi_vars_PREBAS_file)
    file.rename(list.files(path=workdir, pattern="climID_lookup_", all.files=FALSE,full.names=FALSE)[1],
                climID_lookup_file)
  }
  
  source("~/finruns_to_update/functions.R")
  
  MANUALRUN <- F
  if(MANUALRUN){
    easyInit=FALSE; forceSaveInitSoil=F; cons10run = F; coeffPeat1=-240; coeffPeat2=70; coefCH4 = 0.34; coefN20_1 = 0.23; coefN20_2 = 0.077; climScen = 0; clcut=1;  funPreb = regionPrebas; ingrowth = F; initSoilCreStart=NULL; outModReStart=NULL; reStartYear=1; climdata=NULL; sampleX=dataS; P0currclim=NA; fT0=NA; disturbanceON=NA; TminTmax=NA
    deltaID <- 1; outType <- "TestRun"; harvScen="Base"; harvInten="Base"; climScen=0  
    procDrPeat=T; landClassUnman = 2; forceSaveInitSoil=F; sampleX = dataS  
  }
  out <- runModel(1,sampleID=1, outType = "testRun", rcps = "CurrClim", climScen = 0,#RCP=0,
                harvScen="Base", harvInten="Base", procDrPeat=T, 
                thinFactX= thinFactX, landClassUnman = 2,
                compHarvX = compHarvX,ageHarvPriorX = ageHarvPriorX,
                forceSaveInitSoil=F, sampleX = dataS)
  print(paste("Sample area:",sum(dataS$area)))
  if(HarvScen!="Base" | fmi_from_allas){
    workdir <- paste0(getwd(),"/")  
    startingYear <- 2015
    endingYear <- 2024
    nYears <- endingYear-startingYear
    out <- runModel(1,sampleID=1, outType = "testRun", rcps = rcps, climScen = 0,#RCP=0,
                  harvScen=harvScen, harvInten=HarvInten, procDrPeat=T, 
                  thinFactX= thinFactX, landClassUnman = 2,
                  compHarvX = compHarvX,ageHarvPriorX = ageHarvPriorX,
                  forceSaveInitSoil=F, sampleX = dataS)
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

  out$region <- peat_regression_model_multiSite(out$region,which(dataS$peatID==1)) 
  
  output <- out$region$multiOut
  areas <- dataS$area
  time <- (1:dim(output)[2])+2015

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
  
  totArea <- sum(data.all$area)
  sortVar <- c("landclass","peatID","cons")
  outresults <- areatable <- data.table()
  sortid <- 1
  areatable <- cbind(areatable, areaTot = totArea)
  for(sortid in 1:3){
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
    varis <- c("V","age","Wtot","BA","grossGrowth","NEP/SMI[layer_1]",
               "Wharvested","Vharvested","VroundWood","Venergywood",
               "Vmort","CH4em","N2Oem")
    variNams <- c("V","age","Wtot","BA","grossGrowth","NEP",
                  "Wharvested","Vharvested","VroundWood","Venergywood",
                  "Vmort","CH4em","N2Oem")
  #  outresults <- outresultsSum <- data.table()
    ij <- 1
    for(ij in 1:length(varis)){
      if(varis[ij]=="Wtot"){ 
        tmp <- apply(output[,,c(24,25,31,32,33),,1],c(1,2,4),sum)
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
        tmp <- output[,,which(varNames==varis[ij]),,1]
      }
      if(varis[ij]%in%c("V","Wtot","BA","grossGrowth","NEP/SMI[layer_1]","Wharvested","Vharvested","VroundWood","Venergywood","Vmort","CH4em","N2Oem")){ # sums
        if(varis[ij]%in%c("CH4em","N2Oem")){
          outres <- sum(tmp*areas)/sum(areas)
        } else {
          outres <- colSums(apply(tmp,1:2,sum)*areas)/sum(areas)
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
            outres <- colSums(apply(tmp[ni,,],1:2,sum)*areas[ni])/sum(areas[ni])
          }
          outresults <- cbind(outresults, outres)
          colnames(outresults)[ncol(outresults)] <- paste0(variNams[ij],"_",sortVarnams[ik])
          #outresultsSum <- cbind(outresultsSum, outres*sortTotAreas[ik])
          #colnames(outresultsSum)[ncol(outresultsSum)] <- paste0(variNams[ij],"_",sortVarnams[ik])
          #assign(paste0(varis[ij],"_lc",ik),outres)
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
      par(mfrow=c(2,2))
      ij <- which(colnames(outresults)=="grossGrowth")
      tmp <- unlist(outresults[,..ij])
      plot(time, tmp, type="l",main=paste("Region",r_no,rname), 
           xlim = c(time[1]-1,time[length(time)]),
           ylim = c(0,9), ylab = "grossgrowth, m3/ha",lwd=3)
      points(c(2015,2021),ggstats,pch=19,col="red")
      colorsi <- c("blue","green","pink")
      for(ik in 1:length(sortVarnams)){
        ijk <- ij + ik
        tmp <- unlist(outresults[,..ijk])
        if(length(tmp)>1){
          lines(time, tmp,col=colorsi[ik])
        }
      }
      legend("bottomright",c(paste0("all ",round(totArea/1000),"kha"),
                             paste0(sortVarnams," ", round(sortTotAreas/1000),"kha")),
             pch=rep(1,length(sortVarnams)+1),cex=0.7,
             bty = "n",
             col=c("black",colorsi[1:length(sortVarnams)]))

      # NEP
      ij <- which(colnames(outresults)=="NEP")
      tmp <- unlist(outresults[,..ij])
      ij2 <- c(ij,match(paste0("NEP_",sortVarnams),colnames(outresults)))
      ymax <- max(0,max(outresults[,..ij2]))
      ymin <- min(0,min(outresults[,..ij2]))
      plot(time, tmp, type="l",main=paste("Region",r_no,rname), 
           xlim = c(time[1]-1,time[length(time)]),
           ylab = "NEP, g/m2", ylim = c(ymin,ymax),
           lwd=3)
      colorsi <- c("blue","green","pink")
      for(ik in 1:length(sortVarnams)){
        ijk <- which(paste0("NEP_",sortVarnams[ik])==colnames(outresults))
        tmp <- unlist(outresults[,..ijk])
        if(length(tmp)>1){
          lines(time, tmp,col=colorsi[ik])
        }
      }
      lines(time,0*time, col="black")
      
      # V
      ij <- which(colnames(outresults)=="V")
      tmp <- unlist(outresults[,..ij])
      ij2 <- c(ij,match(paste0("V_",sortVarnams),colnames(outresults)))
      ymax <- max(outresults[,..ij2])
      ymin <- min(outresults[,..ij2])
      plot(time, tmp, type="l",main=paste("Region",r_no,rname), 
           xlim = c(time[1]-1,time[length(time)]),
           ylab = "V, m3/ha", ylim = c(0,ymax),
           lwd=3)
      points(c(2015,2021),Vstats,pch=19,col="red")
      colorsi <- c("blue","green","pink")
      for(ik in 1:length(sortVarnams)){
        ijk <- which(paste0("V_",sortVarnams[ik])==colnames(outresults))
        tmp <- unlist(outresults[,..ijk])
        if(length(tmp)>1){
          lines(time, tmp,col=colorsi[ik])
        }
      }
      
      # age
      ij <- which(colnames(outresults)=="age")[1]
      tmp <- unlist(outresults[,..ij])
      ij2 <- c(ij,match(paste0("age_",sortVarnams),colnames(outresults)))
      ymax <- max(outresults[,..ij2])
      ymin <- min(outresults[,..ij2])
      plot(time, tmp, type="l",main=paste("Region",r_no,rname), 
           xlim = c(time[1]-1,time[length(time)]),
           ylab = "age, years", ylim = c(0,ymax),
           lwd=3)
      colorsi <- c("blue","green","pink")
      for(ik in 1:length(sortVarnams)){
        ijk <- which(paste0("age_",sortVarnams[ik])==colnames(outresults))
        tmp <- unlist(outresults[,..ijk])
        if(length(tmp)>1){
          lines(time, tmp,col=colorsi[ik])
        }
      }
      
      # Wtot
      ij <- which(colnames(outresults)=="Wtot")
      tmp <- unlist(outresults[,..ij])
      ij2 <- c(ij,match(paste0("Wtot_",sortVarnams),colnames(outresults)))
      ymax <- max(outresults[,..ij2])
      ymin <- min(outresults[,..ij2])
      plot(time, tmp, type="l",main=paste("Region",r_no,rname), 
           xlim = c(time[1]-1,time[length(time)]),
           ylab = "Wtot, kg C/ha", ylim = c(0,ymax),
           lwd=3)
      colorsi <- c("blue","green","pink")
      for(ik in 1:length(sortVarnams)){
        ijk <- ij2[ik+1]
        tmp <- unlist(outresults[,..ijk])
        if(length(tmp)>1){
          lines(time, tmp,col=colorsi[ik])
        }
      }
      legend("bottomright",c(paste0("all ",round(totArea/1000),"kha"),
                             paste0(sortVarnams," ", round(sortTotAreas/1000),"kha")),
             pch=rep(1,length(sortVarnams)+1),cex=0.7,
             bty = "n",
             col=c("black",colorsi[1:length(sortVarnams)]))
      
      # Vharvested
      ij <- which(colnames(outresults)=="Vharvested")
      tmp <- unlist(outresults[,..ij])
      ij2 <- c(ij,match(paste0("Vharvested_",sortVarnams),colnames(outresults)))
      ymax <- max(outresults[,..ij2])
      ymin <- min(outresults[,..ij2])
      plot(time, tmp, type="l",main=paste("Region",r_no,rname), 
           xlim = c(time[1]-1,time[length(time)]),
           ylab = "Vharv, m3/ha", ylim = c(0,ymax),
           lwd=3)
      colorsi <- c("blue","green","pink")
      for(ik in 1:length(sortVarnams)){
        ijk <- ij2[1 + ik]
        tmp <- unlist(outresults[,..ijk])
        if(length(tmp)>1){
          lines(time, tmp,col=colorsi[ik])
        }
      }
      points(time[1:nYears],rowSums(HarvLimMaak[1:nYears,])/totArea*1000,col="red")
      
      if(TRUE){
        
        # NBE
        ij <- which(colnames(outresults)=="NBE")
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("NBE_",sortVarnams),colnames(outresults)))
        ymax <- max(0,max(outresults[,..ij2]))
        ymin <- min(0,min(outresults[,..ij2]))
        plot(time, tmp, type="l",main=paste("Region",r_no,rname), 
             xlim = c(time[1]-1,time[length(time)]),
             ylab="NBE, kg CO2eq/ha", ylim = c(ymin,ymax),
             lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- ij2[1 + ik]
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(time, tmp,col=colorsi[ik])
          }
        }
        lines(c(time[1],time[length(time)]),c(0,0),col="black")
        
        # NBEsum
        ij <- which(colnames(outresults)=="NBEsum")
        tmp <- unlist(outresults[,..ij])
        ij2 <- c(ij,match(paste0("NBEsum_",sortVarnams),colnames(outresults)))
        ymax <- max(0,max(outresults[,..ij2]))
        ymin <- min(0,min(outresults[,..ij2]))
        plot(time, tmp/1e6, type="l",main=paste("Region",r_no,rname), 
             xlim = c(time[1]-1,time[length(time)]),
             ylab="NBEsum, million kg CO2eq", ylim = c(ymin,ymax)/1e6,
             lwd=3)
        colorsi <- c("blue","green","pink")
        for(ik in 1:length(sortVarnams)){
          ijk <- ij2[1 + ik]
          tmp <- unlist(outresults[,..ijk])
          if(length(tmp)>1){
            lines(time, tmp/1e6,col=colorsi[ik])
          }
        }
        lines(c(time[1],time[length(time)]),c(0,0),col="black")
        
      }
    }
  }

  if(toFile) save(outresults, areatable, 
                  file = paste0(outDir,"results_agesample",samplaus,"compHarv",compHarvX,"ageHarvPrior",ageHarvPriorX,"_rno",r_noi,".rdata"))  
  if(fmi_from_allas){
    file.remove(paste0(workdir,fmi_vars_PREBAS_file))
    file.remove(paste0(workdir,climID_lookup_file))
  }
  rm(list=setdiff(ls(), toMem))
  gc()
  
}

r_noi <- 1
outresults_wholecountry <- areatable_wholecountry <- data.frame()
for(r_noi in 1:length(rids)){
  toMem <- ls()
  set.seed(1)
  r_no <- rids[r_noi]
  rname <- regionNames[r_no]
  rname_fi <- regionNames_fi[r_no]
  rnameid <- r_nos[r_no]
  load(file = paste0(outDir,"results_agesample",samplaus,"compHarv",compHarvX,"ageHarvPrior",ageHarvPriorX,"_rno",r_noi,".rdata"))  

  ij <- which(grepl("NBEsum",colnames(outresults)))
  cnames <- colnames(outresults)[ij]
  if(r_noi==1){
    outresults_wholecountry <- outresults[,..ij]
    areatable_wholecountry <- data.table(r_no, rname, areatable)
  } else {
    outresults_wholecountry <- outresults_wholecountry + outresults[,..ij]
    areatable_wholecountry <- rbind(areatable_wholecountry,
                                    data.table(r_no, rname, areatable))
  }
  rm(list=setdiff(ls(), toMem))
  gc()
  
}    

if(toFile) save(outresults_wholecountry, areatable_wholecountry,
                file = paste0(outDir,"results_agesample",samplaus,"compHarv",compHarvX,"ageHarvPrior",ageHarvPriorX,"_wholeCountry.rdata"))  
par(mfrow=c(3,1))
sortid <- 1
cnames <- colnames(outresults_wholecountry)
timei <- 2015+1:nrow(outresults_wholecountry)
for(sortid in 1:3){
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
if(toFile) dev.off()
