library(dplyr)
library(ggplot2)
library(rlang)
library(RColorBrewer)
library(kableExtra)

levels_country <- c("Belgium","Germany", "Denmark", "France", "Ireland", "Netherlands", "Norway", "Sweden", "UK", "Channel Is.- Guernsey", "Channel Is.- Jersey", "Isle of Man", "UK (England)", "UK(Scotland)", "UK(Northern Ireland)")
labels_country <- c("BE", "DE", "DK" , "FR", "IE", "NL", "NO", "SE", "UK","GG" , "JE", "IM","UK","UK", "UK")
levels_country_ol <- c("BE", "DE", "DK", "FR", "IE", "NL", "NO", "SE", "UK", "CI", "GG", "IM", "JE")
labels_country_ol <- c("BE", "DE", "DK", "FR", "IE", "NL", "NO", "SE", "UK", "UK", "UK", "UK", "UK")
levels_gear <- c("OTB", "SDN", "TBB", "GNS", "GTR", "LLS", "MIS", "SSC", "FPO", "DRB", "LHP", "OTM")
labels_gear <-  c("Otter and Seine", "Otter and Seine", "Beam", "Trammel/gillnets", "Trammel/gillnets", "Other", "Other", "Otter and Seine", "Other", "Other", "Other", "Other")
levels_area <- c("27.3.a", "27.3.a.21", "27.3.a.20", "27.4" , "27.4.a", "27.4.b", "27.4.c","27.7.d", "27.7.e")
labels_area <-  c("27.3.a", "27.3.a", "27.3.a", "27.4" , "27.4", "27.4", "27.4","27.7.de", "27.7.de")

## Functions
split_to_numbers <- function(x) as.numeric(strsplit(gsub(",", "", x), '[[:blank:]]{1,}')[[1]])


NAorCode <- function(x) {
  res <- unique(na.omit(x))
  if(length(res) == 0) return(NA_character_)
  res
}

# Normalize the units by dividing each value by its mean
normalize <- function(x) {
  return(x / mean(x))
}

## Helper functions ----
vsum <- function(x, y) {
  sum(x, y, na.rm = TRUE )
}

getBMS <- function(fn) {
  res <- read.csv(fn,
                  stringsAsFactors = FALSE, header = TRUE) %>% as_tibble() %>%
    filter(Species.Latin.Name %in% c("Scophthalmus rhombus"),
           Area %in% c("27_3_A", "27_3_A_20", "27_3_A_21", "27_4_A" , "27_4_B", "27_4_C", "27_7_D", "27_7_E")) %>%
    mutate(Country = ifelse(Country == "GB", "UK", Country)) %>%
    group_by(Year, Country, Area)
  if ("AMS_Catch" %in% names(res)) {
    res$AMS.Catch.TLW. <- res$AMS_Catch
    suppressWarnings(res$BMS.Catch.TLW. <- as.numeric(res$BMS_Catch))
  }
  res %>% summarise(BMS = sum(BMS.Catch.TLW., na.rm = TRUE))
}

get_catchcat_percent <- function(dat) {
  perarea <- dat %>%
    filter(!CatchCategory  %in% c("Logbook Registered Discard", "BMS landing")) %>%
    group_by(CatchCategory , Area, Country) %>%
    summarise(catchpercatchcat = sum(Caton), .groups = "drop") %>%
    ungroup() %>%
    reshape2::dcast(Area + Country ~ CatchCategory, value.var = "catchpercatchcat") %>%
    mutate(Total = Discards + Landings,
           `Discard ratio` = ifelse(Total > 0, Discards / (Landings + Discards) * 100, 0),
           DLratio = ifelse(Total > 0, Discards / Landings, 0),
           Year = first(dat$Year),
           Area = case_when(Area %in% c("27.3.a", "27.3.a.21", "27.3.a.20")~ "27.3.a",
                            Area %in% c("27.4" , "27.4.a", "27.4.b", "27.4.c")~ "27.4",
                            Area %in% c("27.7.d", "27.7.e")~ "27.7.de"),
           Country = ifelse(Country %in% levels, Country, "Other"),
           Country = factor(Country, levels = levels, labels = labels, ordered = TRUE))
  total <- perarea %>%
    summarise(Area = "Total",
              Discards = sum(Discards, na.rm = TRUE),
              Landings = sum(Landings, na.rm = TRUE),
              Total = sum(Total, na.rm = TRUE),
              Year = first(Year),
              .groups = "drop") %>%
    mutate(#`Raising factor` = Discards / Landings * 100,
      DLratio = ifelse(Total > 0, Discards / Landings, 0),
      `Discard ratio` = Discards / (Landings + Discards) * 100)
  bind_rows(perarea, total)
}


#' Reads ICES preliminary catch statistics and returns only
#'
#' @param fn file name, it should contain the
#' @param latin string or vector, latin names of species to select
#' @param areas vector with ICES areas, e.g. "27_3_A_20"
#' @param areaout string, the name of the combined area
#' @param speciesout string, species name in the returned data.frame

#' @return data.frame of selected species by country
#' @export


readPrel <- function(fn,
                     latin = c("Scophthalmus rhombus"),
                     areas = c("27_3_A", "27_3_A_20", "27_3_A_21",
                               "27_4_A" , "27_4_B", "27_4_C" ,
                               "27_7_D", "27_7_E"),
                     speciesout = "BLL") {
  res <- read.csv(fn, stringsAsFactors = FALSE, header = TRUE) %>% as_tibble()
  yr <- unique(res$Year)
  stopifnot(length(yr) == 1)
  if ("AMS_Catch" %in% names(res)) {
    res$AMS.Catch.TLW. <- res$AMS_Catch
    suppressWarnings(res$BMS.Catch.TLW. <- as.numeric(res$BMS_Catch))
  }
  res %>%
    filter(Species.Latin.Name %in% latin,
           Area %in% areas) %>%
    mutate(Country = ifelse(Country == "GB", "UK", Country)) %>%
    group_by(Year, Country) %>%
    mutate(Area = case_when(Area %in% c("27_3_A", "27_3_A_20", "27_3_A_21")~ "27.3.a",
                            Area %in% c("27_4_A" , "27_4_B", "27_4_C")~ "27.4",
                            Area %in% c("27_7_D", "27_7_E")~ "27.7.de"))%>%
    group_by(Species.Latin.Name,Area, Year, Country )%>%
    summarise(Landings = vsum(AMS.Catch.TLW., BMS.Catch.TLW.),
              Units = "TLW",
              Species = speciesout)%>%
    ungroup() %>%
    select(Species,Area,Units,Country, Year,Landings)
}


assign_percent_to_unallocated_landings <- function(s) {
  s %>%
    filter(CatchCategory  != "Discards", geargroup != "MIS") %>%
    group_by(group) %>%
    mutate(weight = ifelse(count == 2, Caton, NA)) %>%
    mutate(percraise = ifelse(count == 1,
                              weighted.mean(ifelse(count == 2, perc, NA), ifelse(count == 2, weight, NA), na.rm = TRUE),
                              ##weighted.mean(perc, Caton, na.rm = TRUE),
                              NA)) %>%
    mutate(percraise_includes = ifelse(count == 1,
                              paste0(perc, collapse = ", "),
                              NA),
           percraise_weights = ifelse(count == 1,
                                       paste0(weight, collapse = ", "),
                                       NA)) %>%
    ungroup()
}

get_gear_percent <- function(dat) {
  dat %>% mutate(gearcat = substr(Fleets, 1, 3)) %>%
    group_by(gearcat) %>%
    summarise(catchpergear = sum(Caton)) %>%
    ungroup() %>%
    mutate(perc=catchpergear / sum(catchpergear)*100,
           year = first(dat$Year))
}

get_gear_fine_percent <- function(dat) {
  dat %>% mutate(gearcat = substr(Fleets, 1, 7)) %>%
    group_by(gearcat) %>%
    summarise(catchpergear = sum(Caton)) %>%
    ungroup() %>%
    mutate(perc=catchpergear / sum(catchpergear)*100,
           year = first(dat$Year))
}

readPrel <- function(fn,
                     latin = c("Scophthalmus rhombus"),
                     areas = c("27_3_A", "27_3_A_20", "27_3_A_21",
                               "27_4_A" , "27_4_B", "27_4_C" ,
                               "27_7_D", "27_7_E"),
                     speciesout = "BLL") {
  res <- read.csv(fn, stringsAsFactors = FALSE, header = TRUE) %>% as_tibble()
  yr <- unique(res$Year)
  stopifnot(length(yr) == 1)
  if ("AMS_Catch" %in% names(res)) {
    res$AMS.Catch.TLW. <- res$AMS_Catch
    suppressWarnings(res$BMS.Catch.TLW. <- as.numeric(res$BMS_Catch))
  }
  res %>%
    filter(Species.Latin.Name %in% latin,
           Area %in% areas) %>%
    mutate(Country = ifelse(Country == "GB", "UK", Country)) %>%
    group_by(Year, Country) %>%
    mutate(Area = case_when(Area %in% c("27_3_A", "27_3_A_20", "27_3_A_21")~ "27.3.a",
                            Area %in% c("27_4_A" , "27_4_B", "27_4_C")~ "27.4",
                            Area %in% c("27_7_D", "27_7_E")~ "27.7.de"))%>%
    group_by(Species.Latin.Name,Area, Year, Country )%>%
    summarise(Landings = vsum(AMS.Catch.TLW., BMS.Catch.TLW.),
              Units = "TLW",
              Species = speciesout)%>%
    ungroup() %>%
    select(Species,Area,Units,Country, Year,Landings)
}

getBMS <- function(fn) {
  res <- read.csv(fn,
                  stringsAsFactors = FALSE, header = TRUE) %>% as_tibble() %>%
    filter(Species.Latin.Name %in% c("Scophthalmus rhombus"),
           Area %in% c("27_3_A", "27_3_A_20", "27_3_A_21", "27_4_A" , "27_4_B", "27_4_C", "27_7_D", "27_7_E")) %>%
    mutate(Country = ifelse(Country == "GB", "UK", Country)) %>%
    group_by(Year, Country, Area)
  if ("AMS_Catch" %in% names(res)) {
    res$AMS.Catch.TLW. <- res$AMS_Catch
    suppressWarnings(res$BMS.Catch.TLW. <- as.numeric(res$BMS_Catch))
  }
  res %>% summarise(BMS = sum(BMS.Catch.TLW., na.rm = TRUE))
}

vsum <- function(x, y) {
  sum(x, y, na.rm = TRUE )
}

  get_catchcat_percent <- function(dat) {
      perarea <- dat %>%
      filter(!CatchCategory  %in% c("Logbook Registered Discard", "BMS landing")) %>%
      group_by(CatchCategory , Area, Country) %>%
      summarise(catchpercatchcat = sum(Caton), .groups = "drop") %>%
      ungroup() %>%
      reshape2::dcast(Area + Country ~ CatchCategory, value.var = "catchpercatchcat") %>%
      mutate(Total = Discards + Landings,
             `Discard ratio` = ifelse(Total > 0, Discards / (Landings + Discards) * 100, 0),
             DLratio = ifelse(Total > 0, Discards / Landings, 0),
             Year = first(dat$Year),
             Area = case_when(Area %in% c("27.3.a", "27.3.a.21", "27.3.a.20")~ "27.3.a",
                              Area %in% c("27.4" , "27.4.a", "27.4.b", "27.4.c")~ "27.4",
                              Area %in% c("27.7.d", "27.7.e")~ "27.7.de"),
             Country = ifelse(Country %in% levels, Country, "Other"),
             Country = factor(Country, levels = levels, labels = labels, ordered = TRUE))
    total <- perarea %>%
      summarise(Area = "Total",
                Discards = sum(Discards, na.rm = TRUE),
                Landings = sum(Landings, na.rm = TRUE),
                Total = sum(Total, na.rm = TRUE),
                Year = first(Year),
                .groups = "drop") %>%
      mutate(#`Raising factor` = Discards / Landings * 100,
        DLratio = ifelse(Total > 0, Discards / Landings, 0),
        `Discard ratio` = Discards / (Landings + Discards) * 100)
    bind_rows(perarea, total)
  }

##### From Intercatch_Output_Analysis_Functions.r ####
readStockOverview <- function(StockOverviewFile,NumbersAtAgeLengthFile){
  Wdata <- read.table(StockOverviewFile,header=TRUE,sep="\t")
  names(Wdata)[7] <- "Fleet"
  names(Wdata)[10] <- "CatchWt"
  names(Wdata)[11] <- "CatchCat"
  names(Wdata)[12] <- "ReportCat"

  temp <- sum(Wdata$CatchWt[Wdata$CatchCat=="Logbook Registered Discard"])
  print(paste("The sum of catches from Logbook Registered Discard is ", temp, " kg. They are excluded from the summary", sep=""))
  temp <- sum(Wdata$CatchWt[Wdata$CatchCat=="BMS landing"])
  print(paste("The sum of catches from BMS landing is ", temp, " kg. They are re-categorized as Discards", sep=""))
  Wdata <- Wdata[Wdata$CatchCat!="Logbook Registered Discard",]
  Wdata$CatchCat[Wdata$CatchCat=="BMS landing"] <- "Discards"
  Wdata$CatchCat <- as.character(Wdata$CatchCat)

  Wdata$CatchCat <- substr(Wdata$CatchCat,1,1)
  Wdata <- Wdata[,-ncol(Wdata)]

  Ndata <- read.table(NumbersAtAgeLengthFile,header=TRUE,sep="\t",skip=0)
  names(Ndata)[7] <- "CatchCat"
  names(Ndata)[9] <- "Fleet"

  Wdata <- merge(Wdata,Ndata[,c(3,4,5,7,9,10,11)],by=c("Area","Season","Fleet","Country","CatchCat"),all.x=TRUE)
  Wdata$Sampled <- ifelse(is.na(Wdata$SampledCatch),FALSE,TRUE)

  return(Wdata)
}

plotStockOverview <- function(dat,plotType="LandPercent",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,byArea=FALSE,countryColours=NULL,set.mar=TRUE,markSampled=TRUE,individualTotals=TRUE,ymax=NULL,fcex.names=0.7){

  plotTypes <- c("LandWt","LandPercent","CatchWt","DisWt","DisRatio","DiscProvided")
  if (!(plotType %in% plotTypes)) stop(paste("PlotType needs to be one of the following:",paste(plotTypes)))

  stock <- dat$Stock[1]

  impLand <- dat[dat$CatchCat=="L",]
  impDis <- dat[dat$CatchCat=="D",]

  nArea <- nSeason <- nCountry <- nFleet <- 1

  SeasonNames <- sort(unique(impLand$Season))
  AreaNames <- sort(unique(impLand$Area))

  countryLegend <- FALSE

  if (byFleet) nFleet <- length(unique(c(impLand$Fleet, impDis$Fleet))) ## YV
  if (byCountry) {
    nCountry <- length(unique(c(impLand$Country, impDis$Country))) # YV
    countryLegend <- TRUE
  }
  if (byArea) nArea <- length(AreaNames)
  if (bySeason) nSeason <- length(SeasonNames)
  if (!bySampled) markSampled <- FALSE


  if (length(countryColours)==1 &&countryColours){
    #countryColours <- data.frame(
    #"Country"=c("Belgium","Denmark","France","Germany","Netherlands","Norway","Poland","Sweden","UK (England)","UK(Scotland)"),
    #"Colour"=c("green", "red", "darkblue", "black", "orange","turquoise" ,"purple","yellow","magenta","blue")
    #, stringsAsFactors=FALSE)
    
    cols <- colorRampPalette(brewer.pal(12, "Paired"))

    countryColours <- data.frame(
      "Country" = unique(dat$Country)[order(unique(dat$Country))],
      "Colour" = cols(length(unique(dat$Country))),
      stringsAsFactors = FALSE
    )
    
  }
  if (length(countryColours)==1 && countryColours==FALSE){
    countryLegend <- FALSE
    #countryColours <- data.frame(
    #"Country"=c("Belgium","Denmark","France","Germany","Norway","Netherlands","Poland","Sweden","UK (England)","UK(Scotland)")
    # , stringsAsFactors=FALSE)
    countryColours <- data.frame("Country"=unique(dat$Country)[order(unique(dat$Country))],
                                 stringsAsFactors=FALSE)
    countryColours$Colour <- rep("grey",length(countryColours$Country))
  }


  LsummaryList <- list()
  DsummaryList <- list()
  summaryNames <- NULL
  i <- 1
  if (byFleet) {
    LsummaryList[[i]] <- impLand$Fleet
    DsummaryList[[i]] <- impDis$Fleet
    summaryNames <- c(summaryNames,"Fleet")
    i <- i+1
  }
  if(byCountry) {
    LsummaryList[[i]] <- impLand$Country
    DsummaryList[[i]] <- impDis$Country
    summaryNames <- c(summaryNames,"Country")
    i <- i+1
  }
  if(bySeason) {
    LsummaryList[[i]] <- impLand$Season
    DsummaryList[[i]] <- impDis$Season
    summaryNames <- c(summaryNames,"Season")
    i <- i+1
  }
  if (byArea) {
    LsummaryList[[i]] <- impLand$Area
    DsummaryList[[i]] <- impDis$Area
    summaryNames <- c(summaryNames,"Area")
    i <- i+1
  }
  if (bySampled) {
    LsummaryList[[i]] <- impLand$Sampled
    DsummaryList[[i]] <- impDis$Sampled
    summaryNames <- c(summaryNames,"Sampled")
    i <- i+1
  }
  byNames <- summaryNames
  summaryNames <- c(summaryNames,"CatchWt")

  ### YV changed
  if(plotType%in%c("LandWt","LandPercent")){
    Summary <- aggregate(impLand$CatchWt,LsummaryList,sum)
  } else if (plotType=="DisWt"){
    Summary <- aggregate(impDis$CatchWt,DsummaryList,sum)
  } else if (plotType=="DisRatio"){
    SummaryD <- aggregate(impDis$CatchWt,DsummaryList,sum)
    SummaryL <- aggregate(impLand$CatchWt,LsummaryList,sum)
    if(bySampled){
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD)) - 1]
    } else {
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]
    }
    Summary <- merge(SummaryD, SummaryL, all=T, by=testN)
    Summary$DRatio <- Summary$x.x / (Summary$x.x+Summary$x.y)
    Summary <- Summary[!is.na(Summary$DRatio),c(testN,paste('Group.',length(testN)+1,'.x', sep=''),paste('Group.',length(testN)+1,'.y', sep=''),'DRatio')]
  } else if (plotType=="DiscProvided"){
    if(bySampled){
      DsummaryList <- DsummaryList[1: (length(DsummaryList)- 1)]
      LsummaryList <- LsummaryList[1: (length(LsummaryList)- 1)]
    } else {
      DsummaryList <- DsummaryList[ length(DsummaryList)]
      LsummaryList <- LsummaryList[ length(LsummaryList)]
    }
    SummaryD <- aggregate(impDis$CatchWt,DsummaryList,sum)
    SummaryL <- aggregate(impLand$CatchWt,LsummaryList,sum)
    if(bySampled){
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]
    } else {
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]
    }
    Summary <- merge(SummaryD, SummaryL, all=T, by=testN)
    Summary$DRatio <- Summary$x.x / (Summary$x.x+Summary$x.y)
    Summary <- Summary[,c(testN,'DRatio','x.y')]
    Summary$DRatio[!is.na(Summary$DRatio)] <- TRUE
    Summary$DRatio[is.na(Summary$DRatio)] <- FALSE
    Summary <- unique(Summary)
    ProvidedDiscards <<- Summary

  }
  ### end YV changed


  #disSummary <- aggregate(impDis$CatchWt,DsummaryList,sum) YV
  if (plotType!="DisRatio") {
    names(Summary) <- summaryNames #YV
  } else {
    names(Summary) <- c(summaryNames[-c(grep(c('Sampled'),summaryNames), grep(c('CatchWt'),summaryNames))],
                        "SampledD", "SampledL", "DRatio")
  }
  #names(disSummary) <- summaryNames # yv
  #names(disSummary) <- c("Fleet" ,  "Country", "Area" ,   "SampledD" ,"DisWt") # yv
  #names(landSummary) <- c("Fleet" ,  "Country", "Area" ,   "SampledL" ,"LandWt") # yv


  #stratumSummary <- merge(landSummary,disSummary,by=byNames,all=TRUE) #YV
  stratumSummary <- Summary #YV
  if(plotType%in%c("LandWt","LandPercent","DiscProvided")){
    names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "LandWt" # YV
  } else if (plotType=="DisWt"){
    names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "DisWt" # YV
  }

  #names(stratumSummary)[names(stratumSummary)=="CatchWt.y"] <- "DisWt"  # YV

  #if (bySampled ) {
  ##  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$LandWt)),
  #	if(plotType%in%c("LandWt","LandPercent")){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$LandWt)),]
  #	} else if (plotType=="DisWt"){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$DisWt)),]
  #	}
  #} else {
  #	if(plotType%in%c("LandWt","LandPercent")){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$LandWt)),]
  #	} else if (plotType=="DisWt"){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$DisWt)),]
  #	}
  #}
  if (bySampled ) {
    if(plotType!="DisRatio"){
      stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary[,dim(stratumSummary)[2]])),]
    } else {
      stratumSummary <- stratumSummary[rev(order(stratumSummary$SampledL,stratumSummary$SampledD,stratumSummary[,dim(stratumSummary)[2]])),]
    }
  } else {
    stratumSummary <- stratumSummary[rev(order(stratumSummary[,dim(stratumSummary)[2]])),]
  }

  #catchData <- matrix(c(stratumSummary$LandWt,stratumSummary$DisWt),byrow=TRUE,nrow=2) YV
  catchData <- stratumSummary[,dim(stratumSummary)[2]] #YV


  if (set.mar) par(mar=c(8,4,1,1)+0.1)

  for (a in 1:nArea) {
    #windows()
    if (bySeason & !(byCountry | byFleet)) nSeason <- 1
    for (s in 1:nSeason) {
      area <- AreaNames[a]
      season <- SeasonNames[s]

      indx <- 1:nrow(stratumSummary)
      if (bySeason & !byArea & (byCountry | byFleet)) indx <- stratumSummary$Season==season
      if (!bySeason & byArea) indx <- stratumSummary$Area==area
      if (bySeason & byArea & (byCountry | byFleet | bySampled)) indx <- stratumSummary$Area==area & stratumSummary$Season==season

      if (individualTotals) {
        sumLandWt <- sum(stratumSummary$LandWt[indx],na.rm=TRUE)
      } else {
        sumLandWt <- sum(stratumSummary$LandWt,na.rm=TRUE)
      }
      if(byCountry) {
        colVec <- countryColours$Colour[match(stratumSummary$Country[indx],countryColours$Country)]
      } else {
        colVec <- "grey"
      }

      if (plotType%in%c("LandWt","DiscProvided")) yvals <- stratumSummary$LandWt[indx]
      if (plotType=="LandPercent") yvals <- 100*stratumSummary$LandWt[indx]/sumLandWt
      if (plotType=="DisWt") yvals <- stratumSummary$DisWt[indx]
      if (plotType=="CatchWt") yvals <- catchData[,indx]
      if (plotType=="DisRatio") yvals <- stratumSummary$DRatio

      if(plotType!="DisRatio"){

        if (!is.null(ymax)) newYmax <- ymax
        if (is.null(ymax)) newYmax <- max(yvals,na.rm=TRUE)
        if (is.null(ymax) & plotType=="LandPercent") newYmax <- max(cumsum(yvals),na.rm=TRUE)
        #if (is.null(ymax) & plotType=="CatchWt") newYmax <- max(colSums(yvals),na.rm=TRUE) #YV
        #if (plotType=="CatchWt") colVec <- c("grey","black") #YV
        #if (plotType=="CatchWt") countryLegend <- FALSE #YV
        if (markSampled) newYmax <- 1.06*newYmax
        if (byFleet) namesVec=stratumSummary$Fleet[indx]
        if (!byFleet) {
          if (byArea & bySeason) namesVec=paste(stratumSummary$Area[indx],stratumSummary$Season[indx])
          if (!byCountry & !byArea & bySeason) namesVec=paste(stratumSummary$Season[indx])
          if (byCountry) namesVec=paste(stratumSummary$Country[indx])
          if (bySampled & !byCountry & nArea==1 & nSeason==1) namesVec=paste(stratumSummary$Season[indx])
        }


        cumulativeY <- cumsum(yvals)
        yvals[yvals>newYmax] <- newYmax

        if ((newYmax==-Inf)) {
          plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
          box()
        } else {
          b <- barplot(yvals,names=namesVec,las=2,cex.names=fcex.names,col=colVec,ylim=c(0,newYmax),yaxs="i")

          if (bySampled & markSampled) {
            nSampled <- sum(stratumSummary$Sampled[indx])
            if(nSampled>0){
              arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[nSampled]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1)
              arrows(b[nSampled]+(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1)
              if(plotType!="DiscProvided"){
                text((b[nSampled]+b[1])/2,newYmax*104/106,"sampled",cex=0.8)
                text((b[length(b)]+b[nSampled])/2+(b[2]-b[1])/2,newYmax*104/106,"unsampled",cex=0.8)
              }else{
                text((b[nSampled]+b[1])/2,newYmax*104/106,"Landings with Discards",cex=0.8)
                text((b[length(b)]+b[nSampled])/2+(b[2]-b[1])/2,newYmax*104/106,"no Discards",cex=0.8)
              }
            } else {
              arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1)
              if(plotType!="DiscProvided"){
                text(b[length(b)]+(b[2]-b[1])/4,newYmax*104/106,"unsampled",cex=0.8)
              }else{
                text(b[length(b)]+(b[2]-b[1])/4,newYmax*104/106,"no Discards",cex=0.8)
              }
            }
          }
          if (countryLegend) legend("topright",inset=0.05,legend=countryColours$Country,col=countryColours$Colour,pch=15)
          box()
          if (plotType=="LandPercent") lines(b-(b[2]-b[1])/2,cumulativeY,type="s")
          if (plotType=="LandPercent") abline(h=c(5,1),col="grey",lty=1)
          if (plotType=="LandPercent") abline(h=c(90,95,99),col="grey",lty=1)
          if (plotType=="LandPercent") abline(h=100)
        }
        if (!bySeason & !byArea) title.txt <- paste(stock)
        if (!bySeason & byArea) title.txt <- paste(stock,area)
        if (bySeason & !byArea & !(byCountry | byFleet | bySampled)) title.txt <- paste(stock)
        if (bySeason & !byArea & (byCountry | byFleet | bySampled)) title.txt <- paste(stock,season)
        if (bySeason & byArea) title.txt <- paste(stock,area,season)
        title.txt <- paste(title.txt,plotType)
        title(title.txt)
      } else {
        par(mfrow=c(2,2))
        listSample <- unique(paste(stratumSummary$SampledD,stratumSummary$SampledL))
        for(i in 1:length(listSample)){
          idx <- which(stratumSummary$SampledD[indx]==strsplit(listSample[i],' ')[[1]][1] & stratumSummary$SampledL[indx]==strsplit(listSample[i],' ')[[1]][2])
          if(length(idx)>0){
            if(byCountry) {
              
            colVec <- countryColours$Colour[match(stratumSummary$Country[indx][idx],countryColours$Country)]
              
            } else {
              colVec <- "grey"
            }

            if (!is.null(ymax)) newYmax <- ymax
            if (is.null(ymax)) newYmax <- max(yvals,na.rm=TRUE)
            if (is.null(ymax) & plotType=="LandPercent") newYmax <- max(cumsum(yvals),na.rm=TRUE)
            #if (is.null(ymax) & plotType=="CatchWt") newYmax <- max(colSums(yvals),na.rm=TRUE) #YV
            #if (plotType=="CatchWt") colVec <- c("grey","black") #YV
            #if (plotType=="CatchWt") countryLegend <- FALSE #YV
            if (markSampled) newYmax <- 1.06*newYmax
            if (byFleet) namesVec=stratumSummary$Fleet[indx][idx]
            if (!byFleet) {
              if (byArea & bySeason) namesVec=paste(stratumSummary$Area[idx],stratumSummary$Season[indx][idx])
              if (!byCountry & !byArea & bySeason) namesVec=paste(stratumSummary$Season[indx][idx])
              if (byCountry) namesVec=paste(stratumSummary$Country[indx][idx])
              if (bySampled & !byCountry & nArea==1 & nSeason==1) namesVec=paste(stratumSummary$Season[indx][idx])
            }


            cumulativeY <- cumsum(yvals[indx][idx])
            yvals[yvals>newYmax] <- newYmax

            if ((newYmax==-Inf)) {
              plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
              box()
            } else {
              b <- barplot(yvals[indx][idx],names=namesVec,las=2,cex.names=fcex.names,col=colVec,ylim=c(0,newYmax),yaxs="i")

              #if (bySampled & markSampled) {
              #  nSampledD <- sum(stratumSummary$SampledD[idx]==T)
              #  nSampledL <- sum(stratumSummary$SampledL[idx]==T)
              #  if (nSampledD>0) arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[nSampledD]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1)
              #  if (nSampledD>0) arrows(b[nSampledD]+(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1)

              #  if (nSampledL>0) arrows(b[1]-(b[2]-b[1])/2,newYmax*92/106,b[nSampledL]+(b[2]-b[1])/2,newYmax*92/106,code=3,length=0.1)
              #  if (nSampledL>0) arrows(b[nSampledL]+(b[2]-b[1])/2,newYmax*92/106,b[length(b)]+(b[2]-b[1])/2,newYmax*92/106,code=3,length=0.1)

              #  }
              if (countryLegend) legend("topright",inset=0.05,legend=countryColours$Country,col=countryColours$Colour,pch=15)
              box()
              if (plotType=="LandPercent") lines(b-(b[2]-b[1])/2,cumulativeY,type="s")
              if (plotType=="LandPercent") abline(h=c(5,1),col="grey",lty=1)
              if (plotType=="LandPercent") abline(h=c(90,95,99),col="grey",lty=1)
              if (plotType=="LandPercent") abline(h=100)
            }
            if (!bySeason & !byArea) title.txt <- paste(stock)
            if (!bySeason & byArea) title.txt <- paste(stock,area)
            if (bySeason & !byArea & !(byCountry | byFleet | bySampled)) title.txt <- paste(stock)
            if (bySeason & !byArea & (byCountry | byFleet | bySampled)) title.txt <- paste(stock,season)
            if (bySeason & byArea) title.txt <- paste(stock,area,season)
            title.txt <- paste(title.txt,plotType, "D/L", listSample[i])
            title(title.txt)
          }
        }
      }
    }
  }
}

# Define a function to calculate Mohn's rho
extract_retro_semester <- function(retroindex, semester) {
  # Initialize the list
  extract_retro <- list()
  extract_retro$Year <- retroindex[[1]][[semester]]$timeI
  extract_retro$base <- retroindex[[1]][[semester]]$ObsI
  
  # Loop through the retroindexd list to extract values and create columns
  for (i in 2:length(retroindex)) {
    col_name <- paste0("peel", i - 1)
    if (semester == 1) {
      extract_retro[[col_name]] <- c(retroindex[[i]][[1]]$ObsI, rep_len(NA, i - 1))
    } else if (semester == 2) {
      extract_retro[[col_name]] <- c(retroindex[[i]][[2]]$ObsI, rep_len(NA, i - 1))
    } else {
      stop("Invalid semester index. Must be 1 or 2.")
    }
  }
  # Convert to data frame
  extract_retro <- as.data.frame(extract_retro)
  # Set row names
  rownames(extract_retro) <- extract_retro$Year
  # Remove Year column
  extract_retro <- extract_retro[, -1]
  return(extract_retro)
}

################## FUnction to get the % of landings with discard #############
CatchAndSampleDataTables <- "../boot/data/CatchAndSampleDataTables.txt"
get_perc_land_with_disc <- function(CatchAndSampleDataTables) {
  # Read the data from the file
  test <- scan(CatchAndSampleDataTables, what = 'character', sep = '\t')
  table2 <- test[(which(test == "TABLE 2.") + 3):length(test)]
  tmp <- table2[-c(1:56)]			  
  table2_bis <- data.frame(matrix(tmp, ncol = 27, byrow = TRUE), stringsAsFactors = FALSE)
  colnames(table2_bis) <- table2[1:27]
  table2_bis <- data.table(table2_bis)
  table2_bis <- table2_bis[, CATON := as.numeric(as.character(CATON))]
  table2_bis <- table2_bis[, CATON := CATON / 1000]
  table2_bis <- table2_bis[, CANUM := as.numeric(as.character(CANUM))]
  table2_bis <- table2_bis[, WECA := as.numeric(as.character(WECA))]
  table2_bis <- table2_bis[, AgeOrLength := as.numeric(as.character(AgeOrLength))]
  table2_bis <- table2_bis[, Area := as.factor(Area)]
  table2_bis <- table2_bis[, Fleet := factor(Fleet)]
  table2_bis <- table2_bis[, Season := factor(Season)]
  table2_bis <- table2_bis[, Country := factor(Country)]
  table2_bis <- table2_bis[, id := paste(Stock, Country, Area, Season, Fleet)]
  table2_bis[Area == "IIIaN                                                       ", 'Area'] <- "IIIaN"
  table2_bis$CatchCategory <- factor(table2_bis$CatchCategory)
  
  colnames(table2_bis)[colnames(table2_bis) == 'CATONRaisedOrImported'] <- 'RaisedOrImported'
  
  # Modify the table2_ter
  table2_ter <- table2_bis[, -c("Sex", "AgeOrLength", "CANUM", "WECA", "LECA")]
  table2_ter <- unique(table2_ter)
  
  # Calculate landings with associated discards percentage
  landingsWithAssociatedDiscards <- table2_ter[, .(CATON = sum(CATON)), by = .(id, RaisedOrImported, CatchCategory)]
  tmp <- landingsWithAssociatedDiscards[(RaisedOrImported == "Imported_Data" & CatchCategory == "Discards"), id]
  landingsWithAssociatedDiscards <- landingsWithAssociatedDiscards[id %in% tmp & CatchCategory == "Landings", sum(CATON)] / table2_ter[CatchCategory == "Landings", sum(CATON)] * 100
  
  return(landingsWithAssociatedDiscards)
}

###### PLOT SPICT RETRO WITH ZOOM #########
plotspict.retro2 <- function (rep, stamp = get.version(), add.mohn = TRUE, CI = 0.95, xlim = NULL) {
  opar <- par(mfrow = c(2, 2), mar = c(2.5, 3.3, 4, 0.8))
  on.exit(par(opar))
  if (!"spictcls" %in% class(rep)) 
    stop("This function only works with a fitted spict object (class 'spictcls'). Please run `fit.spict` first.")
  if (!"retro" %in% names(rep)) 
    stop("No results of the retro function found. Please run the retrospective analysis using the `retro` function.")
  if (add.mohn) {
    mr <- suppressMessages(mohns_rho(rep, what = c("FFmsy", 
                                                   "BBmsy")))
    mrr <- round(mr, 3)
  }
  nruns <- length(rep$retro)
  bs <- bbs <- fs <- ffs <- time <- conv <- list()
  for (i in 1:nruns) {
    bs[[i]] <- get.par("logB", rep$retro[[i]], exp = TRUE, 
                       CI = CI)[rep$retro[[i]]$inp$indest, 1:3]
    bbs[[i]] <- get.par("logBBmsy", rep$retro[[i]], exp = TRUE, 
                        CI = CI)[rep$retro[[i]]$inp$indest, 1:3]
    fs[[i]] <- get.par("logFnotS", rep$retro[[i]], exp = TRUE, 
                       CI = CI)[rep$retro[[i]]$inp$indest, 1:3]
    ffs[[i]] <- get.par("logFFmsynotS", rep$retro[[i]], 
                        exp = TRUE, CI = CI)[rep$retro[[i]]$inp$indest, 
                                             1:3]
    time[[i]] <- rep$retro[[i]]$inp$time[rep$retro[[i]]$inp$indest]
    conv[[i]] <- rep$retro[[i]]$opt$convergence
  }
  conv <- ifelse(unlist(conv) == 0, TRUE, FALSE)
  sel <- function(x) x[, 2]
  cols <- spict:::cols()
  ylim <- range(0, sapply(bs[conv], sel), na.rm = TRUE) * 
    1.2
  plot(time[[1]], sel(bs[[1]]), type = "n", ylim = ylim, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(B[t]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(bs[[1]][, 1], rev(bs[[1]][, 
                                                                    3])), col = "lightgrey", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(bs[[i]]), col = cols[i], lwd = 2)
    }
  }
  par(lend = 2)
  s <- seq(nruns)
  lbls <- c("All", ifelse(conv[-1], paste0("-", s[-length(s)]), 
                          ""))[conv]
  cls <- cols[s][conv]
  if (sum(conv) <= 10) {
    ncol <- sum(conv)
  }
  else {
    ncol <- ceiling(sum(conv)/2)
    a <- seq(ncol)
    b <- seq(ncol + 1, sum(conv) + sum(conv)%%2)
    w <- unique(c(rbind(a, b)))
    lbls <- lbls[w]
    cls <- cls[w]
  }
  usr <- par("usr")
  xx <- usr[2] + diff(usr[c(1, 2)]) * 0.1
  yy <- mean(usr[4])
  legend(xx, yy, title = "Number of retrospective years", 
         xjust = 0.5, yjust = 0.1, legend = lbls, ncol = ncol, 
         col = cls, lty = 1, seg.len = 1, lwd = 6, x.intersp = 0.5, 
         bg = "transparent", box.lwd = 0, box.lty = 0, xpd = NA)
  par(lend = 1)
  box(lwd = 1.5)
  plot(time[[1]], sel(fs[[1]]), typ = "n", ylim = range(0, 
                                                        sapply(fs[conv], sel), na.rm = TRUE) * 1.2, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(F[t]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(fs[[1]][, 1], rev(fs[[1]][, 
                                                                    3])), col = "lightgrey", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(fs[[i]]), col = cols[i], lwd = 2)
    }
  }
  box(lwd = 1.5)
  par(mar = c(4, 3.3, 2.5, 0.8))
  plot(time[[1]], sel(bbs[[1]]), typ = "n", ylim = range(0, 
                                                         sapply(bbs[conv], sel), na.rm = TRUE) * 1.2, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(B[t]/B[MSY]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(bbs[[1]][, 1], rev(bbs[[1]][, 
                                                                      3])), col = "lightgrey", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(bbs[[i]]), col = cols[i], lwd = 2)
    }
  }
  if (add.mohn) 
    mtext(bquote("Mohn's " * rho[B/B[MSY]] * " = " * .(unname(mrr["BBmsy"]))), 
          3, 0.1)
  box(lwd = 1.5)
  plot(time[[1]], sel(ffs[[1]]), typ = "n", ylim = range(0, 
                                                         sapply(ffs[conv], sel), na.rm = TRUE) * 1.2, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(F[t]/F[MSY]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(ffs[[1]][, 1], rev(ffs[[1]][, 
                                                                      3])), col = "lightgray", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(ffs[[i]]), col = cols[i], lwd = 2)
    }
  }
  if (add.mohn) 
    mtext(bquote("Mohn's " * rho[F/F[MSY]] * " = " * .(unname(mrr["FFmsy"]))), 
          3, 0.1)
  box(lwd = 1.5)
  txt.stamp(stamp, do.flag = TRUE)
  nnotconv <- sum(!conv)
  if (nnotconv > 0) {
    message("Excluded ", nnotconv, " retrospective runs that ", 
            if (nnotconv == 1) 
              "was"
            else "were", " not converged: ", paste(which(!conv) - 
                                                     1, collapse = ", "))
  }
  if (add.mohn) 
    mr
  else invisible(NULL)
}

get_percent_raised <- function(fn) {
  so <- read.delim(fn, stringsAsFactors = FALSE)
  so %>%
    ##filter(CatchCategory  == "Discards") %>%
    group_by(CATONRaisedOrImported ) %>%
    summarise(sum(Caton))
}

get_discard_ratio_per_gear <- function(fn) {
  so <- read.delim(fn, stringsAsFactors = FALSE)
  so %>%
    mutate(gearcat = substr(Fleets, 1, 3)) %>%
    group_by(Area, gearcat, CatchCategory ) %>%
    summarise(Caton = sum(Caton)) %>%
    group_by(gearcat, Area) %>%
    mutate(Percent = Caton / sum(Caton) * 100) %>%
    filter(CatchCategory  == "Discards")
}


# raise_discards <- function(fn) {
#   so <- read.delim(fn, stringsAsFactors = FALSE)
#   s <- 0
#   for (fl in unique(so$Fleets)) {
#     s <- s + nrow(so[which(so$Fleets == fl),])
#   }
# }


get_quarter_percent <- function(dat) {
  dat %>% group_by(Season) %>% summarise(tot = sum(Caton)) %>%
    mutate(perc = tot / sum(tot)*100,
           year = first(dat$Year))
}

get_country_percent <- function(dat) {
  dat %>%
    filter(CatchCategory  == "Landings") %>%
    group_by(Country, Area) %>%
    summarise(tot = sum(Caton), .groups = "drop_last") %>%
    mutate(perc = tot / sum(tot)*100,
           year = first(dat$Year)) %>%
    ungroup() %>%
    transmute(Area, Country, Year = year, Landings = tot, perc)
}

read_noraise_lfq <- function(fn) {
  lfq <- read.table(fn, skip = 2, header = TRUE, sep = "\t")
  summary((lfq))
  lngt_cols <- which(grepl("Lngt", names(lfq)))
  lengths <- inteRcatch::get_numbers(names(lfq[lngt_cols]))
  data.frame(length = lengths, freq = unname(colSums(lfq[lngt_cols])))
}

read_noraise_lfq_split_land_dis_gear <- function(fn) {
  lfq <- read.table(fn, skip = 2, header = TRUE, sep = "\t")
  year <- inteRcatch::get_numbers(fn)
  summary((lfq))
  lngt_cols <- which(grepl("Lngt", names(lfq)))
  splitlfq <- split(lfq, list(lfq$CatchCategory ))
  dd <- mapply(function(x, n) {
    lengths <- inteRcatch::get_numbers(names(lfq[lngt_cols]))
    x <- x %>% mutate(gearcat = substr(Fleets, 1, 3))
    lapply(unique(x$gearcat), function(gc) {
      ttt <- x %>% filter(gearcat == gc)
      data.frame(length = lengths, freq = unname(colSums(ttt[lngt_cols])) , catch_cat = n,
                 year = year, stringsAsFactors = FALSE, gearcat = gc)
    })
  },
  splitlfq, names(splitlfq), SIMPLIFY = FALSE)
  bind_rows(dd$D %>% bind_rows(),
            dd$L %>% bind_rows())
}

read_noraise_lfq_split_land_dis <- function(fn) {
  lfq <- read.table(fn, skip = 2, header = TRUE, sep = "\t")
  year <- inteRcatch::get_numbers(fn)
  lngt_cols <- which(grepl("Lngt", names(lfq)))
  splitlfq <- split(lfq, list(lfq$CatchCategory ))
  dd <- mapply(function(x, n) {
    lengths <- inteRcatch::get_numbers(names(lfq[lngt_cols]))
    data.frame(length = lengths, freq = unname(colSums(x[lngt_cols])) , catch_cat = n,
               year = year, stringsAsFactors = FALSE)
  },
  splitlfq, names(splitlfq), SIMPLIFY = FALSE)
  bind_rows(dd$D %>% bind_rows(),
            dd$L %>% bind_rows())
}

plotQuantity <- function(x, add = FALSE, col = 1, ylim = NULL, xlim = NULL,
                         addUncertainty = FALSE, col.unc = 'lightgrey',
                         what = "logBBmsy", ylab = sub("log", "", what)) {
  bbmsy <- get.par(what, x, exp = TRUE)
  idx <- x$inp$indest
  time <- x$inp$time[idx]
  if ( ! add) {
    plot(time, bbmsy[idx, 2], col = col, lwd = 2, type = "n", 
         ylim = ylim, xlim = xlim,
         xlab = "", ylab = "")
    title(xlab = "Year", ylab = ylab)
  }
  if (addUncertainty) {
    polygon(c(time, rev(time)), 
            c(bbmsy[idx, 1], rev(bbmsy[idx, 3])), 
            col = col.unc, border = 'darkgrey')
  }
  lines(time, bbmsy[idx, 2], col = col, lwd = 2, type = "l", ylim = ylim)
}
getFractileSeries <- function(fit, what = c("BBmsy", "FFmsy"), fractiles = c(0.35, 0.65), when = last) {
  stopifnot(length(what) == length(fractiles))
  time <- fit$inp$time
  t <- annual(time, time, when)$annvec
  res <- mapply(function(w, f) {
    g <- get.par(paste0("log", w), fit, exp = FALSE)
    gm <- annual(time, g[, 2], when)$annvec
    gsd <- annual(time, g[, 4], when)$annvec
    exp(qnorm(f, gm, gsd))
  }, what, fractiles) 
  res <- cbind(t, res)
  colnames(res) <- c("Year", paste(what, fractiles, sep = "_"))
  res
}

plotStockStatus <- function(fit, when, plot = TRUE){
  time <- fit$inp$time
  idx <- which(time == when)
  
  bbmsy <- get.par("logBBmsy", fit, exp = FALSE)
  ffmsy <- get.par("logFFmsy", fit, exp = FALSE)
  bbmsyLast <- bbmsy[idx, ,drop = FALSE]
  ffmsyLast <- ffmsy[idx, ,drop = FALSE]
  
  fractile <- 0.35
  bbmsyLast_pa <- exp(qnorm(fractile,bbmsyLast[,2],bbmsyLast[,4]))
  ffmsyLast_pa <- exp(qnorm(1 - fractile,ffmsyLast[,2],ffmsyLast[,4]))
  
  if (plot) {
    par(mfrow = c(2, 1), mar = c(4,4,0.5, 0.5))
    plotQuantity(fit, addUncertainty = TRUE, ylim = c(0, 3.5), ylab = "B/Bmsy")
    abline(h = 0.5, lty = 2, col = "darkgrey")
    points(when, bbmsyLast_pa, pch = 20)
    plotQuantity(fit, addUncertainty = TRUE, ylim = c(0, 3.5), what = "logFFmsy", ylab = "F/Fmsy")
    abline(h = 1, lty = 2, col = "darkgrey")
    points(when, ffmsyLast_pa, pch = 20) 
  }
  list(BBmsy_last = bbmsyLast_pa, FFmsy_last = ffmsyLast_pa,
       BBmsy = bbmsy, FFmsy = ffmsy)
}

normalize <- function(x) {
  return(x / mean(x))
}

getmanline <- function(fit) {
  cfy <- fit$inp$maninterval[1]
  by <- fit$inp$maneval
  find <- which(fit$inp$time == cfy)
  bind <- which(fit$inp$time == by)
  cind <- which(fit$inp$timeCpred == cfy)
  
  lan <- get.par("logCpred", fit, TRUE)[cind,2]
  ct <- lan/(100-meandis) * 100
  dis <- ct*meandis/100
  f <- get.par("logFFmsy", fit, TRUE)[find,2]
  if (f < 0.000001) f <- 0
  b <- get.par("logBBmsy", fit, TRUE)[bind,2]
  
  nms <- gsub("YYY", by,
              gsub("XXX", cfy, c("Total catch (XXX)", "Proj. landings (XXX)", 
                                 "Proj. discards (XXX)", "Fishing mortality (FXXX/FMSY)",
                                 "Stock size (BYYY/BMSY)", "% BYYY/BMSY change*", "% TAC change†", "% Advice change‡" )))
  setNames(data.frame(round(ct), round(lan), round(dis), 
                      icesRound(f), icesRound(b), icesRound((b - b_for) / b * 100),
                      icesRound((ct - as.numeric(lastTAC)) / as.numeric(lastTAC) * 100),
                      icesRound((ct - as.numeric(lastAdvice)) / as.numeric(lastAdvice) * 100)
                      , row.names = FALSE),
           nms)
}

plotspict.retro2 <- function (rep, stamp = get.version(), add.mohn = TRUE, CI = 0.95, xlim = NULL) {
  opar <- par(mfrow = c(2, 2), mar = c(2.5, 3.3, 4, 0.8))
  on.exit(par(opar))
  if (!"spictcls" %in% class(rep)) 
    stop("This function only works with a fitted spict object (class 'spictcls'). Please run `fit.spict` first.")
  if (!"retro" %in% names(rep)) 
    stop("No results of the retro function found. Please run the retrospective analysis using the `retro` function.")
  if (add.mohn) {
    mr <- suppressMessages(mohns_rho(rep, what = c("FFmsy", 
                                                   "BBmsy")))
    mrr <- round(mr, 3)
  }
  nruns <- length(rep$retro)
  bs <- bbs <- fs <- ffs <- time <- conv <- list()
  for (i in 1:nruns) {
    bs[[i]] <- get.par("logB", rep$retro[[i]], exp = TRUE, 
                       CI = CI)[rep$retro[[i]]$inp$indest, 1:3]
    bbs[[i]] <- get.par("logBBmsy", rep$retro[[i]], exp = TRUE, 
                        CI = CI)[rep$retro[[i]]$inp$indest, 1:3]
    fs[[i]] <- get.par("logFnotS", rep$retro[[i]], exp = TRUE, 
                       CI = CI)[rep$retro[[i]]$inp$indest, 1:3]
    ffs[[i]] <- get.par("logFFmsynotS", rep$retro[[i]], 
                        exp = TRUE, CI = CI)[rep$retro[[i]]$inp$indest, 
                                             1:3]
    time[[i]] <- rep$retro[[i]]$inp$time[rep$retro[[i]]$inp$indest]
    conv[[i]] <- rep$retro[[i]]$opt$convergence
  }
  conv <- ifelse(unlist(conv) == 0, TRUE, FALSE)
  sel <- function(x) x[, 2]
  cols <- spict:::cols()
  ylim <- range(0, sapply(bs[conv], sel), na.rm = TRUE) * 
    1.2
  plot(time[[1]], sel(bs[[1]]), type = "n", ylim = ylim, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(B[t]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(bs[[1]][, 1], rev(bs[[1]][, 
                                                                    3])), col = "lightgrey", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(bs[[i]]), col = cols[i], lwd = 2)
    }
  }
  par(lend = 2)
  s <- seq(nruns)
  lbls <- c("All", ifelse(conv[-1], paste0("-", s[-length(s)]), 
                          ""))[conv]
  cls <- cols[s][conv]
  if (sum(conv) <= 10) {
    ncol <- sum(conv)
  }
  else {
    ncol <- ceiling(sum(conv)/2)
    a <- seq(ncol)
    b <- seq(ncol + 1, sum(conv) + sum(conv)%%2)
    w <- unique(c(rbind(a, b)))
    lbls <- lbls[w]
    cls <- cls[w]
  }
  usr <- par("usr")
  xx <- usr[2] + diff(usr[c(1, 2)]) * 0.1
  yy <- mean(usr[4])
  legend(xx, yy, title = "Number of retrospective years", 
         xjust = 0.5, yjust = 0.1, legend = lbls, ncol = ncol, 
         col = cls, lty = 1, seg.len = 1, lwd = 6, x.intersp = 0.5, 
         bg = "transparent", box.lwd = 0, box.lty = 0, xpd = NA)
  par(lend = 1)
  box(lwd = 1.5)
  plot(time[[1]], sel(fs[[1]]), typ = "n", ylim = range(0, 
                                                        sapply(fs[conv], sel), na.rm = TRUE) * 1.2, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(F[t]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(fs[[1]][, 1], rev(fs[[1]][, 
                                                                    3])), col = "lightgrey", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(fs[[i]]), col = cols[i], lwd = 2)
    }
  }
  box(lwd = 1.5)
  par(mar = c(4, 3.3, 2.5, 0.8))
  plot(time[[1]], sel(bbs[[1]]), typ = "n", ylim = range(0, 
                                                         sapply(bbs[conv], sel), na.rm = TRUE) * 1.2, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(B[t]/B[MSY]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(bbs[[1]][, 1], rev(bbs[[1]][, 
                                                                      3])), col = "lightgrey", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(bbs[[i]]), col = cols[i], lwd = 2)
    }
  }
  if (add.mohn) 
    mtext(bquote("Mohn's " * rho[B/B[MSY]] * " = " * .(unname(mrr["BBmsy"]))), 
          3, 0.1)
  box(lwd = 1.5)
  plot(time[[1]], sel(ffs[[1]]), typ = "n", ylim = range(0, 
                                                         sapply(ffs[conv], sel), na.rm = TRUE) * 1.2, xlab = "", 
       ylab = "", lwd = 1.5, xlim = xlim)
  title(ylab = expression(F[t]/F[MSY]), line = 2.2)
  polygon(c(time[[1]], rev(time[[1]])), c(ffs[[1]][, 1], rev(ffs[[1]][, 
                                                                      3])), col = "lightgray", border = NA)
  for (i in seq(nruns)) {
    if (conv[i]) {
      lines(time[[i]], sel(ffs[[i]]), col = cols[i], lwd = 2)
    }
  }
  if (add.mohn) 
    mtext(bquote("Mohn's " * rho[F/F[MSY]] * " = " * .(unname(mrr["FFmsy"]))), 
          3, 0.1)
  box(lwd = 1.5)
  txt.stamp(stamp, do.flag = TRUE)
  nnotconv <- sum(!conv)
  if (nnotconv > 0) {
    message("Excluded ", nnotconv, " retrospective runs that ", 
            if (nnotconv == 1) 
              "was"
            else "were", " not converged: ", paste(which(!conv) - 
                                                     1, collapse = ", "))
  }
  if (add.mohn) 
    mr
  else invisible(NULL)
}

plotQuantity <- function(x, add = FALSE, col = 1, ylim = NULL, xlim = NULL,
                         addUncertainty = FALSE, col.unc = 'lightgrey',
                         what = "logBBmsy", ylab = sub("log", "", what)) {
  bbmsy <- get.par(what, x, exp = TRUE)
  idx <- x$inp$indest
  time <- x$inp$time[idx]
  if ( ! add) {
    plot(time, bbmsy[idx, 2], col = col, lwd = 2, type = "n", 
         ylim = ylim, xlim = xlim,
         xlab = "", ylab = "")
    title(xlab = "Year", ylab = ylab)
  }
  if (addUncertainty) {
    polygon(c(time, rev(time)), 
            c(bbmsy[idx, 1], rev(bbmsy[idx, 3])), 
            col = col.unc, border = 'darkgrey')
  }
  lines(time, bbmsy[idx, 2], col = col, lwd = 2, type = "l", ylim = ylim)
}
getFractileSeries <- function(fit, what = c("BBmsy", "FFmsy"), fractiles = c(0.35, 0.65), when = last) {
  stopifnot(length(what) == length(fractiles))
  time <- fit$inp$time
  t <- annual(time, time, when)$annvec
  res <- mapply(function(w, f) {
    g <- get.par(paste0("log", w), fit, exp = FALSE)
    gm <- annual(time, g[, 2], when)$annvec
    gsd <- annual(time, g[, 4], when)$annvec
    exp(qnorm(f, gm, gsd))
  }, what, fractiles) 
  res <- cbind(t, res)
  colnames(res) <- c("Year", paste(what, fractiles, sep = "_"))
  res
}

plotStockStatus <- function(fit, when, plot = TRUE){
  time <- fit$inp$time
  idx <- which(time == when)
  
  bbmsy <- get.par("logBBmsy", fit, exp = FALSE)
  ffmsy <- get.par("logFFmsy", fit, exp = FALSE)
  bbmsyLast <- bbmsy[idx, ,drop = FALSE]
  ffmsyLast <- ffmsy[idx, ,drop = FALSE]
  
  fractile <- 0.35
  bbmsyLast_pa <- exp(qnorm(fractile,bbmsyLast[,2],bbmsyLast[,4]))
  ffmsyLast_pa <- exp(qnorm(1 - fractile,ffmsyLast[,2],ffmsyLast[,4]))
  
  if (plot) {
    par(mfrow = c(2, 1), mar = c(4,4,0.5, 0.5))
    plotQuantity(fit, addUncertainty = TRUE, ylim = c(0, 3.5), ylab = "B/Bmsy")
    abline(h = 0.5, lty = 2, col = "darkgrey")
    points(when, bbmsyLast_pa, pch = 20)
    plotQuantity(fit, addUncertainty = TRUE, ylim = c(0, 3.5), what = "logFFmsy", ylab = "F/Fmsy")
    abline(h = 1, lty = 2, col = "darkgrey")
    points(when, ffmsyLast_pa, pch = 20) 
  }
  list(BBmsy_last = bbmsyLast_pa, FFmsy_last = ffmsyLast_pa,
       BBmsy = bbmsy, FFmsy = ffmsy)
}