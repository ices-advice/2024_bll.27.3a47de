## Extract results of interest, write TAF output tables

## Before:
## After:

##################################CONTINUE HEEEEEEEEEEEEEEEEEEEEERRRRRRRRRRRRRRRRRRRRREEEEEEEEEEEEEEEEE

###################################
icesTAF::msg("Model: Load packages")
###################################
library(spict)
library(FLCore)
library(ggplot2)
library(patchwork)
library(stringr)
library(grid) # for unit()
library(RColorBrewer)
library(icesAdvice)
library(kableExtra)
library(float)
library(dplyr)
library(data.table)
library(icesTAF)
options(dplyr.summarise.inform=FALSE)

mkdir("output")
mkdir("output/tables")
source("./00_functions.R")

## ggplot options
theme_set(theme_minimal())
type <- "qual"
palette <- 3

levels_country <- c("Belgium","Germany", "Denmark", "France", "Ireland", "Netherlands", "Norway", "Sweden", "UK", "Channel Is.- Guernsey", "Channel Is.- Jersey", "Isle of Man", "UK (England)", "UK(Scotland)", "UK(Northern Ireland)")
labels_country <- c("BE", "DE", "DK" , "FR", "IE", "NL", "NO", "SE", "UK","GG" , "JE", "IM","UK","UK", "UK")
levels_country_ol <- c("BE", "DE", "DK", "FR", "IE", "NL", "NO", "SE", "UK", "CI", "GG", "IM", "JE")
labels_country_ol <- c("BE", "DE", "DK", "FR", "IE", "NL", "NO", "SE", "UK", "UK", "UK", "UK", "UK")
levels_gear <- c("OTB", "SDN", "TBB", "GNS", "GTR", "LLS", "MIS", "SSC", "FPO", "DRB", "LHP", "OTM")
labels_gear <-  c("Otter and Seine", "Otter and Seine", "Beam", "Trammel/gillnets", "Trammel/gillnets", "Other", "Other", "Otter and Seine", "Other", "Other", "Other", "Other")
levels_area <- c("27.3.a", "27.3.a.21", "27.3.a.20", "27.4" , "27.4.a", "27.4.b", "27.4.c","27.7.d", "27.7.e")
labels_area <-  c("27.3.a", "27.3.a", "27.3.a", "27.4" , "27.4", "27.4", "27.4","27.7.de", "27.7.de")

###################################
icesTAF::msg("Model: Load data")
###################################
# olbll <- readRDS("./../data/official_landings_bll.27.3a47de.Rds") 
olbll <- read.taf("./data/bll.27.3a47de.official.landings.csv")
# icd <- readRDS(file = "./../data/brill.3a47de.InterCatch_raised_discards.Rds")
icd <- read.taf(file = "./data/bll.27.3a47de.IC.raised.csv")
lastyr <- max(icd$Year)
# OL_BMS <- data.frame(readRDS("./../data/bll.27.3a47de.BMS.Rds")) 
OL_BMS <- read.taf("./data/bll.27.3a47de.ol.BMS.csv")


###################################
icesTAF::msg("Model: Getting Official landings per Area")
###################################

olbll_summ <- olbll%>% 
  group_by(Year, Country, Area)%>% 
  summarise(Landings = sum(Landings, na.rm = TRUE), .groups = "drop")

olbll_27.3.a <- olbll_summ%>% filter(Area == "27.3.a")%>%select(!Area)%>% 
  pivot_wider(names_from = "Country", values_from = c("Landings"))%>% 
  mutate(UK= UK+ IE)%>%
  select(!IE)%>%
  mutate(Total = rowSums(.[-1], na.rm = TRUE)) %>%
  mutate(across(!Year, ~replace(., is.na(.), 0))) %>% 
  mutate(across(!Year, round)) %>% 
  select(Year,BE,DE,DK,NL,NO,SE,UK,Total)

olbll_27.4 <- olbll_summ%>% filter(Area == "27.4")%>%select(!Area)%>% 
  reshape2::dcast(Year~Country, value.var = "Landings") %>% 
  mutate(Total = rowSums(.[-1], na.rm = TRUE)) %>%
  mutate(across(!Year, ~replace(., is.na(.), 0)),
         UK = GG+IE+IM+UK) %>% 
  select(-GG,-IE,-IM)%>% 
  mutate(across(!Year, round))%>% 
  select(Year, BE, DE,DK,FR,UK,NL,NO,SE, Total)

olbll_27.7.de <- olbll_summ%>% filter(Area == "27.7.de")%>%select(!Area)%>% 
  reshape2::dcast(Year~Country, value.var = "Landings") %>% 
  mutate(across(!Year, ~replace(., is.na(.), 0)),         
         CI = CI+GG+JE) %>% 
  select(-GG,-JE) %>% 
  mutate(Total = BE+ CI+ DE+DK+FR+IE+NL+UK) %>%
  mutate(across(!Year, round))%>% 
  select(Year, BE, DE,DK,FR,UK,IE,NL,CI,Total)

write.taf(olbll_27.3.a, file = "./output/tables/bll.27.3a47de.official.landings.3.a.csv")
write.taf(olbll_27.4,file = "./output/tables/bll.27.3a47de.official.landings.4.csv")
write.taf(olbll_27.7.de,file = "./output/tables/bll.27.3a47de.official.landings.7.de.csv")

olbll_summ_area <- olbll_summ%>% 
  group_by(Year, Area)%>% 
  summarise(Landings = sum(Landings, na.rm = TRUE))%>%
  mutate(Percent = 100*Landings/ sum(Landings))

olbll_summ_country <- olbll_summ%>% 
  group_by(Year, Country)%>% 
  summarise(Landings = sum(Landings, na.rm = TRUE))%>%
  mutate(Percent = 100*Landings/ sum(Landings))















############################## MAKING FLSTOCK OBJECTS FOR MIXFISH ###########################################

fit <- readRDS("./../model/bll.27.3a47de_fit.Rds")
# fit <- readRDS("./model/bll.27.3a47de_fit.Rds")
spict_fit <- fit
spict2flbeia <- function (spict_fit, wt_units = "kg", n_units = "10^3", catch_units = "t",
                          stock_name = "stk", disc = NULL)
{
  Bs <- as.data.frame(get.par("logB", spict_fit, exp = TRUE))
  Bs$time <- as.numeric(rownames(Bs))
  Bs$year <- floor(Bs$time)
  yrs <- sort(unique(Bs$year))
  tmp <- data.frame(year = yrs)
  tmp$B <- Bs$est[match(tmp$year, Bs$time)]
  flq <- FLQuant(tmp$B, dim = c(1, nrow(tmp)), dimnames = list(age = 1,
                                                               year = tmp$year), units = "t")
  stock <- FLStock(stock = flq, name = stock_name)
  stock@stock.wt[1, ] <- 1
  stock@stock.n <- stock@stock/stock@stock.wt
  stock@stock.wt@units <- wt_units
  stock@stock.n@units <- n_units
  Fs <- as.data.frame(get.par("logF", spict_fit, exp = TRUE))
  Fs$time <- as.numeric(rownames(Fs))
  Fs$year <- floor(Fs$time)
  tmp <- aggregate(Fs$est, list(year = Fs$year), FUN = mean)
  names(tmp)[which(names(tmp) == "x")] <- "f"
  stock@harvest[, ac(yrs)] <- tmp$f[match(yrs, tmp$year)]
  stock@harvest@units <- "f"
  Cs <- as.data.frame(get.par("logB", spict_fit, exp = TRUE) *
                        get.par("logF", spict_fit, exp = TRUE) * spict_fit$inp$dt)
  Cs$time <- as.numeric(rownames(Cs))
  Cs$year <- floor(Cs$time)
  tmp <- aggregate(Cs$est, list(year = Cs$year), FUN = sum)
  names(tmp)[which(names(tmp) == "x")] <- "catch"
  stock@catch[, ac(yrs)] <- tmp$catch[match(yrs, tmp$year)]
  stock@catch.wt[, ] <- 1
  stock@catch.n[] <- c(stock@catch/stock@catch.wt)
  stock@catch@units <- catch_units
  stock@catch.wt@units <- wt_units
  stock@catch.n@units <- n_units
  if (!is.null(disc)) {
    stock@discards[, ac(yrs)] <- disc
  }
  else {
    stock@discards[, ac(yrs)] <- 0
  }
  stock@discards.wt[1, ] <- 1
  stock@discards.n[] <- c(stock@discards/stock@discards.wt)
  stock@discards@units <- catch_units
  stock@discards.wt@units <- wt_units
  stock@discards.n@units <- n_units
  stock@landings <- stock@catch - stock@discards
  stock@landings.wt[1, ] <- 1
  stock@landings.n[] <- c(stock@landings/stock@landings.wt)
  stock@landings@units <- catch_units
  stock@landings.wt@units <- wt_units
  stock@landings.n@units <- n_units
  stock@mat[1, ] <- 1
  stock@harvest.spwn[1, ] <- 0
  stock@m[1, ] <- 0
  stock@m.spwn[1, ] <- 0
  tab1 <- sumspict.parest(spict_fit)
  tab3 <- sumspict.states(spict_fit)
  tab5 <- sumspict.predictions(spict_fit)
  r.stk <- (get.par("logm", spict_fit, exp = T)[2] * get.par("logn",
                                                             spict_fit, exp = T)[2]^(get.par("logn", spict_fit, exp = T)[2]/(get.par("logn",
                                                                                                                                     spict_fit, exp = T)[2] - 1)))/get.par("logK", spict_fit,
                                                                                                                                                                           exp = T)[2]
  K.stk <- get.par("logK", spict_fit, exp = T)[2]
  p.stk <- get.par("logn", spict_fit, exp = T)[2] - 1
  res <- list()
  res$stk <- stock
  res$BDinfo$par.fixed <- spict_fit$par.fixed
  res$BDinfo$cov.fixed <- spict_fit$cov.fixed
  res$BDinfo$PellaTomlinson_pars <- c(r = r.stk, K = K.stk,
                                      p = p.stk)
  res$BDinfo$refPts <- c(Fmsy = get.par("logFmsy", spict_fit,
                                        exp = TRUE)[, "est"], Bmsy = get.par("logBmsy", spict_fit,
                                                                             exp = TRUE)[, "est"], MSY = get.par("MSY", spict_fit)[,
                                                                                                                                   "est"])
  return(res)
}

res <- spict2flbeia(spict_fit = fit)
save(res, file = "./output/bll.27.3a47de.FLBEIA.RData")

stock_estimated <- res$stk
op <- par(mfcol = c(3,1), mar = c(3,4,2,2))

plotspict.biomass(fit)
plotspict.catch(fit)
plotspict.f(fit)

par(op)
# compare FLStock
plot(stock_estimated, metrics = list(SSB = ssb, Catch = catch, F = fbar))
