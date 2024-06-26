## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(dplyr)
library(tidyr)
library(grid) # for unit()
library(RColorBrewer)
library(readr)
library(ggplot2)
# taf.bootstrap()
mkdir("data")
cp("boot/initial/00_functions.R", "./")
source("./00_functions.R")
###################################
icesTAF::msg("Data: defining parameters and helper functions")
###################################

levels <- c("Belgium","Channel Is.- Guernsey","Channel Is.- Jersey",
            "Channel Islands (ns)","Denmark","France","Germany","Germany  Fed. Rep. Of", "Germany, Fed. Rep. of",
            "Ireland","Netherlands","Norway", "Sweden",
            "UK (England)","UK(Scotland)", "UK - Scotland", "UKS", "UKE", "UK - Eng+Wales+N.Irl.", "UK - England & Wales", "GB", "UK(Northern Ireland)")


labels <- c("BE", "GG" , "JE",
            "CI", "DK", "FR", "DE","DE", "DE",
            "IE","NL", "NO", "SE",
            "UK","UK", "UK","UK","UK","UK","UK","UK", "UK")


###################################
icesTAF::msg("Data: Loading Official Landings data")
###################################
  
  ## Loading data and settings ----
  mkdir("data")
  
  olhist <-
    read.csv("boot/data/ICES_1950-2010.csv",
             stringsAsFactors = FALSE, header = TRUE) %>% as_tibble() %>%
    filter(Species == "Brill", Division %in% c("III a","IV (not specified)", "IV a","IV a+b (not specified)","IV b","IV c", "VII d","VII e","VII d+e (not specified)")) %>%
    reshape2:::melt.data.frame(id.vars = c("Species", "Division",  "Country"), variable.name = "Year") %>%
    as_tibble() %>%
    filter(as.integer(substr(Year, start=2, stop=5)) >= 1950)%>%
    mutate(Year = as.integer(substr(Year, start=2, stop=5)),
           Country = ifelse(Country %in% levels, Country, "Other"),
           Country = factor(Country, levels = levels, labels = labels, ordered = TRUE),
           Area = case_when(Division %in% c("III a")~ "27.3.a",
                            Division %in% c("IV (not specified)", "IV a","IV a+b (not specified)","IV b","IV c")~ "27.4",
                            Division %in% c("VII d","VII e","VII d+e (not specified)")~ "27.7.de")) %>%
    filter(! value %in% c("-", ".", "<0.5")) %>%
    group_by(Species,Year, Country,Area) %>%
    summarize(Landings = sum(as.numeric(value), na.rm = TRUE))%>%
    mutate( Species= "BLL", Units = "TLW")%>%
    filter(Year < 2006, Landings > 0)

  
  ol <- read_csv("boot/data/ICESCatchDataset2006-2021.csv", na = "0 c") %>%
    as_tibble()
  
  prels <- bind_rows(readPrel("boot/data/Preliminary_landings_allSpecies_2022.csv"),
                     readPrel("boot/data/Preliminary_landings_allSpecies_2023.csv"))
  
  olbll <- ol %>% filter(Species == "BLL", Area %in% c("27.3.a","27.4", "27.7.d", "27.7.e")) %>%
    reshape2:::melt.data.frame(id.vars = c("Species", "Area", "Units", "Country"), variable.name = "Year") %>%
    as_tibble() %>%
    mutate(Country = ifelse(Country %in% c("GB","IM"), "UK", Country),
           # Year = as.integer(substr(Year, start=2, stop=5)),
           Year = as.integer(as.character(Year)),
           Country = factor(Country, levels = unique(labels), ordered = TRUE),
           Area = case_when(Area %in% c("27.3.a","27.3.a_NK","27.3.a.20","27.3.a.21")~ "27.3.a",
                            Area %in% c("27.4", "27.4.a", "27.4.b", "27.4.c")~ "27.4",
                            Area %in% c("27.7.d", "27.7.e")~ "27.7.de")) %>%
    group_by(Species, Area, Units, Country, Year) %>%
    summarise(Landings = sum(as.numeric(value), na.rm = TRUE)) %>%
    bind_rows(olhist, prels)

  write.taf(olbll , file = "bll.27.3a47de.official.landings.csv", dir = "data")

  bms <- bind_rows(
    getBMS("boot/data/Preliminary_landings_allSpecies_2022.csv"),
    getBMS("boot/data/Preliminary_landings_allSpecies_2023.csv"))
  
  write.taf(bms , file = "bll.27.3a47de.ol.bms.csv", dir = "data")
  ###################################
  icesTAF::msg("Data: Loading IC data")
  ###################################
  
  # Now load Intercatch data to merge with OL
  icd <- readRDS(file = "boot/initial/data/brill.3a47de.InterCatch_raised_discards.Rds") %>% filter(Year>2013)
  write.taf(icd, file = "bll.27.3a47de.intercatch.raised.csv", dir = "data", quote = TRUE)

  lastyr <- max(icd$Year)
  
    ## Catches ----
  #Get Landings and Discards for IC data
  IC_Catches <- lapply(split(icd, icd$Year), get_catchcat_percent)%>%
    bind_rows() %>%
    filter (!Area == "Total")%>%
    reframe (Species ="BLL",Area, Units = "TLW", Country, Year, Landings, Discards)

  write.taf(IC_Catches, file = "bll.27.3a47de.intercatch.catches.csv", dir = "data")
  
  ## Discard rate and estimation
  percent_discards <- lapply(split(icd, icd$Year), get_catchcat_percent)%>%
    bind_rows() %>%
    reshape2:::melt.data.frame(id.vars = c("Area", "Year", "Country"))

  
  ###################################
  icesTAF::msg("Data: Merging Official + IC data")
  ###################################
  
  bll.27.3a47de_catch<- olbll %>% group_by(Year) %>%
    summarise(Landings = sum(Landings)) %>%
    filter (!Year >=2014)%>%
    bind_rows(
      IC_Catches %>%
        filter (!Year <2014)%>%
        group_by(Year)%>%
        summarise(Landings = sum(Landings/1000,na.rm = TRUE ),
                  Discards = sum (Discards/1000,na.rm = TRUE )))%>%
    mutate(Total = Landings + Discards )
  
  write.taf(bll.27.3a47de_catch, file = "bll.27.3a47de.catches.csv", dir = "data")
  
  ###################################
  icesTAF::msg("Data: Calculating TAC uptake 2000-2023")
  ###################################
  
  #Historial Official landngs 2000-2005
  olhist2a4 <-
    read.csv("boot/data/ICES_1950-2010.csv",
             stringsAsFactors = FALSE, header = TRUE) %>% as_tibble() %>%
    filter(Species %in% c("Brill", "Turbot"), Division %in% c("II a (not specified)","II a2", "IV a","IV a+b (not specified)","IV b","IV c")) %>%
    reshape2:::melt.data.frame(id.vars = c("Species", "Division",  "Country"), variable.name = "Year") %>%
    as_tibble() %>%
    filter(as.integer(substr(Year, start=2, stop=5)) %in% c(2000:2005))%>%
    mutate(Year = as.integer(substr(Year, start=2, stop=5)),
           Country = ifelse(Country %in% levels, Country, "Other"),
           Country = factor(Country, levels = levels, labels = labels, ordered = TRUE)) %>%
    filter(! value %in% c("-", ".", "<0.5")) %>%
    group_by(Species,Year) %>%
    summarize(Landings = sum(as.numeric(value), na.rm = TRUE))%>%
    pivot_wider (names_from= "Species", values_from = "Landings")%>%
    mutate( Total = Brill + Turbot)
  
  # Official landngs 2006-2021
  ol2a4 <-
  ol %>% filter(Species %in% c("BLL", "TUR"), Area %in% c("27.2.a","27.4")) %>%
    reshape2:::melt.data.frame(id.vars = c("Species", "Area", "Units", "Country"), variable.name = "Year") %>%
    as_tibble() %>%
    mutate(Country = ifelse(Country == "GB", "UK", Country),
           # Year = as.integer(substr(Year, start=2, stop=5)),
           Year = as.integer(as.character(Year)),
           Country = factor(Country, levels = unique(labels), ordered = TRUE))%>%
    group_by(Species, Year) %>%
    summarize(Landings = sum(as.numeric(value), na.rm = TRUE))%>%
    pivot_wider (names_from= "Species", values_from = "Landings")%>%
    rename(Brill = BLL, Turbot = TUR)%>%
    mutate( Total = Brill + Turbot)
  
  #Preliminary Landings 2022-2023
  prel2a4<-
    bind_rows(
    read.csv("boot/data/Preliminary_landings_allSpecies_2022.csv",
             stringsAsFactors = FALSE, header = TRUE) %>% 
    filter(Species.Latin.Name %in% c("Scophthalmus rhombus", "Scophthalmus maximus"), Area %in% c("27_2_A_2","27_4_A", "27_4_B", "27_4_C")) %>%
    rename(Species= Species.Latin.Name)%>%
    mutate (BMS.Catch.TLW. = case_when(is.na(BMS.Catch.TLW.)~0,
                                  TRUE~as.double(BMS.Catch.TLW.)),
            Species = case_when(Species == "Scophthalmus rhombus"~"Brill",
                                Species == "Scophthalmus maximus"~"Turbot"))%>%
    group_by(Year,Species)%>%
    summarize (AMS_Catch = sum(AMS.Catch.TLW.),
               BMS_Catch = sum(BMS.Catch.TLW.),
               Total = AMS_Catch+BMS_Catch),
    read.csv("boot/data/Preliminary_landings_allSpecies_2023.csv",
             stringsAsFactors = FALSE, header = TRUE) %>% 
      filter(Species.Latin.Name %in% c("Scophthalmus rhombus", "Scophthalmus maximus"), Area %in% c("27_2_A_2","27_4_A", "27_4_B", "27_4_C")) %>%
      rename(Species= Species.Latin.Name)%>%
      mutate (BMS.Catch.TLW. = case_when(is.na(BMS.Catch.TLW.)~0,
                                         TRUE~as.double(BMS.Catch.TLW.)),
              Species = case_when(Species == "Scophthalmus rhombus"~"Brill",
                                  Species == "Scophthalmus maximus"~"Turbot"))%>%
      group_by(Year,Species)%>%
      summarize (AMS_Catch = sum(AMS.Catch.TLW.),
                 BMS_Catch = sum(BMS.Catch.TLW.),
                 Total = AMS_Catch+BMS_Catch))%>%
    select(Year, Species, Total )%>%
    pivot_wider(names_from = Species, values_from = Total)%>%
    mutate( Total = Brill + Turbot)
  
  
    Uptake2a4 <- rbind(olhist2a4, ol2a4, prel2a4) %>%
    full_join(read_csv("boot/data/TAC.csv"), by = "Year") %>%
    mutate(Uptake = ifelse(!is.na(Total), Total / `Agreed TAC 2a4` * 100, NA))
  
  write.taf(Uptake2a4, file = "bll.27.3a47de.uptake.TAC.2a4.csv", dir = "data")
    
  ###################################
  icesTAF::msg("Data: Calculating TAC (NEW AREA AND SPECIES) uptake 2024")
  ###################################

  prel2a43a7de <-  read.csv("boot/data/Preliminary_landings_allSpecies_2023.csv",
                            stringsAsFactors = FALSE, header = TRUE) %>%
    filter(Species.Latin.Name %in% c("Scophthalmus rhombus"),
           Area %in% c("27_2_A_2","27_4_A", "27_4_B", "27_4_C", "27_3_A_20", "27_3_A_21", "27_3_A", "27_7_E", "27_7_D")) %>%
    rename(Species = Species.Latin.Name) %>%
    mutate (BMS.Catch.TLW. = case_when(is.na(BMS.Catch.TLW.) ~ 0,
                                       TRUE ~ as.double(BMS.Catch.TLW.)),
            Area_grouped =  case_when(Area %in% c("27_2_A_2","27_4_A", "27_4_B", "27_4_C")~ "27.2a4",
                                      Area %in% c("27_3_A_20", "27_3_A_21", "27_3_A")~ "27.3a",
                                      Area %in% c("27_7_E", "27_7_D")~ "27.7de")) %>%
      group_by(Year)%>%
      summarize (AMS_Catch = sum(AMS.Catch.TLW.),
                 BMS_Catch = sum(BMS.Catch.TLW.),
                 Total = AMS_Catch+BMS_Catch)%>%
    select(Year, Total)


  Uptake2a3a47de <- as.data.frame(prel2a43a7de %>%
    full_join(read_csv("boot/data/TAC_BLL_2a3a47de.csv"), by = c("Year")) %>%
    ungroup() %>%
    rename("TAC"= Agreed.TAC.2a3a47de) %>% 
    mutate(Uptake = ifelse(!is.na(Total), Total / TAC * 100, NA)))
  
  write.taf(Uptake2a3a47de, file = "bll.27.3a47de.uptake.TAC.2a3a47de.csv", dir = "data")

  ###################################
  icesTAF::msg("Data: Writting in Data repository")
  ###################################

  write.taf(bll.27.3a47de_catch, dir = "data")


  #Read and write indices
  IndexSem1 <- read.taf("boot/data/Survey Indices/IndexSem1.csv")[-1]
  IndexSem2 <- read.taf("boot/data/Survey Indices/IndexSem2.csv")[-1]
  IndexSem83_98 <- read_csv("boot/data/Survey Indices/IndexSem83_98.csv")
  
  write.taf(IndexSem1, file = "bll.27.3a47de.idx.sem.1.csv", dir = "data")
  write.taf(IndexSem2, file = "bll.27.3a47de.idx.sem.2.csv", dir = "data")
  write.taf(IndexSem83_98, file = "bll.27.3a47de.idx.83.98.csv", dir = "data")
  

# Normalize the indices
IndexSem1$normalized <- normalize(IndexSem1$ObsI)
IndexSem2$normalized <- normalize(IndexSem2$ObsI)
IndexSem83_98$normalized <- normalize(IndexSem83_98$ObsI)

# Add 0.5 to timeI in IndexSem2
IndexSem2$timeI <- IndexSem2$timeI + 0.5

# Combine the data frames
combined_data <- rbind(transform(IndexSem1, Index = "Comb.idx.Sem.1"),
                       transform(IndexSem2, Index = "Comb.idx.Sem.2"),
                       transform(IndexSem83_98, Index = "Hist.idx.Q1"))

# Plotting
ggplot(combined_data, aes(x = timeI, y = normalized, group = Index, color = Index)) +
  geom_line() +
  geom_ribbon(aes(ymin = normalized - sd(normalized), ymax = normalized + sd(normalized), fill = Index), alpha = 0.3, linetype= 2) +
  labs(x = "Year", y = "Normalized Index") +
  scale_color_manual(values = c("blue", "red", "green")) +
  scale_fill_manual(values = c("blue", "red", "green")) +
  scale_x_continuous(breaks = seq(min(combined_data$timeI), max(combined_data$timeI), by = 2)) +
  theme(legend.position = "bottom")+
  theme_minimal()

ggsave(filename = paste("./boot/initial/report/figures/Normalized_Indices.pdf", sep=""), plot = last_plot(), width=20, height=10, dpi=300)
mkdir("output")
ggsave(filename = "./output/Figure.3.13.Indices.Normalized.jpg", plot = last_plot(), width=15, dpi=300)

# sourceTAF("data")