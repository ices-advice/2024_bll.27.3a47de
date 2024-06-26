## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)
library(icesSAG)
library(icesASD)
library(rmarkdown)

# source.taf("report")

mkdir("report")
mkdir("report/tables")
cp("boot/initial/report/*", "report/")
cp("boot/initial/00_functions.R", "report/")

outdir <- "report/"

output_format <- NULL # "all"
quiet <- FALSE

icesTAF::msg("Report: Making catch working document")
render("report/bll.3a47de_catch_WD.Rmd", output_dir = outdir,
       #output_format = output_format,
       clean = TRUE, quiet = quiet,  encoding = 'UTF-8')

icesTAF::msg("Report: Making assessment working document")
render("report/bll.3a47de_assessment_WD.Rmd", output_dir = outdir,
       #output_format = output_format,
       clean = TRUE, quiet = quiet,  encoding = 'UTF-8')

icesTAF::msg("Report: Making catch and assessment presentation")
render("report/bll.3a47de_catch_assessment_Presentation.Rmd",
       output_dir = outdir,
       #output_format = output_format,
       clean = TRUE, quiet = quiet,  encoding = 'UTF-8')


#####MAKING SAG PLOTS ################

bllcatch <- read.taf("./../data/bll.27.3a47de.catches.csv")
# bllcatch <- read.taf("./data/bll.27.3a47de.catches.csv")

lastyr <- max(bllcatch$Year)

info <- stockInfo(StockCode = "bll.27.3a47de",
                  AssessmentYear = lastyr+1,
                  Purpose = "Advice",
                  StockCategory = 2,
                  ContactPerson = "damian.villagra@ilvo.vlaanderen.be",
                  # B: Biomass model (like SPiCT)
                  ModelType = "B",
                  ModelName = "SPiCT",
                  # B/Bmsy
                  StockSizeDescription="Biomass relative to Bmsy", StockSizeUnits="",
                  # F/Fmsy

                  FishingPressureDescription="F/Fmsy", FishingPressureUnits="ratio",
                  # catch
                  CatchesLandingsUnits="t",
                  # REFPTS
                  # F/FMSY
                  FMSY = 1,
                  # B/BMSY
                  MSYBtrigger = 0.5,
                  # CIs
                  ConfidenceIntervalDefinition="95%")

# assessmentsummary <- read.taf("./report/tables/bll.27.3a47de.assessment.summary.csv")
assessmentsummary <- read.taf("./tables/bll.27.3a47de.assessment.summary.csv")

fishdata <-
  stockFishdata(
    Year = assessmentsummary$Year,
    # Catches
    # Catches = assessmentsummary$Catches,
    Landings = assessmentsummary$Landings,
    Discards = assessmentsummary$Discards )

# F/Fmsy   
fishdata$FishingPressure <- assessmentsummary$`F/Fmsy`
fishdata$Low_FishingPressure <- assessmentsummary$`F/Fmsy_lower`
fishdata$High_FishingPressure <- assessmentsummary$`F/Fmsy_upper`
# B/Bmsy
fishdata$StockSize <- assessmentsummary$`B/Bmsy`
fishdata$Low_StockSize <- assessmentsummary$`B/Bmsy_lower`
fishdata$High_StockSize <- assessmentsummary$`B/Bmsy_upper`

options(icesSAG.use_token = TRUE)
options(icesSAG.messages = FALSE)

xmlfile <- createSAGxml(info, fishdata)
capture.output(cat(xmlfile), file="output/bll.27.3a47de_SAG.xml")

key <- icesSAG::uploadStock(info,fishdata,verbose = TRUE)

# key <- findAssessmentKey(stock = "bll.27.3a47de", year = lastyr+1)

Catches_Plot <- icesSAG::getLandingsGraph(key)
F_Plot <- icesSAG::getFishingMortalityGraph(key)
B_Plot <- icesSAG::getSpawningStockBiomassGraph(key)
F_Histo_plot <- icesSAG::getFishingMortalityHistoricalPerformance(key)
B_Histo_plot <- icesSAG::getSSBHistoricalPerformance(key)
# icesSAG::setSAGSettingForAStock(key, chartKey = 10, settingKey = 58, settingValue = length(seq(lastyr:2022)))

plot(icesSAG::getStockStatusTable(key))
getStockStatusValues(key)

plot(Catches_Plot)
plot(F_Plot)
plot(B_Plot)
plot(Histo_plot)
plot(B_Histo_plot)


key <- findAssessmentKey(stock = "bll.27.3a47de", year = lastyr+1)
a <- icesSAG::getStockStatusTable(key)
plot(a)
