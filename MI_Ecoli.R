library(dplyr)

setwd("~/DSTribune/Stories/FlintWaterQuality")
#wqDf <- read.csv("AllEcoliSamples.csv")
#wqDf <- read.csv("FecalColiform.csv")
#wqDf <- read.csv("TotalColiform.csv")
wqDf <- read.csv("FecalStrept.csv")


wqDf$ResultMeasureValue <- as.numeric(as.character(wqDf$ResultMeasureValue))
wqDf$MonitoringLocationIdentifier <- as.character(wqDf$MonitoringLocationIdentifier)
wqDf$ActivityStartDate <- as.POSIXct(wqDf$ActivityStartDate)

wqDf <- filter(wqDf, ResultMeasure.MeasureUnitCode == "cfu/100ml")

sampleData <- wqDf %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(Avg = mean(ResultMeasureValue, na.rm = TRUE),
            Max = max(ResultMeasureValue, na.rm = TRUE),
            LatestSample = max(ActivityStartDate, na.rm = TRUE),
            totalSamples = n())
print(sampleData)

coords <- read.csv("MI_Stations.csv")

df <- merge(x = sampleData, y = coords, by.x = "MonitoringLocationIdentifier", by.y = "Monitoring_Location_Identifier")

write.csv(df, file = "MI_StationAverage_FecalStrept.csv")
