
setwd("~/DSTribune/Stories/FlintWaterQuality")
library(ggplot2)
library(acs)
library(dplyr)
library(gdal)
library(maptools)


#download.file("http://waterqualitydata.us/Result/search?countrycode=US&statecode=US%3A26&countycode=US%3A26%3A049&sampleMedia=Water&characteristicType=Inorganics%2C+Major%2C+Non-metals&characteristicName=Chloride&mimeType=csv&zip=yes&sorted=no", destfile = "waterQualityMI")
wqDf <- read.csv("result.csv")
wqDf <- filter(wqDf, ActivityMediaSubdivisionName == "Surface Water", ResultSampleFractionText == 'Dissolved', ResultStatusIdentifier == 'Accepted')
wqDf$MonitoringLocationIdentifier <- as.character(wqDf$MonitoringLocationIdentifier)
wqDf$ActivityStartDate <- as.POSIXct(wqDf$ActivityStartDate)
wqDf <- wqDf %>%
  filter(ResultMeasureValue != "NA")

summary(wqDf$ResultMeasureValue)

ggplot(data=wqDf, aes(ResultMeasureValue)) + geom_histogram(color = "black", fill = 'blue') + ggtitle("Distribution of surface water dissolved chloride \n in Genesee County, USGS NAWQA") + ylab("Frequency") + xlab("mg/l Dissolved Chloride")

#download.file("http://waterqualitydata.us/Result/search?countrycode=US&statecode=US%3A26&countycode=US%3A26%3A049&mimeType=csv&zip=yes&sorted=no", destfile = "stationLocationsMI")
#download.file("http://waterqualitydata.us/Station/search?countrycode=US&statecode=US%3A26&countycode=US%3A26%3A049&mimeType=kml&zip=yes&sorted=no", destfile = "stationShapesMI")

sampleData <- wqDf %>%
  group_by(MonitoringLocationIdentifier) %>%
    summarise(Avg = mean(ResultMeasureValue, na.rm = TRUE),
              Max = max(ResultMeasureValue, na.rm = TRUE),
              LatestSample = max(ActivityStartDate, na.rm = TRUE),
              totalSamples = n())
print(sampleData)

#At this point we used QGIS to open the kml and extract the coordinates for these 5 unique sample locations
Lat = c(-83.79, -83.7902778, -83.8460698, -83.6693998, -83.4627282)
Long = c(42.9111111, 42.9119444, 42.9419733, 43.0003052, 43.1144707)
MonitoringLocationIdentifier = c("USGS-0414826544", "USGS-0414826545", "USGS-041482663", "USGS-430001083401006", "USGS-430645083274606")
coords <- data.frame(Lat, Long, MonitoringLocationIdentifier)
coords <- merge(x=coords, y=sampleData, by="MonitoringLocationIdentifier", all = TRUE)
