
setwd("~/DSTribune/Stories/FlintWaterQuality")
library(ggplot2)
library(dplyr)
library(XML)

temp <- tempfile()
download.file("http://waterqualitydata.us/Result/search?countrycode=US&statecode=US%3A26&countycode=US%3A26%3A049&sampleMedia=Water&characteristicType=Inorganics%2C+Major%2C+Non-metals&characteristicName=Chloride&mimeType=csv&zip=yes&sorted=no", temp)
wqGen<- read.csv(unz(temp, "result.csv"))
wqGen$County = "Genesee"

#The water planned to be sourced from Lake Huron is located in Sanilac County
#Source: http://www.nytimes.com/2014/05/26/business/detroit-plan-to-profit-on-water-looks-half-empty.html?_r=0

#The majority of Detroits water is sourced from the Detroit River in Wayne County
#Source: http://www.dwsd.org/downloads_n/customer_service/customer_information/water_quality_report.pdf

temp <- tempfile()
download.file("http://waterqualitydata.us/Result/search?countrycode=US&statecode=US%3A26&countycode=US%3A26%3A163&sampleMedia=Water&characteristicType=Inorganics%2C+Major%2C+Non-metals&characteristicName=Chloride&mimeType=csv&zip=yes&sorted=no", temp)
wqWayne<- read.csv(unz(temp, "result.csv"))
wqWayne$County = "Wayne"

#Merge the three County Water Measurements
wqDf <- rbind(wqGen, wqWayne)

#Save an offline version of the merged county water data
write.csv(wqDf, file ="MI3CountyCountyWaterData.csv")

wqDf <- filter(wqDf, ActivityMediaSubdivisionName == "Surface Water", ResultSampleFractionText == 'Dissolved', ResultStatusIdentifier == 'Accepted' | ResultStatusIdentifier == 'Final' | ResultStatusIdentifier == 'Historical')
wqDf$MonitoringLocationIdentifier <- as.character(wqDf$MonitoringLocationIdentifier)
wqDf$ActivityStartDate <- as.POSIXct(wqDf$ActivityStartDate)
wqDf <- wqDf %>%
  filter(ResultMeasureValue != "NA")

#What we want is a percentage of samples binned by concentration
percentConc<- wqDf %>%
  group_by(County) %>%
  summarise(Avg = mean(ResultMeasureValue, na.rm = TRUE),
            Max = max(ResultMeasureValue, na.rm = TRUE),
            LatestSample = max(ActivityStartDate, na.rm = TRUE),
            totalSamples = n(),
            stdError = sd(ResultMeasureValue, na.rm = TRUE))


ggplot(data=percentConc, aes(x=County, y=Avg)) + 
         stat_boxplot(geom ='errorbar') + 
         geom_boxplot()

percentConc$min <- percentConc$Avg - percentConc$stdError
percentConc$max <- percentConc$Avg + percentConc$stdError
       
plot1 <- ggplot(percentConc, aes(x=County)) 
plot1 <- plot1 + geom_errorbar(aes(ymin=min,ymax=max),data=percentConc,width = 0.5)
plot1 <- plot1 + geom_boxplot(aes(y=Avg))
plot1 <- plot1 + ggtitle("Surface Water Chloride Concentrations \n in Genesse, Sanilac, and Wayne County MI (USGS)") + ylab("Average Chloride Concentration")
plot1

#Just looking initially at potential corrosivity of initial surface water across the three counties it seems that Wayne County which contains the Detroit River may have a slight
# advantage because initial concentrations of chloride are lower

Gen <- filter(wqDf, County == "Genesee")
Way <- filter(wqDf, County == "Wayne")

Gen_Way <- t.test(Gen$ResultMeasureValue, Way$ResultMeasureValue)


Gen_Way$p.value


# Indeed initial untreated surface water concentrations of chloride are significantly lower in Wayne County compared to Sanilac and Genesee

# We should get more granular now and display these samples relative to the three sites rather than entire counties
# Shout out to https://tagteam.harvard.edu/hub_feeds/1981/feed_items/1014182 for showing a simple way to extract data from KML's

#Wayne KML
download.file("http://waterqualitydata.us/Station/search?countrycode=US&statecode=US%3A26&countycode=US%3A26%3A163&mimeType=kml&zip=yes&sorted=no", destfile = "WayneCountyStations")

#Sanilac KML
download.file("http://waterqualitydata.us/Station/search?countrycode=US&statecode=US%3A26&countycode=US%3A26%3A151&mimeType=kml&zip=yes&sorted=no", destfile = "SanilacCountyStations")

#Genesee KML
download.file("http://waterqualitydata.us/Station/search?countrycode=US&statecode=US%3A26&countycode=US%3A26%3A049&mimeType=kml&zip=yes&sorted=no", destfile = "GeneseeCountyStations")



sampleData <- wqDf %>%
  group_by(MonitoringLocationIdentifier) %>%
    summarise(Avg = mean(ResultMeasureValue, na.rm = TRUE),
              Max = max(ResultMeasureValue, na.rm = TRUE),
              LatestSample = max(ActivityStartDate, na.rm = TRUE),
              totalSamples = n())
print(sampleData)

#Vis of chloride concentrations
write.csv(sampleData,file="MI_SampleData.csv")



