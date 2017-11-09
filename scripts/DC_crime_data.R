library(dplyr)
library(tidyr)
library(jsonlite)
library(ggplot2)
library(stringr)
library(lubridate)
library(ggmap)
library(gganimate)

#all data comes directly from the opendata.dc.gov website using their API 

#read in crime data sets from 2008 to 2017
crime_2017_all <- fromJSON("https://opendata.arcgis.com/datasets/6af5cb8dc38e4bcbac8168b27ee104aa_38.geojson")
crime_2016_all <- fromJSON("https://opendata.arcgis.com/datasets/bda20763840448b58f8383bae800a843_26.geojson")
crime_2015_all <- fromJSON("https://opendata.arcgis.com/datasets/35034fcb3b36499c84c94c069ab1a966_27.geojson")
crime_2014_all <- fromJSON("https://opendata.arcgis.com/datasets/6eaf3e9713de44d3aa103622d51053b5_9.geojson")
crime_2013_all <- fromJSON("https://opendata.arcgis.com/datasets/5fa2e43557f7484d89aac9e1e76158c9_10.geojson")
crime_2012_all <- fromJSON("https://opendata.arcgis.com/datasets/010ac88c55b1409bb67c9270c8fc18b5_11.geojson")
crime_2011_all <- fromJSON("https://opendata.arcgis.com/datasets/9d5485ffae914c5f97047a7dd86e115b_35.geojson")
crime_2010_all <- fromJSON("https://opendata.arcgis.com/datasets/fdacfbdda7654e06a161352247d3a2f0_34.geojson")
crime_2009_all <- fromJSON("https://opendata.arcgis.com/datasets/73cd2f2858714cd1a7e2859f8e6e4de4_33.geojson")
crime_2008_all <- fromJSON("https://opendata.arcgis.com/datasets/180d56a1551c4e76ac2175e63dc0dce9_32.geojson")

#access data properties
crime_2017_df <- crime_2017_all$features$properties
crime_2016_df <- crime_2016_all$features$properties
crime_2015_df <- crime_2015_all$features$properties
crime_2014_df <- crime_2014_all$features$properties
crime_2013_df <- crime_2013_all$features$properties
crime_2012_df <- crime_2012_all$features$properties
crime_2011_df <- crime_2011_all$features$properties
crime_2010_df <- crime_2010_all$features$properties
crime_2009_df <- crime_2009_all$features$properties
crime_2008_df <- crime_2008_all$features$properties

#exploratory plot - histogram of offenses in 2017
offense_2017 <- ggplot(crime_2017_df, aes(x = OFFENSE, y = ..count.., fill = SHIFT)) +
  geom_bar() +
  labs(title = "Frequency of DC Crimes in 2017") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank())

#join data from 2008 through 2016
crime_2008_through_2016 <- bind_rows(crime_2016_df, 
                                     crime_2015_df, 
                                     crime_2014_df, 
                                     crime_2013_df, 
                                     crime_2012_df,
                                     crime_2011_df,
                                     crime_2010_df,
                                     crime_2009_df,
                                     crime_2008_df)

###START CLEANING DATA FOR YEAR VS FREQUENCY OF CRIME PLOT###
#clean date information to start so there aren't 200k+ points on the graphic
crime_2008_through_2016$REPORT_DAT <- gsub(pattern = "([0-9]{4}-[0-9]{2}-[0-9]{2}).*", replacement = "\\1", crime_2008_through_2016$REPORT_DAT)
crime_2008_through_2016$REPORT_DAT <- ymd(crime_2008_through_2016$REPORT_DAT)
crime_2008_through_2016$REPORT_DAT <- gsub(pattern = "([0-9]{4}).*", replacement = "\\1", crime_2008_through_2016$REPORT_DAT)

#first step is to create recoded crime column so we can sum up how many crimes occured per year
crime_2008_through_2016$OFFENSE_Recode <- crime_2008_through_2016$OFFENSE
crime_2008_through_2016$OFFENSE_Recode <- gsub(pattern = ".*", replacement = "1", crime_2008_through_2016$OFFENSE_Recode)
crime_2008_through_2016$OFFENSE_Recode <- as.numeric(crime_2008_through_2016$OFFENSE_Recode) #convert to numeric

#reshape pre-cleaned data
frequency_crime_2008_through_2016 <- crime_2008_through_2016 %>%
  select(REPORT_DAT, OFFENSE, OFFENSE_Recode) %>% #select columns with year and count of offenses
  group_by(REPORT_DAT, OFFENSE) %>% #group by column containing year information
  summarise(freq.offense = sum(OFFENSE_Recode)) %>% #summarise by frequency of offense
  rename(DATE = REPORT_DAT) #rename column

#exploratory plot - time series of offenses from 2008-2016
crime_by_year <- ggplot(frequency_crime_2008_through_2016, aes(x = DATE, y = freq.offense, col = OFFENSE, group = OFFENSE)) +
  geom_line(size = 2, alpha = 0.6) +
  labs(title = "Frequency of Crimes in DC from 2008 to 2016", y = "Number of Offenses") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank())

#reshape data to understand what time of day crimes typically occur over time
time_crime_2008_through_2016 <- crime_2008_through_2016 %>%
  select(REPORT_DAT, SHIFT, OFFENSE, OFFENSE_Recode) %>% #select columns with year and count of offenses
  group_by(REPORT_DAT, OFFENSE, SHIFT) %>% #group by column containing year information
  summarise(freq.offense = sum(OFFENSE_Recode)) %>% #summarise by frequency of offense
  rename(DATE = REPORT_DAT) #rename column

#exploratory plot - time series of time of day for offenses from 2008 - 2016
crime_by_year <- ggplot(time_crime_2008_through_2016, aes(x = DATE, y = freq.offense, col = OFFENSE, group = OFFENSE)) +
  geom_line(size = 2, alpha = 0.6) +
  facet_grid(. ~ SHIFT) +
  labs(title = "Frequency of Crimes in DC from 2008 to 2016", y = "Number of Offenses") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank())


#exploratory map plot of crime concentration
#create the DF with longitude and latitude
map_df_crime_2008_through_2016 <- bind_rows(crime_2016_df, 
                                     crime_2015_df, 
                                     crime_2014_df, 
                                     crime_2013_df, 
                                     crime_2012_df,
                                     crime_2011_df,
                                     crime_2010_df,
                                     crime_2009_df,
                                     crime_2008_df)

#create the dc_map_13 with get_map from ggmap
dc_map_13 <- get_map(location = "DC, USA", zoom = 13)

#create the map of DC
ggmap(dc_map_13) +
  geom_point(data = crime_2016_df, aes(x = LONGITUDE, y = LATITUDE, col = OFFENSE), size = 6, alpha = 0.6)

##need to install homebrew, then install imagemagick for this to work
#####   make animated map .gif ####
auto_theft_crime <- crime_2008_through_2016 %>%
  filter(OFFENSE == "THEFT F/AUTO") %>%
  arrange(REPORT_DAT)

auto_theft_plot <- ggmap(dc_map_13) +
  geom_point(data = auto_theft_crime, aes(x = LONGITUDE, y = LATITUDE, col = REPORT_DAT, group = REPORT_DAT, frame = REPORT_DAT, cumulative = TRUE), size = 6, alpha = 0.6)

gganimate(auto_theft_plot, interval = 1.0, filename = "auto_theft_plot.gif")


#saveGIF code
saveGIF({
  for (i in unique(auto_theft_crime$REPORT_DAT)) {
    data <- filter(auto_theft_crime, REPORT_DAT == i)
    
    p <- ggmap(dc_map_13) +
      geom_point(data = data, aes(x = LONGITUDE, y = LATITUDE, col = OFFENSE), size = 3, alpha = 0.6) +
      ggtitle(i)
    
    print(p)
  }
}, movie.name = "DC_auto_theft_map.gif", outdir = getwd(), interval = 2.0)

######
######
######
#make bounding box to foucus on what specific area (like NW)
bbox <- make_bbox(lon = dc_map_13$LONGITUDE, lat = dc_map_13$LATITUDE, f = 0.3)

#update get_map to use bbox
dc_map_13_box <- get_map(location = bbox, zoom = 13)

#map from pervious exercise









