# CODE DESCRIPTION ####
# always helpful to add a little description of the purpose of the code... you will thank your previous self.

rm(list=ls()) # clear workspace

#INSTALL PACKAGES ####
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")

#LOAD PACKAGES ####
library(dplyr)
library(tidyr)
library(ggplot2) 


#GET DATA ####
# Need to download file first (see "DATA DOWNLOAD" on right)
download_page <- "https://oceanhealthindex.org/global-scores/data-download/"
browseURL(download_page) #opens the webpage
# read in data
inDir = "G:/My Drive/Data to Policy/Data to Policy (student)/data/GitHub/Data-Env-Decision-Making/SeeingData/" #update this to your computer!!
data = read.csv( paste0(inDir,"scores.csv") )

#HOW MUCH DATA?? ####
names(data)
dim(data)
unique(data$scenario)
# NOTE: results are in the Consol window

#SUMMARIZE $ MANIPULATE DATA ####
#always start with a question so you know where you are going!
#What is the average score per region?
byRegion = data %>%
  group_by(region_name) %>%
  summarise(
    avg_score = mean(value, na.rm = TRUE)
  )
maxRegion1 =  byRegion$region_name[ which.max(byRegion$avg_score)]
cat ("This region has the highest score: ", maxRegion1)

#What is the average score per region, per year?
byRegionYear = data %>%
  group_by(region_name, scenario) %>%
  summarise(
    avg_score = mean(value, na.rm = TRUE)
  )

#What is the average score per region without Global average?
byRegion_NoGA = data %>% filter(region_name != maxRegion1) %>%
  group_by(region_name) %>%
  summarise(
    avg_score = mean(value, na.rm = TRUE),
    .groups = "drop"
  )
maxRegion2 =  byRegion_NoGA$region_name[ which.max(byRegion_NoGA$avg_score)]
cat ("This region has the highest score: ", maxRegion2 )

#What is the global score for each year?
data_Global = data %>% filter(region_name == maxRegion1)
byYear_Global = data %>% filter(region_name == maxRegion1) %>%
  group_by(scenario)  %>%
  summarise(
    avg_score = mean(value, na.rm = TRUE)
  )
 
#VISUALIZE DATA ####
# let's just use global data, or you can filter by another region of interest
#(think about how you might "loop" through regions or years) 
names(data_Global)
unique(data_Global$long_goal) #lists out the goals..

# PLOT 1: How do the global scores vary by goal?
dataLabel = maxRegion1
ggplot(data_Global, aes(x = goal, y= value, color = dimension)) +
  # add data as points
  geom_point() +
  # add labels
  labs(y = "score", title = paste0( "Ocean Health Index- ",  dataLabel),
       color = "dimension",
       subtitle = "How do the global scores vary by goal?" )  +
  # simplify the graphic 
  theme_minimal() 

#Let's just look at the current status
ggplot(data_Global %>% filter(dimension == "status"), aes(x = goal, y= value) ) +
  # add data as points
  geom_point() +
  # add labels
  labs(y = "score", title = paste0( "Ocean Health Index- ",  dataLabel),
       subtitle = "How do the global scores vary by goal?" )  +
  # simplify the graphic 
  theme_minimal() 
# can we turn it into a bars in a circle
p = ggplot(data_Global %>% filter(dimension == "status"), aes(x = goal, y = value, fill = goal)) +
  geom_col() +             # bars 
  coord_polar(start = 0) + # make it circular
  labs(
    title = paste0("Ocean Health Index - ", dataLabel),
    subtitle = "Global status scores by goal"
  ) +
  theme_minimal() +
  theme(
    axis.title  = element_blank(),   # usually remove axis labels in rose plots
    axis.text.y = element_blank(),   # hide y-axis text for cleaner look
    axis.ticks  = element_blank(),
    legend.position = "none", #remove legend- default with fill
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 10, face = "bold")
  )
p

ggsave(filename = paste0(inDir, "SeeingData-plot2.jpg"), plot = p , 
       width = 10, height = 12, dpi = 300)
