install.packages("ggplot2")
library(ggplot2) #plotting library
library(dplyr)

# Define the URL
url <- "https://data.ceda.ac.uk/badc/ipcc-ddc-regions/data/ar5/seasonal-means/IPCC-AR5-WGII-Chap21SM_scatter.csv"
#url <- "https://data.ceda.ac.uk/badc/ipcc-ddc-regions/data/ar5/seasonal-means/var_IPCC-AR5-WGII-Chap21SM_scatter.csv"
# Create a temporary file location
temp_file <- tempfile(fileext = ".csv")
# Download the file
download.file(url, destfile = temp_file, mode = "wb")
# Read the CSV into R
ipcc_data <- read.csv(temp_file)
# Preview the data
head(ipcc_data)
ipcc_data = as.data.frame( ipcc_data)
unique(ipcc_data$Activity)
unique(ipcc_data$Scenario)

#plot the data-- question first!
# Which Season has the most projected variation? (for each scenario)
ggplot(ipcc_data, aes(x = Season, y = T)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  facet_wrap(~ Scenario, nrow = 1) +
  labs(
    title = "Which Season has the most projected variation? \n (for each scenario)",
    x = "Season",
    y = "Temperature Difference All Regions"
  ) +
  theme_minimal()


#scatter- x = delta T, y = deltaP, row = region (5), column = season, color = scenario
#Look across rows for seasonal varition in each region
#Look down rows for variation across regions in each season
#Look at color for model variation- red has highest projected change across all regions
# WHAT ARE MY COLUMNS????
names(ipcc_data)
unique(ipcc_data$Region) # combined to 5
unique(ipcc_data$Model)
unique(ipcc_data$Year)
unique(ipcc_data$Scenario)
unique(ipcc_data$Label) # unique(ipcc_data$Region)
unique(ipcc_data$Mask)
unique(ipcc_data$Activity) # combine with Scenario
ipcc_data$Type = paste( ipcc_data$Activity, ipcc_data$Scenario, sep = " ")

#ONLY SPECIFIC REGIONA in SOUTH and CENTRAL AMERICA
filtered_data <- ipcc_data %>%
  filter(Label %in% c("CAM", "AMZ", "NEB", "WSA", "SSA"))
hist(filtered_data$T)

ggplot(filtered_data[ filtered_data$Mask == "land", ], aes(x = T, y = pr, color = Type)) +
  geom_point(alpha = 0.7, size = 1) +
  facet_grid(Label ~ Season) +  # Rows = Region, Columns = Season
  labs(
    title = "Projected Seasonal Temperature and Precipitation Changes",
    x = "Δ Temperature (°C)",
    y = "Δ Precipitation (%)",
    color = "Scenario"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
