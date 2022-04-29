## Load the necessary libraries ################################################

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

## Import the downloaded csv ##################################################

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",") # adjust path

wildschwein_BE

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

wildschwein_BE$timelag <- as.numeric(difftime(lead(wildschwein_BE$DatetimeUTC), wildschwein_BE$DatetimeUTC, units = "secs"))

wildschwein_BE


# Task 2: Getting an overview

# How many individuals were tracked?
unique(wildschwein_BE$TierID)
unique(wildschwein_BE$TierName)
# => 3 Individuals

# For how long were the individuals tracked? Are there gaps?
wildschwein_dates <- wildschwein_BE %>%
  group_by(TierID) %>%
  summarise(
    startTime = min(DatetimeUTC),
    endTime = max(DatetimeUTC),
    duration = as.factor(as.integer(difftime(endTime, startTime, units = "days")))
  )
wildschwein_dates

wildschwein_dates %>%
  ggplot(aes(color = duration)) +
  geom_segment(aes(x = startTime, xend = endTime, y = TierID, yend = TierID), size = 0.5) +
  xlab("DatetimeUTC")



# Were all individuals tracked concurrently or sequentially?
# => Concurrently

# What is the temporal sampling interval between the locations?
# => What does that even mean?


# Task 3: Deriving movement parameters I: Speed

wildschwein_BE$steplength <- 
  ((lead(wildschwein_BE$E, 1) - wildschwein_BE$E)^2 + (lead(wildschwein_BE$N, 1) - wildschwein_BE$N)^2)^(1/2)
wildschwein_BE

wildschwein_BE$speed <- wildschwein_BE$steplength / wildschwein_BE$timelag
wildschwein_BE

# => Speed = Steplength per second => But what is steplength?