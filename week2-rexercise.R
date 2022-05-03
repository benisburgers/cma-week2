## Load the necessary libraries ################################################

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times
library(tidyverse)

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

wildschwein_BE %>%
  ggplot() +
  geom_freqpoly(mapping = aes(x = timelag, colour = TierName)) +
  xlim(c(700, 1000))

# Were all individuals tracked concurrently or sequentially?
# => Concurrently

# What is the temporal sampling interval between the locations?
# => What does that mean?


# Task 3: Deriving movement parameters I: Speed

wildschwein_BE$steplength <- 
  ((lead(wildschwein_BE$E, 1) - wildschwein_BE$E)^2 + (lead(wildschwein_BE$N, 1) - wildschwein_BE$N)^2)^(1/2)
wildschwein_BE

wildschwein_BE$speed <- wildschwein_BE$steplength / wildschwein_BE$timelag
wildschwein_BE

# => Speed = Steplength per second => But what is steplength? => Apparently it's meter (see Task 4, last plot) => m/s


# Task 4: Cross-scale movement analysis

caro <- read_delim("caro60.csv")
caro
caro <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE)
caro

caro_3 <- caro %>%
  filter(row_number() == 1 | row_number() %% 3 == 0)

caro_6 <- caro %>%
  filter(row_number() == 1 | row_number() %% 6 == 0)

caro_9 <- caro %>%
  filter(row_number() == 1 | row_number() %% 9 == 0)

nrow(caro)
nrow(caro_3)
nrow(caro_6)
nrow(caro_9)

lead(caro$DatetimeUTC, 1)

calc_step_and_speed <- function(df) {
  
  df <- mutate(df, timelag = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, unit = "secs")))
  df <- mutate(df, steplength = ((lead(E, 1) - E)^2 + (lead(N, 1) - N)^2)^(1/2))
  df <- mutate(df, speed = steplength / timelag)
  
  return(df)
}

caro <- calc_step_and_speed(caro)
caro

caro_3 <- calc_step_and_speed(caro_3)
caro_3

caro_6 <- calc_step_and_speed(caro_6)
caro_6

caro_9 <- calc_step_and_speed(caro_9)
caro_9

caro_combined <- bind_rows("1 minute" = caro, "3 minute" = caro_3, "6 minute" = caro_6, "9 minute" = caro_9, .id = "groups")

caro_combined <- drop_na(caro_combined, speed)

caro_combined %>%
  ggplot(mapping = aes(x = DatetimeUTC, y = speed, colour = groups)) +
  geom_line()


# Task 5: Deriving movement parameters II: Rolling window functions

library(zoo)

example <- rnorm(10)
example

rollmean(example, k = 3, fill = NA, align = "left")
rollmean(example, k = 4, fill = NA, align = "left")

caro$rollmean_3 <- rollmean(caro$speed, k = 3, fill = NA, align = "left")
caro %>% 
  drop_na(rollmean_3) %>%
  ggplot(mapping = aes(x = DatetimeUTC, y = rollmean_3)) +
  geom_line()

caro$rollmean_4 <- rollmean(caro$speed, k = 4, fill = NA, align = "left")
caro %>% 
  drop_na(rollmean_4) %>%
  ggplot(mapping = aes(x = DatetimeUTC, y = rollmean_4)) +
  geom_line()

caro$rollmean_8 <- rollmean(caro$speed, k = 8, fill = NA, align = "left")
caro %>% 
  drop_na(rollmean_8) %>%
  ggplot(mapping = aes(x = DatetimeUTC, y = rollmean_8)) +
  geom_line()
