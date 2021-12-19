# Data of most recent change: 19.12.21
# Author of script: David Teuscher
# This script looks up the name for each catcher in 2021 according to their MLBAM id
######################################

# Load packages
library(tidyverse) # Data manipulation and joining
library(baseballr) # Functions to get the player information based on ID

# Load in 2021 Statcast data
data_2021 <- read_csv("statcast_2021.csv")

# Filter to include only pitches the batter didn't swing at and where the
# catcher is correctly recorded
caught_pitches <- data_2021 %>% 
    filter(description %in% c("ball", "called_strike")) %>% 
    filter(fielder_2 != 1000000000)

# Get the ID for all of the catchers
ids <- unique(caught_pitches$fielder_2)

# Create an empty data frame to include the catcher information
catchers <- data.frame()

# For each ID, look up the player and get his information and add it to the
# data frame. A 1 second pause is added between each iteration.
for (i in 1:length(ids)){
    id <- ids[i]
    data <- playername_lookup(id)
    catchers <- catchers %>% bind_rows(data)
    Sys.sleep(1)
}

# Write the data to a .csv file, if desired
#write_csv(catchers, "catcher_ids.csv")
