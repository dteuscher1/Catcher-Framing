library(tidyverse)
library(baseballr)

data_2021 <- read_csv("statcast_2021.csv")
caught_pitches <- data_2021 %>% filter(description %in% c("ball", "called_strike")) %>% filter(fielder_2 != 1000000000)


ids <- unique(caught_pitches$fielder_2)
catchers <- data.frame()
for (i in 1:length(ids)){
    id <- ids[i]
    data <- playername_lookup(id)
    catchers <- catchers %>% bind_rows(data)
    Sys.sleep(1)
}

write_csv(catchers, "catcher_ids.csv")
