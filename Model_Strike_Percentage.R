library(tidyverse)
data_2021 <- read_csv("statcast_2021.csv")

caught_pitches <- data_2021 %>% filter(description %in% c("ball", "called_strike")) %>% filter(fielder_2 != 1000000000)

plate_width <- 17 + 2 * (9/pi)
strike_zone <- ggplot(NULL, aes(x = plate_x, y = plate_z)) + 
    geom_rect(xmin = -(plate_width/2)/12,
              xmax = (plate_width/2)/12,
              ymin = 1.5,
              ymax = 3.6, color = "navyblue", alpha = 0) + 
    coord_equal() + 
    scale_x_continuous("Horizontal location (ft.)",
                       limits = c(-2, 2)) +
    scale_y_continuous("Vertical location (ft.)",
                       limits = c(0, 5)) +
    theme_bw()

strike_zone %+% sample_n(caught_pitches, 10000) + 
    aes(color = type) + 
    geom_point(alpha = .3) + 
    scale_color_manual(values = c("gray", "navyblue"))
