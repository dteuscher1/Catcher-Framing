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
    scale_x_continuous("Horizontal pitch location (ft.)",
                       limits = c(-2, 2)) +
    scale_y_continuous("Vertical pitch location (ft.)",
                       limits = c(0, 5)) +
    theme_light() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), 
          axis.line = element_line(color = 'black'))

strike_zone %+% sample_n(caught_pitches, 10000) + 
    aes(color = type) + 
    geom_point(alpha = .3) + 
    scale_color_manual(values = c("gray", "navyblue"))

library(mgcv)
model <- gam(type == 'S' ~ s(plate_x, plate_z) + release_speed, family = binomial, data = sample_n(caught_pitches, 100000))
summary(model)

#preds <- predict(model, newdata = sample_n(caught_pitches, 100000), type = "response")
library(broom)
hats <- model %>%
    augment(type.predict = "response")

## ----gam_k_zone, warning=FALSE, fig.cap="Estimated strike probability for taken pitches using a generalized additive model. "----
strike_zone %+% sample_n(hats, 50000) +
    geom_point(aes(color = .fitted), alpha = 0.1) + 
    scale_color_gradient(low = "gray70", high = "navyblue")

strike_zone %+% sample_n(hats, 50000) +
    geom_point(aes(color = .se.fit), alpha = 0.1) + 
    scale_color_gradient(low = "gray70", high = "navyblue")

head(caught_pitches)
