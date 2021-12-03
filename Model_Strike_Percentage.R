library(tidyverse)
data_2021 <- read_csv("statcast_2021.csv")
umpires <- read_csv("plate_umpires_2021.csv")
catchers <- read_csv("catcher_ids.csv")
caught_pitches <- data_2021 %>% 
    filter(description %in% c("ball", "called_strike")) %>% 
    filter(fielder_2 != 1000000000) %>%
    inner_join(umpires, by = 'game_pk') %>%
    inner_join(catchers, by = c("fielder_2" = 'key_mlbam'))
    
plate_width <- 17 + 2 * (9/pi)
strike_zone <- ggplot(NULL, aes(x = plate_x, y = plate_z)) + 
    geom_rect(xmin = -(plate_width/2)/12,
              xmax = (plate_width/2)/12,
              ymin = 1.5,
              ymax = 3.5, color = "black", alpha = 0) + 
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

strike_zone %+% sample_n(caught_pitches, 7000) + 
    aes(color = type) + 
    geom_point(alpha = .5) + 
    scale_color_manual("Pitch Result", 
                       values = c("darkgoldenrod", "royalblue4"), 
                       labels = c("Ball", "Strike"))

ggsave("pitches.jpg")
zones <- caught_pitches %>%
    group_by(zone) %>%
    summarize(
        N = n(), 
        right_edge = min(2, max(plate_x)), 
        left_edge = max(-2, min(plate_x)),
        top_edge = min(5, quantile(plate_z, 0.95, na.rm = TRUE)), 
        bottom_edge = max(0, quantile(plate_z, 0.05, na.rm = TRUE)),
        strike_pct = sum(type == "S") / n(),
        plate_x = mean(plate_x), 
        plate_z = mean(plate_z))

library(ggrepel)
library(plotly)

p <- strike_zone %+% zones + 
    geom_rect(aes(xmax = right_edge, xmin = left_edge,
                  ymax = top_edge, ymin = bottom_edge,
                  fill = strike_pct, alpha = strike_pct), 
              color = "lightgray") +
    geom_text_repel(size = 3, aes(label = round(strike_pct, 2),
                                  color = strike_pct < 0.5)) + 
    scale_fill_gradient(low = "gray70", high = "navyblue") + 
    scale_color_manual(values = c("white", "black")) +
    guides(color = FALSE, alpha = FALSE)
p

library(mgcv)
model_data <- sample_n(caught_pitches, 100000)
model <- gam(type == 'S' ~ s(plate_x, plate_z) + stand + p_throws, family = binomial, data = model_data)
model2 <- gam(type == 'S' ~ s(plate_x, plate_z) + release_speed, family = binomial, data = model_data)
summary(model)
anova(model2, model)
AIC(model)
AIC(model2)
#preds <- predict(model, newdata = sample_n(caught_pitches, 100000), type = "response")
library(broom)
library(modelr)
hats <- model %>%
    augment(newdata = caught_pitches, type.predict = "response")

grid <- caught_pitches %>%
    data_grid(plate_x = seq(-2, 2, length.out = 100),
              plate_z = seq(0, 5, length.out = 100),
              p_throws, stand)
grid_hat <- model %>%
    augment(newdata = grid, type.predict = "response")
head(grid_hat)
## ----gam_k_zone, warning=FALSE, fig.cap="Estimated strike probability for taken pitches using a generalized additive model. "----

strike_zone %+% sample_n(hats, 50000) +
    geom_point(aes(color = .fitted), alpha = 0.1) + 
    scale_color_gradient(low = "gray", high = "red")

strike_zone %+% grid_hat +
    geom_tile(aes(fill= .fitted), alpha = 0.7) + 
    scale_fill_gradient(low = "white", high = "navyblue")

strike_zone %+% grid_hat +
    geom_tile(aes(fill = .se.fit), alpha = 0.7) + 
    scale_fill_gradient(low = "gray70", high = "navyblue")

library(pitchRx)

library(lme4)

mixed_1 <- glmer(type == "S" ~ .fitted + (1|fielder_2) + (1|pitcher) + (1|stand) + (1|inning_topbot),
               data = sample_n(hats, 100000), family = binomial, nAGQ = 0)

summary(mixed_1)
View(hats[1:6, ])
library(modelr)

catchers <- read.csv("catcher_ids.csv") %>%
    mutate(key_mlbam = factor(key_mlbam))
random.effects(mixed_1) %>% data.frame() %>%
    inner_join(catchers, by = c("grp" = 'key_mlbam')) %>% arrange(desc(condval)) %>%
    head(10)

random.effects(mixed_1) %>% data.frame() %>%
    inner_join(catchers, by = c("grp" = 'key_mlbam')) %>% arrange(condval) %>%
    head(10)
r_eff
?glmer()
