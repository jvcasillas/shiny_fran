library(tidyverse)
library(lme4)

source("calc_fran.R")

all_athletes <- read_csv("./athletes.csv") %>% 
  filter(., gender %in% c("Male", "Female"), 
            age <= 90) %>% 
  mutate(., athlete_id = as.factor(athlete_id), 
            gender_dev = if_else(gender == "Male", -0.5, 0.5))

glimpse(all_athletes)


# Fran stuff
fran_df <- all_athletes %>% 
  select(., fran, gender_dev, age, candj, snatch, pullups) %>% 
  na.omit(.) %>% 
  mutate(., age_c = age - mean(age), 
            candj_c = as.numeric(candj) - mean(candj), 
            snatch_c = snatch - mean(snatch), 
            pullups_c = pullups - mean(pullups))

glimpse(fran_df)

# Take only athletes with times under 1 hour
fran_trim <- fran_df %>% filter(., fran != 0 & fran < 1000)
range(fran_trim$fran)

fran_trim %>% 
  ggplot(., aes(x = fran)) + 
    geom_histogram() 


fran_mod_full <- glm(fran ~ gender_dev * age_c, #+ candj_c + pullups_c, 
                     data = fran_trim, 
                     family = poisson(link = "log"))

summary(fran_mod_full)

fran_pred_df <- expand.grid(
    gender_dev = c(-0.5, 0.5), 
    age_c = seq(from = min(fran_trim$age_c), 
                to = max(fran_trim$age_c), 
                length.out = 100))
fran_preds_df <- predict(object = fran_mod_full, 
                         newdata = fran_pred_df, 
                         se.fit = TRUE)

fran_preds <- bind_cols(fran_pred_df, 
                        fit = exp(fran_preds_df$fit), 
                        se = exp(fran_preds_df$se.fit))

fran_preds %>% 
  ggplot(., aes(x = age_c, y = fit)) + 
    geom_jitter(data = fran_trim, 
                aes(x = age_c, y = fran, color = factor(gender_dev)), 
                alpha = 0.1) + 
    stat_summary(aes(color = factor(gender_dev)), fun.y = mean, geom = 'line', 
                 size = 1.25) + 
    stat_summary(aes(group = factor(gender_dev)), fun.y = mean, geom = 'line', 
                 lty = 3, color = 'white') + 
    stat_summary(fun.y = mean, geom = 'line', size = 1.25) +
    stat_summary(fun.y = mean, geom = 'line', lty = 3, color = 'white') + 
    scale_color_brewer(palette = 'Set1') 




