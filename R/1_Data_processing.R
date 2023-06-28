library(tidyverse)
library(dplyr)        
library(lubridate)
library(ggpattern)
library(MMWRweek)
library(png)

`%nin%` <- negate(`%in%`)

setwd("~/LAC")

theme_fig1 <- function () { 
  theme_bw() %+replace% 
    theme(plot.title = element_text(size=16, hjust = 0.5, margin = margin(t = 15, r = 0, b = 15, l = 0), face = "bold",lineheight=1.3),
          plot.subtitle = element_text(size=12, hjust = 0.5, margin = margin(t = 0, r = 0, b = 15, l = 0)),
          plot.caption = element_text(size=12, hjust = 1, margin = margin(t = 15, r = 0, b = 0, l = 0)),
          axis.text.y = element_text(size=12),
          axis.text.x = element_text(size=7, angle = 90),
          axis.title.y = element_text(size=12, angle = 90, margin = margin(t = 0, r = 15, b = 0, l = 25)),
          axis.title.x = element_text(size=12, margin = margin(t = 15, r = 0, b = 0, l = 0)),
          panel.border = element_blank(),
          legend.position = "right",
          panel.ontop = FALSE,
          panel.grid.major = element_line(colour = "grey90"),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          legend.title=element_text(size=12), 
          legend.text=element_text(size=12)
    )
}

theme_fig_3 <- function () { 
  theme_bw() %+replace% 
    theme(plot.title = element_text(size=24, hjust = 0.5, margin = margin(t = 15, r = 0, b = 15, l = 0), face = "bold",lineheight=1.3),
          plot.subtitle = element_text(size=18, hjust = 0.5, margin = margin(t = 0, r = 0, b = 15, l = 0)),
          plot.caption = element_text(size=18, hjust = 1, margin = margin(t = 15, r = 0, b = 0, l = 0)),
          axis.text.y = element_text(size=18),
          axis.text.x = element_text(size=18, angle = 90),
          axis.title.y = element_text(size=18, angle = 90, margin = margin(t = 0, r = 15, b = 0, l = 25)),
          axis.title.x = element_text(size=18, margin = margin(t = 15, r = 0, b = 0, l = 0)),
          panel.border = element_blank(),
          legend.position = "right",
          panel.ontop = FALSE,
          panel.grid.major = element_line(colour = "grey90"),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          legend.title=element_text(size=18), 
          legend.text=element_text(size=18)
    )
}

# 2 Read
# `weekly_cities_communities.csv` is the non-suppressed version of data
# which was used for the analysis
weekly_cities_communities <- read.csv("Data_master_weekly.csv", header=TRUE)
weekly_cities_communities$cum_pos_tests <- ave(weekly_cities_communities$new_pos_t, weekly_cities_communities$geo_merge, FUN=cumsum) 

# 3 Suppressing to cell size 1-11 in compliance with data release regulation
# `LAC_weekly_cities_communities_suppressed.csv` is the suppressed data released
weekly_cities_communities_suppressed <-weekly_cities_communities
weekly_cities_communities_suppressed$total_tests[weekly_cities_communities$total_tests<12 & weekly_cities_communities$total_tests>0] <- "1-11"
weekly_cities_communities_suppressed$new_pos_t[weekly_cities_communities$new_pos_t<12 & weekly_cities_communities$new_pos_t>0] <- "1-11"
weekly_cities_communities_suppressed$cum_pos_tests[weekly_cities_communities$cum_pos_tests<12 & weekly_cities_communities$cum_pos_tests>0] <- "1-11"
write.csv(weekly_cities_communities_suppressed, "LAC_weekly_cities_communities_suppressed.csv")

# 4 Creating indicators and lags
weekly_t <- weekly_cities_communities %>%
  mutate(test_rate = total_tests/population*100,
         pct_cum_inc = cum_pos_tests/population*100,
         pct_pos = new_pos_t/total_tests*100,
         tminus1 = time - 1,
         tminus2 = time - 2,
         tminus3 = time - 3,
         tminus4 = time - 4) 

weekly_tminus1 <- weekly_t %>%
  left_join(
    select(weekly_t, 
           geo_merge,
           tminus1 = time, 
           pct_pos_tminus1 = pct_pos,
           total_tests_tminus1 = total_tests), 
    by = c("geo_merge","tminus1"))

weekly_tminus2 <- weekly_tminus1 %>%
  left_join(
    select(weekly_t, 
           geo_merge,
           tminus2 = time, 
           pct_cum_doses_tminus2 = pct_cum_doses,
           pct_pos_tminus2 = pct_pos,
           pct_cum_inc_tminus2 = pct_cum_inc,
           test_rate_tminus2 = test_rate,
           total_tests_tminus2 = total_tests), 
    by = c("geo_merge","tminus2"))

weekly_tminus3 <- weekly_tminus2 %>%
  left_join(
    select(weekly_tminus2, 
           geo_merge,
           tminus3 = time, 
           pct_pos_tminus3 = pct_pos), 
    by = c("geo_merge","tminus3")) 

weekly_tminus4 <- weekly_tminus3 %>%
  left_join(
    select(weekly_tminus3,
           geo_merge,
           tminus4 = time, 
           pct_cum_inc_tminus4 = pct_cum_inc,
           pct_pos_tminus4 = pct_pos), 
    by = c("geo_merge","tminus4")) 

Data_master_weekly <- weekly_tminus4 %>%
  mutate(avg_pct_pos_past_2_4wk = (pct_pos_tminus2+pct_pos_tminus3+pct_pos_tminus4)/3,
         pct_diff1_pos = (pct_pos-pct_pos_tminus1)/pct_pos_tminus1 * 100,
         pct_diff1_test = (total_tests-total_tests_tminus1)/total_tests_tminus1 * 100)

# 5 Exclude communities at the lowest 5% percentile of population size 
pop <- Data_master_weekly %>%
  select(geo_merge, population) %>%
  distinct()
pop_above_5th <- (Data_master_weekly$geo_merge)[quantile(pop$population,0.05,na.rm=TRUE)<Data_master_weekly$population]

data_pop_above_5th <- Data_master_weekly %>% 
  filter(geo_merge %in% pop_above_5th)

Data_master_weekly_2 <- data_pop_above_5th %>%
  filter(time>=77)

Data_master_weekly_3 <- Data_master_weekly_2 %>%
  filter(total_tests>=50)
