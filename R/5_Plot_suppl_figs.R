# Fig.S1
LA_bin_tests <- Data_master_weekly %>%
  group_by(epi_week) %>%
  filter(epi_week != max(Data_master_weekly$epi_week)) %>%
  summarise(Total = sum(total_tests),
            Positive = sum(new_pos_t)) %>%
  mutate(Negative = Total - Positive) %>%
  pivot_longer(c(Negative, Positive), values_to = "Freq", names_to = "Results")

## Re-level the result var
LA_bin_tests$Results <- relevel(as.factor(LA_bin_tests$Results), ref = "Positive")
LA_bin_tests$epi_week <- as.Date(LA_bin_tests$epi_week)

## Tidy labels for dates
date_df <- LA_bin_tests %>%
  mutate(year = format(epi_week, "%Y"),
         month = format(epi_week, "%m"),
         day = format(epi_week, "%d")) %>%
  group_by(year, month) %>%
  mutate(first_week = ifelse(day == min(day), 1,0)) %>%
  filter(first_week == 1) %>%
  select(epi_week) %>%
  unique()

breaks <- as.character(date_df$epi_week)

labels <- unique(format(LA_bin_tests$epi_week, "%Y-%m"))

FigS1 <- ggplot() +
  geom_rect(data=NULL,aes(xmin="2021-06-13",xmax="2021-12-26",ymin=-Inf,ymax=Inf), fill="lightblue", alpha=0.4)+
  geom_rect(data=NULL,aes(xmin="2021-12-26",xmax=Inf,ymin=-Inf,ymax=Inf), fill="lightgreen", alpha=0.4)+
  geom_bar(aes(y=Freq, x=as.character(epi_week), group = Results, fill = Results), data = LA_bin_tests, position = "stack", stat = "identity") +
  theme_fig_3() +
  scale_x_discrete(breaks = breaks, labels = labels) +
  labs(title = "Weekly COVID-19 tests in the Los Angeles (L.A.) County",
       y="Weekly number of tests",
       x= "Year-Month") +
  theme(axis.text.x = element_text(size=18, angle = 90),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18))

ggsave("R/Figures/FigS1.png", plot = FigS1, width=18, height=8, dpi=300)

# Fig.S6
latest <- Data_master_weekly_3 %>%
  filter(time == max(Data_master_weekly_3$time)) 

# Subset areas with highest cumulative incidence (as of July 23, 2022)
highest_ci_areas <- latest %>%
  filter(pct_cum_inc > quantile(latest$pct_cum_inc, 0.9)) %>%
  pull(geo_merge)

highest_ci_areas_pos <- Data_master_weekly_3 %>%
  filter(geo_merge %in% highest_ci_areas)

highest_ci_areas_pos$geo_merge <- gsub(".*-", "", highest_ci_areas_pos$geo_merge)

# x-axis = %Y-%m
# Each tick represent the first date of the month
first_weeks <- highest_ci_areas_pos %>%
  mutate(year = format(date(epi_week), "%Y"),
         month = format(date(epi_week), "%m"),
         day = format(date(epi_week), "%d")) %>%
  group_by(year, month) %>%
  mutate(first_week = ifelse(day == min(day), 1,0)) %>%
  filter(first_week == 1) %>%
  select(epi_week) %>%
  unique()

breaks <- unique(as.character(first_weeks$epi_week))

labels <- unique(format(date(first_weeks$epi_week), "%Y-%m"))

LA_weekly_pos <- Data_master_weekly_3 %>%
  group_by(epi_week) %>%
  filter(as.character(epi_week) %nin% "2022-07-24",
         time >= 77) %>%
  summarise(Total = sum(total_tests),
            Positive = sum(new_pos_t)) %>%
  mutate(positivity = Positive / Total * 100) 

FigS6 <- ggplot(aes(y=pct_pos, x=as.character(epi_week), group=1), data = highest_ci_areas_pos) +
  geom_rect(data=NULL,aes(xmin="2021-06-13",xmax="2021-12-26",ymin=-Inf,ymax=Inf), fill="lightblue", alpha=0.4)+
  geom_rect(data=NULL,aes(xmin="2021-12-26",xmax=Inf,ymin=-Inf,ymax=Inf), fill="lightgreen", alpha=0.4)+
  geom_line() +
  geom_line(data = LA_weekly_pos, aes(y=positivity, x=as.character(epi_week), group=1), lty=2) +
  theme_fig() +
  facet_wrap(~ geo_merge, nrow = 5) +
  scale_x_discrete(breaks = breaks, labels = labels) +
  labs(title = "Weekly COVID-19 positivity in coummunities with highest cumulative percent positive (as of July 23, 2022)",
       subtitle = "Dotted line represents L.A. County overall",
       y="Weekly positivity (%)",
       x= "Month") +
  theme(axis.text.x = element_text(size=12, angle = 90))

ggsave("R/Figures/FigS6.png", FigS6, width=21, height = 12, dpi = 300)

# Fig.S7
# Plot % change in tests vs % pos
# Only weeks with 0% to 50% increase in tests and -100% to 300% change in positivity were shown.
FigS7_for_plot <- highest_ci_areas_pos %>% 
  filter(pct_diff1_test<50 & pct_diff1_test>0,
         epi_week >= "2021-12-26")

FigS7 <- ggplot(data = FigS7_for_plot, 
                aes(y=pct_diff1_pos, x=pct_diff1_test, group = geo_merge, col = geo_merge)) +
  geom_point() +
  theme_fig_3() +
  facet_wrap(~ geo_merge, nrow = 5) +
  labs(y="Change in positivity (%)", x="% Change in tests") +
  theme(legend.position="none",
        axis.text.x = element_text(size=12, angle = 90),
        strip.text.x = element_text(size = 12)) +
  geom_hline(yintercept=0, color = "black") +
  lims(y=c(-100,300))

ggsave("R/Figures/FigS7.png", plot = FigS7, width=21, height = 14, dpi = 300)

